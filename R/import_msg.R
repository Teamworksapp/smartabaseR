
.calculate_import_progress_vals <- function(df, index, arg) {
  ix <- dplyr::if_else(arg$total_length_body > 1, paste0(index, ": "), "")
  n_row <- df %>%
    dplyr::select(.data$user_id, .data$start_date) %>%
    dplyr::n_distinct(.)
  list("ix" = ix, "n_row" = n_row)
}


.generate_import_progress_msg <- function(import_action, prog_vals, arg) {
  if (import_action == "update") {
    msg <- paste(
      "{prog_vals[['ix']]}UPDATING {.val {prog_vals[['n_row']]}}",
      "existing record{?s} in {.field {arg$form}}..."
    )
  } else {
    msg <- paste(
      "{prog_vals[['ix']]}INSERTING {.val {prog_vals[['n_row']]}}",
      "new record{?s} into {.field {arg$form}}..."
    )
  }
}


.generate_update_confirmation <- function(
    df_list,
    update_nrow,
    insert_nrow,
    arg
) {
  cli::cli_h1("WARNING")
  cli::cli_inform(c(
    "!" = paste(
      "You are about to UPDATE {.val {update_nrow}} record{?s} in",
      "{.field {arg$form}}."
    ),
    "i" = "Please be aware the incoming data will overwrite the existing data."
    )
  )
  if (insert_nrow > 0) {
    cli::cli_inform(
      c("i" = "You will also INSERT {.val {insert_nrow}} new record{?s}.")
    )
  }
  df_len_val <- length(df_list)
  if (df_len_val == 1) {
    df_len_val <- "once"
  } else if (df_len_val == 2) {
    df_len_val <- "twice"
  }
  cli::cli_inform("The Smartabase API will be called {df_len_val}{?/ times}.")
  cli::cli_progress_done(result = "clear", .envir = arg$current_env)
  cli::cli_alert("Are you sure you want to continue?")
  confirm <- utils::menu(choices = c("Yes", "No"))
  if (confirm == 2) {
    cli::cli_progress_done(result = "clear", .envir = arg$env)
    cli::cli_abort("Data import was aborted.", call = arg$env)
  }
}


.count_unique_import_records <- function(df_list, import_action, table_field) {
  dfs <- df_list[which(names(df_list) == import_action)]
  if (length(dfs) == 0) {
    return(0)
  }

  if (!is.null(table_field)) {
  dfs %>%
    purrr::map(
      ~ .x %>%
        dplyr::select(.data$user_id, .data$start_date)%>%
        dplyr::n_distinct()
    ) %>%
    purrr::reduce(sum)

  } else {
    dfs %>%
      purrr::map(
        ~ .x %>%
          dplyr::select(.data$user_id, .data$start_date) %>%
          dplyr::n_distinct()
      ) %>%
      purrr::reduce(sum)
  }
}


.generate_import_confirmation <- function(df_list, arg) {
  update_nrow <- .count_unique_import_records(
    df_list = df_list,
    import_action = "update",
    table_field = arg$option$table_field
  )
  insert_nrow <- .count_unique_import_records(
    df_list = df_list,
    import_action = "insert",
    table_field = arg$option$table_field
  )
  if (update_nrow > 0) {
    .generate_update_confirmation(df_list, update_nrow, insert_nrow, arg)

  } else if (update_nrow == 0 && insert_nrow > 0) {
    df_len <- length(df_list)
    if (df_len > 0) {
      msg <- paste(
          "INSERTING {.val {insert_nrow}} record{?s} into",
          "{.field {arg$form}} over {.val {df_len}} calls to the",
          "Smartabase API..."
        )
    } else {
      msg <- paste(
        "INSERTING {.val {insert_nrow}} record{?s} into",
        "{.field {arg$form}}..."
      )
    }
    cli::cli_progress_message(msg)
  }
}


.generate_duplicate_date_user_id_msg <- function(arg) {
  if (arg$duplicate_date_user_id) {
    cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
    cli::cli_inform(message = c(
      "i" = "Duplicate {.field user_id} and {.field start_date} were detected."
    ))
    cli::cli_ul(id = "duplicate_user_date_id")
    cli::cli_li(paste(
      "These rows will be imported separately since",
      "{.field table_field} = NULL"
    ))
    cli::cli_end(id = "duplicate_user_date_id")  }
}


.generate_import_result_msg <- function(
    content,
    import_action,
    prog_vals,
    arg
) {
  if (arg$type == "event") {
    .generate_event_import_result_msg(content, import_action, prog_vals, arg)
  } else if (arg$type == "profile") {
    .generate_profile_import_result_msg(content, import_action, prog_vals, arg)
  }
}

.generate_profile_import_result_msg <- function(
    content,
    import_action,
    prog_vals,
    arg
) {
  message <- content %>% purrr::pluck("..JSON", "message")
  state <- content %>% purrr::pluck("..JSON", "state")
  cli_msg <- "{prog_vals[['ix']]}{state}: {message}"
  if (state == "UNEXPECTED ERROR") {
    cli::cli_progress_done(result = "clear", .envir = arg$current_env)
    cli::cli_alert_warning(cli_msg)
  }

  message <- message %>%
    stringr::str_remove(., "\\.") %>%
    glue::glue(" into {arg$form}.") %>%
    stringr::str_remove_all(., .missing_field_pattern())

  if (stringr::str_detect(state, "SUCCESS")) {
    cli::cli_progress_done(result = "clear", .envir = arg$current_env)
    cli::cli_alert_success(cli_msg)
  } else if (stringr::str_detect(state, "ERROR")) {
    cli::cli_progress_done(result = "clear", .envir = arg$current_env)
    cli::cli_alert_warning(cli_msg)
  }
}


.generate_event_import_result_msg <- function(
    content,
    import_action,
    prog_vals,
    arg
) {
  result <- content$..JSON$result
  state <- result$state
  cli_msg <- "{prog_vals[['ix']]}{state}: {message}"

  if (!is.null(result$state)) {
    if (result$state == "UNEXPECTED_ERROR") {
      message <- result$message
      cli::cli_progress_done(result = "clear", .envir = arg$current_env)
      return(cli::cli_alert_warning(cli_msg))
    }
  }

  cli::cli_progress_message("")
  state <- stringr::str_replace(
    result$state,
    "IMPORTED",
    paste0(toupper(import_action), "ED") %>%
      stringr::str_replace(., "EE", "E")
  )

  if (import_action == "insert") {
    in_into <- "into"
  } else if (import_action == "update") {
    in_into <- "in"
  }

  event_import_results <- purrr::pluck(
    content,
    "..JSON",
    "eventImportResultForForm",
    1,
    "eventImportResults"
  )

  message <- event_import_results[["message"]]
  message <- stringr::str_replace(
    message,
    "imported.",
    paste0(import_action, "ed") %>%
      stringr::str_replace(., "ee", "e") %>%
      paste0(" ", in_into, " ", arg$form, ".")
  ) %>%
    stringr::str_remove_all(., .missing_field_pattern())

  if (stringr::str_detect(state, "SUCCESS")) {
    cli::cli_progress_done(result = "clear", .envir = arg$current_env)
    cli::cli_alert_success(cli_msg)

  } else if (stringr::str_detect(state, "ERROR")) {
    cli::cli_progress_done(result = "clear", .envir = arg$current_env)
    cli::cli_alert_warning(cli_msg)
  }
}


.extract_missing_field <- function(content) {
  missing_field_msg <- stringr::str_extract(
    content$message, .missing_field_pattern()
  )
  if (!is.na(missing_field_msg)) {
    missing_field_msg <- missing_field_msg %>% paste0(., ".")
    missing_field <- stringr::str_extract(
      missing_field_msg, "(?<=\\[)[^\\]]+(?=\\])"
    ) %>%
      stringr::str_split(., ", ") %>%
      purrr::pluck(1)
  } else {
    missing_field <- NULL
  }
  missing_field
}


.generate_missing_field_warning <- function(missing_field, arg) {
  if (length(missing_field) > 1) {
    field_str <- "fields"
  } else {
    field_str <- "field"
  }
  cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
  cli::cli_alert_warning(
    "The following {field_str} do not exist in {.field {arg$form}} and were \\
    not imported:"
  )

  cli::cli_ul(id = "missing_field_id")
  for (i in missing_field) {
    cli::cli_warn(c("*" = "{.field {i}}"))
  }
  cli::cli_end(id = "missing_field_id")
}


.generate_missing_event_id_warning <- function(arg) {
  cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
  cli::cli_alert_warning(
    "Some records could not be imported into {.field {arg$form}}."
  )
  cli::cli_ul(id = "missing_event_id")
  cli::cli_alert_warning(
    c("*" = paste(
      "Smartabase did not return valid event IDs for all records,",
      "even though some records may have imported successfully."
    ))
  )
  cli::cli_alert_warning(
    c("*" = paste(
      "We recommend validating the import status by exporting",
      "and manually checking the {.field {arg$form}} data."
    ))
  )
  cli::cli_end(id = "missing_event_id")
}
