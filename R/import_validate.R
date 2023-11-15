#' .validate_table_field
#'
#' Returns error if table_field input in push_smartabase() / .df_to_json()
#' do not exist as columns in the input data frame.
#'
#' @param df Data to be uploaded to Smartabase
#' @param table_field A vector of column names that are going to be uploaded
#' into a table
#' @param interactive_mode If TRUE, all messages are printed to the console.
#' If FALSE, they are suppressed. The idea is that `interactive_mode` should be
#' set to FALSE in automated environments to ensure logs aren't clogged up with
#' progress messages.
#'
#' @noRd
#' @keywords internal
#' @return An error when the table_fields inputs do not exist as columns in the
#' input data frame
.validate_table_field <- function(df, table_field, env) {
  if (!is.null(table_field)) {
    if (!all(table_field %in% names(df))) {
      missing_field <- table_field[!table_field %in% names(df)]
      clear_progress_id()
      cli::cli_abort(
        c("!" = "All elements of {.arg table_field} must match column names of
          {.arg df}.",
          "i" = "There is no column in {.arg df} called \\
          {.field {table_field[[1]]}}."),
        call = env
      )
    }
  }
}


#' .validate_id_col
#'
#' @noRd
#'
#' @keywords internal
#'
#' @return error if validate_date or validate_time are triggered
.validate_id_col <- function(df, arg) {
  if (!any(arg$option$id_col %in% names(df))) {
    clear_progress_id()
    msg <- c(
      "!" = "{.arg id_col} must exist as a column in the data frame.",
      "!" = "There is no column called {.field {arg$option$id_col}}."
    )
    if (arg$option$id_col == "user_id") {
      id_col_name <- c("about", "username", "email")
      if (any(id_col_name %in% names(df))) {
        id_col_match <- names(df)[names(df) %in% id_col_name][[1]]
        msg <- c(
          msg,
          "i" = "Did you mean to set {.code id_col = \"{id_col_match}\"}?"
        )
      } else {
        msg <- c(
          msg,
          "i" = "Did you forget to supply a value to {.field id_col}?"
        )
      }
    }
    clear_progress_id()
    cli::cli_abort(msg, call = arg$current_env)
  }
}


#' .build_import_url
#' @noRd
#'
#' @keywords internal
#'
#' @return Data to be uploaded to Smartabase - 'JSON'
.build_import_url <- function(arg) {
  if (arg$type == "profile") {
    selected_endpoint <- "insert_profile"
    arg$response_msg <- NULL
  } else {
    selected_endpoint <- "insert_event"
    arg$response_msg <- list(
      "eventImportResultForForm",
      "eventImportResults"
    )
  }

  table_field_exists <- exists("arg$option$table_field")
  empty_table_field <- identical(arg$option$table_field, "")
  if (table_field_exists && empty_table_field) {
    arg$option$table_field <- NULL
  }

  .build_url(
    url = arg$url,
    endpoints = arg$endpoints,
    endpoint = selected_endpoint
  )
}


#' .validate_duplicate_event_id
#'
#' Create the text that will appear in the confirmation message when
#' update_event = TRUE
#'
#' @noRd
#'
#' @keywords internal
#'
#' @return text to appear in confirm message
.validate_duplicate_event_id <- function(arg) {
  df <- arg$df
  df <- df %>%
    dplyr::group_by(.data$start_date, .data$user_id) %>%
    dplyr::mutate(row_num = 1:dplyr::n()) %>%
    dplyr::ungroup()

  duplicate_event_criteria <- all(
    arg$update_event,
    is.null(arg$option$table_field),
    "event_id" %in% names(df)
  )
  if (duplicate_event_criteria) {
    if ("event_id" %in% names(df)) {
      duplicate_event_id <- df %>%
        dplyr::group_by(.data$event_id) %>%
        dplyr::distinct(.data$row_num) %>%
        dplyr::summarise(unique_row_num = dplyr::n(), .groups = "drop") %>%
        dplyr::summarise(value = ifelse(
          any(.data$unique_row_num > 1), TRUE, FALSE
        )) %>%
        dplyr::pull(.data$value)

      if (duplicate_event_id) {
        clear_progress_id()
        cli::cli_abort(
          message = c(
            "!" = paste(
              "There cannot be multiple rows with the same event ID while",
              "{.field table_field} = NULL."
            ),
            "i" = paste(
              "Did you forget to supply a value to the",
              "{.field table_field} argument?"
            )
          ),
          call = arg$env
        )
      }
    }
  }
  arg
}


#' .detect_duplicate_date_user_id
#'
#' Create the text that will appear in the confirmation message when
#' update_event = TRUE
#'
#' @noRd
#'
#' @keywords internal
#'
#' @return text to appear in confirm message
.detect_duplicate_date_user_id <- function(df, arg) {
  if (is.null(arg$option$table_field) && arg$type != "profile") {
    duplicate_flag <- df %>%
      dplyr::group_by(.data$start_date, .data$user_id) %>%
      dplyr::summarise(duplicate_date = dplyr::n(), .groups = "drop") %>%
      dplyr::ungroup() %>%
      dplyr::summarise(
        max_row = max(.data$duplicate_date), .groups = "drop"
      ) %>%
      dplyr::summarise(value = ifelse(.data$max_row > 1, TRUE, FALSE)) %>%
      dplyr::pull(.data$value)
  } else {
    duplicate_flag <- FALSE
  }
  duplicate_flag
}


.check_import_class <- function(option, env) {
  fun <- sys.call(1)[[1]]
  if (!is.null(option)) {
    if (!inherits(option, "sb_import_option")) {
      fun <- glue::glue("{fun}_option")
      clear_progress_id()
      cli::cli_abort(
        "{.arg option} must be created by {.fun {fun}}.",
        call = env
      )
    }
  }
}

.validate_upsert_df <- function(df, env) {
  if (!"event_id" %in% names(df)) {
    clear_progress_id()
    cli::cli_abort(
      c("!" = "No {.field event_id} column exists in the data frame.",
        "x" = "You cannot update events without {.field event_id}.",
        "i" = "Did you mean to use {.fun sb_insert_df}?"),
      call = env
    )
  }
}

.validate_update_df <- function(df, env) {
  if ("event_id" %in% names(df)) {
    if (any(is.na(df$event_id))) {
      nrow_na <- df %>%
        dplyr::filter(is.na(.data$event_id)) %>%
        nrow()

      clear_progress_id()
      cli::cli_abort(
        c("!" = "{nrow_na} row{?s} with missing event IDs {?was/were} \\
          detected.",
          "x" = "You cannot update events without {.field event_id}.",
          "i" = "Did you mean to use {.fun sb_upsert_df}?"),
        call = env
      )
    }
  } else {
    clear_progress_id()
    cli::cli_abort(
      c("!" = "No {.field event_id} column exists in the data frame.",
        "x" = "You cannot update events without {.field event_id}.",
        "i" = "Did you mean to use {.fun sb_insert_df}?"),
      call = env
    )
  }

  if (!"event_id" %in% names(df)) {
    df <- df %>% dplyr::select(-.data$event_id)
    clear_progress_id()
    cli::cli_alert_warning(c(
     "!" = "No event ID column was detected.",
     "i" = "All events will be inserted as new events."
    ))
  }
  df
}

.remove_event_id <- function(df) {
  if ("event_id" %in% names(df)) {
    df <- df %>% dplyr::select(-.data$event_id)
    clear_progress_id()
    cli::cli_alert_warning("An event ID column was detected and was removed.")
  }
  df
}

.validate_import_df_class <- function(df, env) {
  if (isFALSE(is.data.frame(df))) {
    clear_progress_id()
    cli::cli_abort(
      message = c(
        "!" = "{.arg df} is not a data frame.",
        "!" = "You supplied {.code class: {.field {class(df)[[1]]}}}."
      ),
      call = env
    )
  }
}


.validate_import_time_leading_zero <- function(df, time_var_name, env) {
  df <- df %>%
    dplyr::mutate(
      leading_zero_test = stringr::str_detect(!!dplyr::sym(time_var_name), "^0")
    ) %>%
    dplyr::filter(.data$leading_zero_test)

  if (nrow(df) > 0) {
    error <- df %>%
      dplyr::slice(1)

    row_num <- error %>% dplyr::pull(.data$row_num)
    time <- error %>% dplyr::pull(!!dplyr::sym(time_var_name))

    clear_progress_id()
    cli::cli_abort(c(
      "!" = "Incorrect {.field {time_var_name}} value supplied at row: \\
      {.field {row_num}}.",
      "!" = "You supplied {.field {time}}.",
      "i" = "There should be no leading zero."
    ),
    call = env)
  }
}



.validate_import_time_colon <- function(df, time_var_name, env) {
  df <- df %>%
    dplyr::mutate(
      colon_test = !stringr::str_detect(!!dplyr::sym(time_var_name), ":")
    ) %>%
    dplyr::filter(.data$colon_test)

  if (nrow(df) > 0) {
    error <- df %>%
      dplyr::slice(1)

    row_num <- error %>% dplyr::pull(.data$row_num)
    time <- error %>% dplyr::pull(!!dplyr::sym(time_var_name))

    clear_progress_id()
    cli::cli_abort(c(
      "!" = "Incorrect {.field {time_var_name}} value supplied at row: \\
      {.field {row_num}}.",
      "!" = "You supplied {.field {time}}.",
      "i" = "There should be a colon after the hour value."
    ),
    call = env)
  }
}


.validate_import_date_character <- function(df, date_var_name, env) {
  if(inherits(df[[date_var_name]], "Date")) {
    clear_progress_id()
    cli::cli_abort(
      c("!" = "{.field {date_var_name}} column must be type 'character'.",
        "!" = "Your supplied {.field {date_var_name}} column is class \\
        {.field Date}."),
      call = env
    )
  }
  if (typeof(df[[date_var_name]]) != "character") {
    clear_progress_id()
    cli::cli_abort(
      c("!" = "{.field {date_var_name}} column must be type 'character'.",
        "!" = "You supplied type: {.field {class(df[[date_var_name]])}}."),
      call = env
    )
  }
}


#' .validate_import_time_ampm
#'
#' Checks that time ends in am or pm
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_import_time_ampm <- function(df, time_var_name, env) {
  df <- df %>%
    dplyr::mutate(
      ampm_test = !stringr::str_detect(
        !!dplyr::sym(time_var_name),
        "(?i).*\\s(?:am|pm)$"
      )
    ) %>%
    dplyr::filter(.data$ampm_test)

  if (nrow(df) > 0) {
    error <- df %>%
      dplyr::slice(1)

    row_num <- error %>% dplyr::pull(.data$row_num)
    time <- error %>% dplyr::pull(!!dplyr::sym(time_var_name))

    clear_progress_id()
    cli::cli_abort(c(
      "!" = "Incorrect {.field {time_var_name}} value supplied at row: \\
      {.field {row_num}}.",
      "!" = "You supplied {.field {time}}.",
      "i" = "{.arg {time_var_name}} must contain {.field am} or \\
        {.field pm}."
    ),
    call = env)
  }
}



#' .validate_import_time_length
#'
#' Checks time values contain valid length
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_import_time_length <- function(df, time_var_name, env) {
  df <- df %>%
    dplyr::mutate(length_test = nchar(!!dplyr::sym(time_var_name))) %>%
    dplyr::filter(.data$length_test < 7 | .data$length_test > 8)

  if (nrow(df) > 0) {
    error <- df %>%
      dplyr::slice(1)

    row_num <- error %>% dplyr::pull(.data$row_num)
    date <- error %>% dplyr::pull(!!dplyr::sym(time_var_name))

    clear_progress_id()
    cli::cli_abort(
      c("!" = "Incorrect {.field {time_var_name}} value supplied at row: \\
        {.field {row_num}}.",
        "!" = "{.arg {time_var_name}} must be less than 7 characters and \\
        no more than 8 characters.",
        "!" = "You supplied {.field {date}}."),
      call = env
    )
  }
}


#' .validate_import_date_length
#'
#' Checks date values contain valid length
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_import_date_length <- function(df, date_var_name, env) {
  df <- df %>%
    dplyr::mutate(length_test = nchar(!!dplyr::sym(date_var_name))) %>%
    dplyr::filter(.data$length_test != 10)

  if (nrow(df) > 0) {
    error <- df %>%
      dplyr::slice(1)

    row_num <- error %>% dplyr::pull(.data$row_num)
    date <- error %>% dplyr::pull(!!dplyr::sym(date_var_name))

    clear_progress_id()
    cli::cli_abort(
      c("!" = "Incorrect {.field {date_var_name}} value supplied at row: \\
        {.field {row_num}}.",
        "!" = "{.arg {date_var_name}} must be 10 characters.",
        "!" = "You supplied {.field {date}}."),
      call = env
    )
  }
}



#' .validate_import_time_hour
#'
#' Checks time values contain valid hours
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_import_time_hour <- function(df, time_var_name, env) {
  df <- df %>%
    dplyr::mutate(
      validate_hour = stringr::str_extract(
        !!dplyr::sym(time_var_name), ".*(?=:)"
      ) %>% as.numeric()
    ) %>%
    dplyr::filter(.data$validate_hour < 0 | .data$validate_hour > 12)

  if (nrow(df) > 0) {
    error <- df %>%
      dplyr::slice(1)

    row_num <- error %>% dplyr::pull(.data$row_num)
    time <- error %>% dplyr::pull(!!dplyr::sym(time_var_name))
    clear_progress_id()
    cli::cli_abort(
      c("!" = "Incorrect {.field {time_var_name}} value supplied at row: \\
        {.field {row_num}}.",
        "!" = "{.arg {time_var_name}} must be in 12-hour format \\
        (hours must be > 0 and <= 12).",
        "!" = "You supplied {.field {time}}."),
      call = env
    )
  }
}

