

.build_import_element_response <- function(
    response,
    df,
    import_action,
    index,
    arg
) {
  prog_vals <- .calculate_import_progress_vals(df, index, arg)
  content <- .extract_content(response, arg)
  # TODO for profile
  .generate_import_result_msg(content, import_action, prog_vals, arg)

  if (arg$type == "profile") {
    result <- content %>% purrr::pluck("..JSON", "state")
  } else {
    result <- content %>% purrr::pluck("..JSON", "result", "state")
  }

  if (result == "UNEXPECTED_ERROR") {
    response$import_time <- NA
  }

  df <- .build_import_element_response_df(df, content, arg) %>%
    dplyr::mutate(
      import_action = import_action,
      import_result = result,
      api_call_index = index,
      import_time = response$import_time
    )
}


.build_import_partial_error_response_df <- function(df, content) {
  ids <- content %>% purrr::pluck("ids", 1)
  content <- content %>%
    purrr::pluck(
      "..JSON", "eventImportResultForForm", 1, "eventImportResults", "error"
    )
  # content <- content %>% purrr::pluck("error") %>% purrr::flatten(.)
  error_data <- seq_len(length(content)) %>%
    purrr::map_df(
      ~content[[.x]] %>%
        purrr::pluck("pairs") %>%
        purrr::map_df(~ .x) %>%
        dplyr::mutate(event_order = as.character(.x)) %>%
        tidyr::pivot_wider(
          names_from = "key",
          values_from = "value",
          id_cols = event_order
        )
    ) %>%
    dplyr::select(-dplyr::any_of(c(
      "First Name", "Last Name", "event_order"
    ))) %>%
    dplyr::rename(
      error = .data$Error,
      user_id = .data$organisationId,
      start_date = .data$`Start Date`,
      start_time = .data$`Start Time`,
      end_date = .data$`Finish Date`,
      end_time = .data$`Finish Time`
    )

  missing_names <- names(df)[!names(df) %in% names(error_data)]
  missing_names <- missing_names[!missing_names %in% c("user_id", "about")]
  if (length(missing_names > 0)) {
    df_new <- df %>% dplyr::select(-dplyr::any_of(c(missing_names)))
  }
  error_data <- error_data %>%
    dplyr::full_join(
      .,
      df_new,
      by = intersect(names(df_new), names(error_data))
    )

  empty_error_flag <- error_data %>%
    dplyr::filter(!is.na(.data$error))

  if (nrow(empty_error_flag) > 0) {
    empty_error_flag <- any(empty_error_flag$error == "")
  } else {
    empty_error_flag <- FALSE
  }

  if (!is.null(ids) && !empty_error_flag) {
    non_error_data <- error_data %>%
      dplyr::filter(is.na(.data$error)) %>%
      # dplyr::mutate(
      #   unix_time = paste(
      #     format(lubridate::dmy(start_date), "%Y-%m-%d"),
      #     format(strptime(start_time, "%I:%M %p"), "%H:%M:%S")
      #   ),
      #   unix_time = as.numeric(as.POSIXct(unix_time))
      # ) %>%
      dplyr::arrange(.data$user_id, .data$start_date) %>%
      # dplyr::select(-unix_time) %>%
      dplyr::mutate(event_id = ids)

    error_data <- error_data %>%
      dplyr::filter(!is.na(.data$error)) %>%
      dplyr::mutate(event_id = NA)

    error_data <- dplyr::full_join(
      error_data,
      non_error_data,
      by = intersect(names(non_error_data), names(error_data))
    ) %>%
      dplyr::full_join(
        ., df, by = intersect(names(.), names(df))
      )
  }
  error_data
}

.build_import_unexpected_error_response_df <- function(df, content) {
  result <- content %>% purrr::pluck("..JSON", "result")
  state <- result$state
  message <- result$message
  df %>%
    dplyr::mutate(
      result = state,
      error = message
    )
}


.build_import_element_response_df <- function(df, content, arg) {
  if (identical(content, "")) {
    # TODO Manually build 500 response
  }

  if (arg$type == "profile") {
    result <- content %>% purrr::pluck("..JSON", "state")
  } else if (arg$type == "event") {
    result <- content %>% purrr::pluck("..JSON", "result", "state")
  }

  if (result == "UNEXPECTED_ERROR") {
    df <- .build_import_unexpected_error_response_df(df, content) %>%
      dplyr::mutate(
        n_records_success = 0,
        n_records_attempted = 0
      )
    cli::cli_progress_done(result = "clear", .envir = arg$current_env)
    return(df)
  }

  if (arg$type == "profile") {
    content <- purrr::pluck(content, "..JSON")
  } else if (arg$type == "event")  {
    content <- purrr::pluck(
      .x = content,
      "..JSON",
      "eventImportResultForForm",
      1,
      "eventImportResults"
    )
  }

  n_records_success <- stringr::str_extract(
    content$message,
    "([0-9]+)(?=\\s*out of)"
  ) %>%
    as.numeric(.)

  n_records_attempted <- stringr::str_extract(
    content$message,
    "(?<=out of\\s{0,15})([0-9]+)"
  ) %>%
    as.numeric(.)

  import_response <- df %>%
    dplyr::mutate(
      form = arg$form,
      n_records_success = n_records_success,
      n_records_attempted = n_records_attempted,
      entered_by_user_id = arg$entered_by_user_id
    )

  missing_field <- .extract_missing_field(content)
  if (!is.null(missing_field)) {
    import_response <- import_response %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(missing_field), ~"FIELD DOES NOT EXIST"
      ))
  }
  import_response
}


.fill_missing_field <- function(df, arg) {
  missing_field <- df %>%
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      ~sum(. == "FIELD DOES NOT EXIST", na.rm = TRUE)
    )) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::filter(.data$value > 0) %>%
    dplyr::pull(.data$name)

  if (length(missing_field) > 0) {
    df <- df %>% tidyr::fill(!!!missing_field, .direction = "downup")
    .generate_missing_field_warning(missing_field, arg)
  }
  df
}


.count_total_records_imported <- function(df) {
  df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$import_result) %>%
    dplyr::distinct(.data$n_records_success, .data$n_records_attempted) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(
        .data$import_result, ~dplyr::case_when(
          stringr::str_detect(., "ERROR") ~ NA_character_,
          TRUE ~ .
        )
      )
    ) %>%
    tidyr::fill(.data$import_result, .direction = "downup") %>%
    dplyr::group_by(.data$import_result) %>%
    dplyr::summarise(
      dplyr::across(
        .data$n_records_success:.data$n_records_attempted,
        ~sum(., na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()
}


.fill_missing_error_msg <- function(df, arg) {
  if ("error" %in% names(df)) {
    if (any(df$error == "", na.rm = TRUE)) {
      .generate_missing_event_id_warning(arg)
    }
    df <- df %>%
      dplyr::group_by(.data$api_call_index) %>%
      dplyr::mutate(
        dplyr::across(
          .data$error,
          ~dplyr::if_else(
            . == "",
            paste("Please check import status in", arg$form),
            .
          )
        )
      ) %>%
      tidyr::fill(.data$error, .direction = "downup") %>%
      dplyr::ungroup()
  }
  df
}


.build_import_response_df <- function(df, arg) {
  import_result <- .count_total_records_imported(df)
  df <- .fill_missing_error_msg(df, arg) %>%
    .fill_missing_field(., arg) %>%
    dplyr::select(-dplyr::any_of(c("n_records_success", "n_records_attempted")))

  import_response <- list(
    "import_result" = import_result[["import_result"]],
    "n_records_success" = import_result[["n_records_success"]],
    "n_records_attempted" = import_result[["n_records_attempted"]],
    "api_call_total" = arg$total_length_body
  )
  new_sb_tibble(import_response, df, arg)
}


