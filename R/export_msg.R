
.generate_export_progress_msg <- function(arg) {
  if (arg$type == "event") {
    msg <- paste(
      "Initiating {.field {arg$form}} export between",
      "{.field {arg$start_date_clean}} - {.field {arg$end_date_clean}}..."
    )
  } else if (arg$type == "profile") {
    msg <- "Initiating {.field {arg$form}} export..."

  } else if (arg$type == "synchronise") {
    msg <- paste(
      "Initiating sync with {.field {arg$form}} from ",
      "{.field {.convert_unix_time_to_utc(arg$last_sync_time/1000)}} onwards..."
    )
  } else if (arg$type == "group") {
    msg <- "Initiating group name export..."

  } else if (arg$type == "user") {
    msg <- "Initiating user export..."
  }
  cli::cli_progress_message(msg, .envir = arg$current_env)
}

.generate_no_data_msg <- function(arg) {
  if (arg$type == "event") {
    cli::cli_inform(
      c("i" = paste(
        "No event data was found in {.field {arg$form}} between \\
        {.field {arg$start_date_clean}} - {.field {arg$end_date_clean}}."
      ),
      "i" = paste(
        "Does {.field {arg$username}} have the appropriate perissions \\
        required to view this data?"
      ),
      "i" = "Are any data filters or user filters applied incorrectly?")
    )
  } else if (arg$type == "profile") {
    cli::cli_inform(
      c("i" = paste("No profile data was found in {.field {arg$form}}."),
      "i" = paste(
        "Does {.field {arg$username}} have the appropriate perissions \\
        required to view this data?"
      ),
      "i" = "Are any user filters applied incorrectly?")
    )
  } else if (arg$type == "synchronise") {
    cli::cli_inform(
      c("i" = paste(
        "No new data in {.field {arg$form}} since \\
        {.field { .convert_unix_time_to_utc(arg$last_sync_time/1000)}}."
      ))
    )
  } else if (arg$type == "user") {
    cli::cli_inform(c("i" = paste0(
      "No user details could be retrieved from {.field {url}}",
      "\n",
      "Please check that the filter conditions are correct and that",
      "{.field {{arg$username}} is assigned the appriopriate Smartabase ",
      "permissions/roles/groups required to access this data."
    )))
  } else if (arg$type == "group") {
    cli::cli_inform(c("i" = paste0(
      "No group names could be retrieved from {.field {url}}",
      "\n",
      "Please check that {.field {arg$username}} is assigned the ",
      "appriopriate Smartabase permissions/roles/groups required to access ",
      "this data."
    )))
  }
}

#' .build_attachment_error
#'
#' @noRd
#'
#' @keywords internal
#'
#' @return data
.generate_attachment_error_msg <- function(attachment_response, arg) {
  if (!is.null(attachment_response$message)) {
    attachment_path_exists <- stringr::str_detect(
      attachment_response$message, "Path exists and overwrite is FALSE."
    )
  } else {
    attachment_path_exists <- FALSE
  }

  if (attachment_path_exists) {
    attachment_message <- paste(
      arg$form,
      "attachment download failed: files already exist in",
      getwd()
    )
  } else {
    attachment_message <- paste0(
      arg$form,
      "attachment download failed: ",
      attachment_response$message, "."
    )
  }
  cli::cli_progress_done(result = "clear", .envir = arg$new_env)
  cli::cli_progress_done(result = "clear", .envir = arg$current_env)
  cli::cli_alert_warning(attachment_message)
}

#' .build_attachment_success
#'
#' @noRd
#'
#' @keywords internal
#'
#' @return data
.generate_attachment_success_msg <- function(attachment_response, arg) {
  attachment_message <- attachment_response %>%
    purrr::map_df(~ tibble::tibble(
      status = httr2::resp_status(attachment_response),
      size = as.numeric(attachment_response$headers$`content-length`) / 1000000
    )) %>%
    dplyr::mutate_at("status", ~ dplyr::if_else(. == 200, 1, 0)) %>%
    dplyr::summarise(
      success_sum = sum(.data$status),
      n_rows = dplyr::n(), size_sum = sum(.data$size)
    )

  attachment_size <- attachment_message$size_sum
  if (attachment_size < 1) {
    attachment_size <- attachment_size * 1000
    attachment_unit <- "kb"
  } else {
    attachment_unit <- "mb"
  }

  attachment_message <- cli::cli_text(
    "{.field \u2713} {.field {arg$form}} attachment download successful: ",
    "{.field {attachment_message$success_sum}} out of ",
    "{.field {attachment_message$n_rows}} files ({round(attachment_size, 2)} ",
    "{attachment_unit} total) saved to {.field {getwd()}}."
  )
  cli::cli_progress_done(result = "clear", .envir = arg$new_env)
  cli::cli_progress_done(result = "clear", .envir = arg$current_env)
  attachment_message
}

.generate_attachment_message <- function(attachment_response, arg) {
  if (any(class(attachment_response) == "error")) {
    .generate_attachment_error_msg(attachment_response, arg)
  } else {
    .generate_attachment_success_msg(attachment_response, arg)
  }
}

.generate_export_success_msg <- function(arg) {
  if (arg$type == "event") {
    msg <- paste(
      "{.field {arg$form}} export successful",
      "({.field {arg$start_date_clean}} - {.field {arg$end_date_clean}})."
    )
  } else if (arg$type == "profile") {
    msg <- paste(
      "{.field {arg$form}} export successful."
    )
  } else if (arg$type == "synchronise") {
    msg <- paste(
      "{.field {arg$form}} synchronise successful",
      "(data modified after",
      "{.field { .convert_unix_time_to_utc(arg$last_sync_time/1000)}})"
    )
  } else if (arg$type == "user") {
    msg <- "User details export successful."
  } else if (arg$type == "group") {
    msg <- "Group export successful."
  }
  cli::cli_progress_done(result = "clear", .envir = arg$new_env)
  cli::cli_progress_done(result = "clear", .envir = arg$current_env)
  cli::cli_alert_success(msg)
}

