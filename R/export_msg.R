#' .generate_export_progress_msg
#'
#' @noRd
#' @keywords internal
#' @returns A character vector containing message
.generate_export_progress_msg <- function(arg) {
  if (arg$endpoint %in% c("eventsearch", "filteredeventsearch")) {
    msg <- paste(
      "Initiating {.field {arg$form}} export between",
      "{.field {arg$start_date_clean}} - {.field {arg$end_date_clean}}..."
    )
  } else if (arg$endpoint == "profilesearch") {
    msg <- "Initiating {.field {arg$form}} export..."
  } else if (arg$endpoint == "synchronise") {
    msg <- paste(
      "Initiating sync with {.field {arg$form}} from ",
      "{.field {.convert_unix_time_to_utc(arg$last_sync_time/1000)}} onwards..."
    )
  } else if (arg$endpoint == "listgroups") {
    msg <- "Initiating group name export..."
  } else if (arg$endpoint == "usersearch") {
    msg <- "Initiating user export..."
  }
  cli::cli_progress_message(msg, .envir = arg$current_env)
}

#' .generate_no_data_msg
#'
#' @noRd
#' @keywords internal
#' @returns A character vector containing message
.generate_no_data_msg <- function(arg) {
  if (arg$endpoint == "eventsearch") {
    cli::cli_inform(
      c("i" = "No event data was found in {.field {arg$form}} between \\
        {.field {arg$start_date_clean}} - {.field {arg$end_date_clean}}."),
      "i" = "Are any user/data filters applied incorrectly?"
    )
  } else if (arg$endpoint == "profilesearch") {
    cli::cli_inform(
      c(
        "!" = "No profile data was found in {.field {arg$form}}.",
        "i" = "Are any user filters applied incorrectly?"
      )
    )
  } else if (arg$endpoint == "synchronise") {
    cli::cli_inform(
      c("!" = "No new data in {.field {arg$form}} since \\
        {.field { .convert_unix_time_to_utc(arg$last_sync_time/1000)}}.")
    )
  } else if (arg$endpoint == "usersearch") {
    cli::cli_inform(
      c(
        "!" = "No user details found.",
        "i" = "Are any user filters applied incorrectly?"
      )
    )
  } else if (arg$endpoint == "listgroups") {
    cli::cli_inform(
      c(
        "!" = "No group details found.",
        "i" = "Are any user filters applied incorrectly?"
      )
    )
  }
}

#' .generate_attachment_error_msg
#'
#' @noRd
#' @keywords internal
#' @returns A character vector containing message
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
  clear_progress_id()
  cli::cli_alert_warning(attachment_message)
}

#' .generate_attachment_success_msg
#'
#' @noRd
#' @keywords internal
#' @returns A character vector containing message
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
  clear_progress_id()
  attachment_message
}

#' .generate_attachment_message
#'
#' @noRd
#' @keywords internal
#' @returns A character vector containing message
.generate_attachment_message <- function(attachment_response, arg) {
  if (any(class(attachment_response) == "error")) {
    .generate_attachment_error_msg(attachment_response, arg)
  } else {
    .generate_attachment_success_msg(attachment_response, arg)
  }
}

#' .generate_export_success_msg
#'
#' @noRd
#' @keywords internal
#' @returns A character vector containing message
.generate_export_success_msg <- function(arg) {
  if (arg$endpoint %in% c("eventsearch", "filteredeventsearch")) {
    msg <- paste(
      "{.field {arg$form}} export successful",
      "({.field {arg$start_date_clean}} - {.field {arg$end_date_clean}})."
    )
  } else if (arg$endpoint == "profilesearch") {
    msg <- paste(
      "{.field {arg$form}} export successful."
    )
  } else if (arg$endpoint == "synchronise") {
    msg <- paste(
      "{.field {arg$form}} synchronise successful",
      "(data modified after",
      "{.field { .convert_unix_time_to_utc(arg$last_sync_time/1000)}})"
    )
  } else if (arg$endpoint %in% c("usersearch","currentgroup","groupmembers")) {
    msg <- "User details export successful."
  } else if (arg$endpoint == "listgroups") {
    msg <- "Group export successful."
  }
  clear_progress_id()
  cli::cli_alert_success(msg)
}
