#' new_sb_tibble
#'
#' @noRd
#' @keywords internal
#' @returns A tibble
new_sb_tibble <- function(response, dat, arg) {
  keep_attr <- c("names", "row.names", "class")
  attributes(dat)[!names(attributes(dat)) %in% keep_attr] <- NULL

  if (arg$option$interactive_mode) {
    class <- "sb_df"
  } else {
    class <- "sb_df_non_interactive"
  }

  sb_tibble <- tibble::new_tibble(
    dat,
    nrow = nrow(dat),
    class = class,
    request = response$request,
    http_method = response$http_method,
    http_status_code = response$http_status_code
  )

  if (!is.null(arg$form)) {
    attr(sb_tibble, "form") <- arg$form
  }
  if (arg$action == "export") {
    attr(sb_tibble, "export_time") <- response$export_time

    if (all(c("user_id", "start_date") %in% names(dat))) {
      attr(sb_tibble, "records") <- dat %>%
        dplyr::group_by(.data$user_id, .data$start_date) %>%
        dplyr::n_distinct()
    }

    if (arg$type == "synchronise" && !is.null(arg$last_sync_time)) {
      attr(sb_tibble, "new_sync_time") <- arg$new_sync_time
    }
    if (arg$type == "synchronise" && "deleted_event_id" %in% names(arg)) {
      attr(sb_tibble, "deleted_event_id") <- arg$deleted_event_id
    }
  } else if (arg$action == "import") {
    if (!is.null(response$import_time)) {
      attr(sb_tibble, "import_time") <- response$import_time
    }
    if (!is.null(response$import_result)) {
      attr(sb_tibble, "import_result") <- response$import_result
    }
    if (!is.null(response$n_records_success)) {
      attr(sb_tibble, "n_records_success") <- response$n_records_success
    }
    if (!is.null(response$n_records_attempted)) {
      attr(sb_tibble, "n_records_attempted") <- response$n_records_attempted
    }
    if (!is.null(response$api_call_total)) {
      attr(sb_tibble, "api_call_total") <- response$api_call_total
    }
  }

  tibble::validate_tibble(sb_tibble)
}




#' print.sb_df
#'
#' Hides the custom attributes that we attach the objects
# returned by the api functions. In particular, the "content" attribute contains
# the raw content returned by the API, and is very verbose to print. The request
# and status code are printed at the top, and everything else is printed as
# normal.
#'
#' @param x A smartabase attributes object
#'
#' @param ... Reserved for future use
#' @returns A cleaned message
#'
#' @export
print.sb_df <- function(x, ...) {
  if (!is.null(attr(x, "form"))) {
    cli::cli_inform(c("i" = paste(
      "Form:", attr(x, "form")
    )))
  }
  if (!is.null(attr(x, "http_status_code"))) {
    cli::cli_inform(c("i" = paste(
      "HTTP status code:", attr(x, "http_status_code")
    )))
  }
  if (!is.null(attr(x, "import_result"))) {
    cli::cli_inform(c("i" = paste(
      "Import result:", attr(x, "import_result")
    )))
  }
  if (!is.null(attr(x, "n_records_success"))) {
    cli::cli_inform(c("i" = paste(
      "Records succesfully imported:", attr(x, "n_records_success")
    )))
  }
  if (!is.null(attr(x, "n_records_attempted"))) {
    cli::cli_inform(c("i" = paste(
      "Records attempted imported:", attr(x, "n_records_attempted")
    )))
  }
  if (!is.null(attr(x, "api_call_total"))) {
    cli::cli_inform(c("i" = paste(
      "Total API calls:", attr(x, "api_call_total")
    )))
  }
  if (!is.null(attr(x, "export_time"))) {
    cli::cli_inform(c("i" = paste(
      "Export time:",
      .convert_unix_time_to_utc(attr(x, "export_time") / 1000)
    )))
  }
  if (!is.null(attr(x, "sync_time"))) {
    cli::cli_inform(c("i" = paste(
      "Last synchronised:",
      .convert_unix_time_to_utc(attr(x, "sync_time") / 1000)
    )))
  }
  if (!is.null(attr(x, "records"))) {
    cli::cli_inform(c("i" = paste(
      "Unique records:", attr(x, "records")
    )))
  }
  NextMethod()
}
