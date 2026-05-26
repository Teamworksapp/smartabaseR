#' .extract_content
#'
#' Safely extracts event json from http response
#'
#' Uses tidyjson package which creates a tibble that stores Smartabase event
#' data in a single `..JSON` column
#'
#' @param response http response
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @returns A tibble_json, event/profile data stored in json column
.extract_content <- function(response, arg) {
  data <- tryCatch(
    {
      httr2::resp_body_string(response$response) %>%
        tidyjson::gather_object("export_object") %>%
        .try_tbl_json(.)
    },
    error = function(e) {
      clear_progress_id()
      .generate_no_data_msg(arg)
      return(tibble::tibble())
    }
  )

  if (rlang::is_empty(data) || nrow(data) == 0) {
    clear_progress_id()
    .generate_no_data_msg(arg)
    return(tibble::tibble())
  }
  data
}


#' .try_tbl_json
#'
#' Safely constructs a tbl_json object for further downstream manipulation by
#' other tidyjson functions
#'
#' @param data Data returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @returns A [tidyjson::tbl_json()]
.try_tbl_json <- function(data) {
  tryCatch(
    {
      data %>% tidyjson::as.tbl_json()
    },
    error = function(e) {
      return(tibble::tibble())
    }
  )
}

#' .extract_cursor
#'
#' Extracts the pagination cursor from a raw API response.
#' Returns NULL when there are no further pages.
#'
#' The cursor field location and key name differs by endpoint:
#' - `eventsearch` / `filteredeventsearch`: top-level `nextCursor`
#' - `synchronise`: nested inside `pagination$cursor`
#'
#' @param response Named list returned by [.make_request()]
#' @param endpoint Character; the AMS endpoint name (e.g. `"eventsearch"`)
#' @noRd
#' @keywords internal
#' @returns A non-empty character cursor string, or NULL
.extract_cursor <- function(response, endpoint) {
  # Use check_type = FALSE because some AMS endpoints return JSON without an
  # explicit application/json Content-Type header.
  body <- httr2::resp_body_json(
    response$response,
    simplifyVector = FALSE,
    check_type     = FALSE
  )

  cursor <- if (endpoint == "synchronise") {
    body[["pagination"]][["cursor"]]
  } else {
    # eventsearch, filteredeventsearch
    body[["nextCursor"]]
  }

  # Treat NULL or empty string as "no more pages"
  if (is.null(cursor) || identical(cursor, "")) NULL else cursor
}


#' .extract_new_sync_time
#'
#'
#' @noRd
#' @keywords internal
#' @returns A character vector containing sync time
.extract_new_sync_time <- function(json, arg) {
  arg$new_sync_time <- json %>%
    dplyr::filter(.data$export_object == "lastSynchronisationTimeOnServer") %>%
    dplyr::pull(.data$`..JSON`) %>%
    purrr::pluck(1)

  if (length(arg$new_sync_time) == 0 || is.null(arg$new_sync_time)) {
    clear_progress_id()
    cli::cli_abort(
      "Expected {.field new_sync_time} but got {arg$new_sync_time}.",
      call = arg$current_env
    )
  }
  arg
}
