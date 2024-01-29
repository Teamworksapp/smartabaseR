
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
#' @return tibble_json: event/profile data stored in json column
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
#' @return tidyjson::tbl_json
.try_tbl_json <- function(data) {
  tryCatch(
    {
      data %>% tidyjson::as.tbl_json()
    },
    error = function(e) {
      return(tibble::tibble())
    })
}


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
