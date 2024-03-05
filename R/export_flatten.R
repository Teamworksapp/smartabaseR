#' .flatten_export_metadata
#'
#' Takes the metadata columns out of the tbl_json and converts into standalone
#' columns. The user-entered data remains in the `..JSON` column. Each row
#' represents a unique event ID.
#'
#' @param data tbl_json returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @returns A tibble
.flatten_export_metadata <- function(json, arg) {
  if (arg$type == "synchronise") {
    json <- json %>%
      dplyr::filter(.data$export_object == "export") %>%
      tidyjson::enter_object("events")
  }

  if (nrow(json) > 0) {
    json <- json %>%
      tidyjson::gather_array("record_number") %>%
      tidyjson::spread_all() %>%
      .rename_export_metadata(., arg$type)
  }
  json
}


#' .flatten_user_export_data
#'
#' @noRd
#' @keywords internal
#' @returns A [tibble()]
.flatten_user_export_data <- function(data) {
  data %>%
    tidyjson::gather_array("record_number") %>%
    tidyjson::enter_object("results") %>%
    tidyjson::gather_array("results_number")
}

#' .flatten_group_user_export_data
#'
#' @noRd
#' @keywords internal
#' @returns A [tibble()]
.flatten_group_export_data <- function(data) {
  data <- data %>%
    tidyjson::gather_array("record_number") %>%
    dplyr::pull(.data$..JSON) %>%
    purrr::reduce(c)

  tibble::tibble(group = data)
}


#' .flatten_export_data
#'
#' Extracts user-entered data into a flat tibble structure
#'
#' @param nested_data Event data that has been nested for ease of use
#' @noRd
#' @keywords internal
#' @returns A tibble
.flatten_export_data <- function(nested_data) {
  nested_data %>%
    tidyjson::enter_object("rows") %>%
    tidyjson::gather_array("row_number") %>%
    tidyjson::enter_object("pairs") %>%
    tidyjson::gather_array("row_number_long") %>%
    tidyjson::spread_values(
      key = tidyjson::jstring("key"),
      value = tidyjson::jstring("value")
    ) %>%
    tibble::as_tibble(.) %>%
    dplyr::select(.data$record_number, .data$row_number,
                  dplyr::everything()) %>%
    dplyr::select(-dplyr::any_of(c(
      "document.id",
      "row_number_long",
      "events",
      "profile"
    ))) %>%
    dplyr::group_by(.data$record_number, .data$row_number) %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.data$record_number, .data$row_number))
}


#' .flatten_deleted_event_id
#'
#' Flattens the IDs of any deleted events returned from [`sb_sync_event()`]
#' since the `last_sync_time`
#'
#' @param data tbl_json returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @returns A list
.flatten_deleted_event_id <- function(json, arg) {
  deleted_events <- json %>%
    dplyr::filter(.data$export_object == "idsOfDeletedEvents")

  if (nrow(deleted_events) == 0) {
    return(arg)
  }
  deleted_events <- deleted_events %>%
    tidyjson::gather_array("record_number") %>%
    dplyr::pull(.data$`..JSON`) %>%
    purrr::reduce(c)

  if (length(deleted_events) > 0) {
    arg$deleted_event_id <- deleted_events
  }
  arg
}
