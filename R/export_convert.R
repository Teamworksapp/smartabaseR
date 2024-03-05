
#' .convert_user_json_to_df
#'
#' Takes Smartabase user json returned by Smartabase API and wrangles into a
#' tibble
#'
#' Uses tidyjson to pluck out the user data from the json returned by
#' Smartabase and then converts this to a tibble. Various details about the
#' export (e.g. export time) are then attached to the tibble as attributes
#'
#' @param response http response
#' @param data tbl_json returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @returns A tibble containing Smartabase user data
.convert_user_json_to_df <- function(response, data, arg) {
  data <- .flatten_user_export_data(data)
  data <- .clean_user_export(data, arg$option$include_all_cols)
  new_sb_tibble(response, data, arg)
}


#' .convert_group_json_to_df
#'
#' Takes Smartabase group json returned by Smartabase API and wrangles into a
#' tibble
#'
#' Uses tidyjson to pluck out the groups data from the json returned by
#' Smartabase and then converts this to a tibble. Various details about the
#' export (e.g. export time) are then attached to the tibble as attributes
#'
#' @param response http response
#' @param data data frame of data returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @returns A tibble containing Smartabase user data
.convert_group_json_to_df <- function(response, data, arg) {
  data <- .flatten_group_export_data(data)
  if (nrow(data) == 0) {
    clear_progress_id()
    .generate_no_data_msg(arg)
    return(tibble::tibble())
  }
  new_sb_tibble(response, data, arg)
}



#' .convert_export_json_to_df
#'
#' Takes Smartabase export json returned by Smartabase API and wrangles into a
#' tibble
#'
#' Uses tidyjson to pluck out the groups data from the json returned by
#' Smartabase and then converts this to a tibble. Various details about the
#' export (e.g. export time) are then attached to the tibble as attributes
#'
#' @param response http response
#' @param data Data returned from Smartabase
#' @param id_data User data returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @returns A tibble
.convert_export_json_to_df <- function(response, data, id_data, arg) {
  if (arg$type == "synchronise") {
    arg <- .flatten_deleted_event_id(data, arg)
    arg <- .extract_new_sync_time(data, arg)
  }

  metadata <- .flatten_export_metadata(data, arg)
  options <- arg$option
  filters <- arg$filter

  if (nrow(metadata) == 0) {
    clear_progress_id()
    .generate_no_data_msg(arg)
    return(new_sb_tibble(response, tibble::tibble(), arg))
  }
  export_data <- .flatten_export_data(metadata)
  metadata <- metadata %>%
    dplyr::select(-dplyr::any_of(c(
      "document.id", "export_object", "record_number", "array.index"
    )))

  if (!arg$type %in% c("profile", "synchronise")) {
    if (arg$option$download_attachment) {
      export_data <- .process_export_attachment(
        response = response,
        nested_data = metadata,
        unnested_data = export_data,
        id_data = id_data,
        arg = arg
      )
    }
  }
  if (nrow(export_data) == 0) {
    export_data <- metadata %>% tibble::as_tibble()
  }
  # TODO: %>% .handle_null_rows(.)
  export_data <- .clean_export(data = export_data, id_data, arg)
  new_sb_tibble(response, export_data, arg)
}

