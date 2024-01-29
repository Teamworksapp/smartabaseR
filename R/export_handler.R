
#' .export_handler
#'
#' Builds http requests to the Smartabase event export API
#'
#' Encapsulates generic tasks common to each export-related API call such as
#' building a valid payload/URL and throwing relevant messages.
#'
#' @param arg list of arguments passed from exposed export functions
#'
#' @return tibble: Smartabase event or profile data
#' @noRd
#' @keywords internal
.export_handler <- function(arg) {
  if (is.null(arg$login)) {
    arg$login <- sb_login(
      url = arg$url,
      username = arg$username,
      password = arg$password,
      option = arg$option
    )
  }
  if (is.null(arg$endpoints)) {
    arg$endpoints <- .get_endpoint(
      login = arg$login,
      url = arg$url,
      username = arg$username,
      password = arg$password,
      interactive_mode = arg$interactive_mode,
      cache = arg$cache,
      env = arg$current_env,
      endpoints = NULL
    )
  }

  if (arg$type %in% c("group", "user")) {
    id_data <- NULL
    user_id <- NULL
  } else {
    if (isTRUE(arg$option$include_user_data)) {
      id_data <- .get_user_id_for_export_body(arg)
      user_id <- id_data %>% dplyr::pull(.data$user_id)
    } else {
      id_data <- arg$filter$user_value
      user_id <- id_data
    }
  }
  body <- .build_export_body(arg, user_id)
  arg$smartabase_url <- .build_export_url(arg)
  arg$dry_run <- FALSE
  arg$action <- "export"
  request <- .build_request(body, arg)

  if (isTRUE(arg$option$interactive_mode)) {
    export_request_progress_id <- cli::cli_progress_message(
      "Requesting {arg$type} data from Smartabase...",
      .envir = arg$current_env
    )
    set_progress_id("export_request_progress_id", export_request_progress_id)
  }
  response <- .make_request(request, arg)
  if (isTRUE(arg$option$interactive_mode)) {
    export_wrangle_progress_id <- cli::cli_progress_message(
      "Wrangling {arg$type} data...",
      .envir = arg$current_env
    )
    set_progress_id("export_wrangle_progress_id", export_wrangle_progress_id)
  }
  .json_to_df_handler(response, arg, id_data)
}


#' .json_to_df_handler
#'
#' Generic handler that passes response to the right conversion function
#' according to arg$type
#'
#' @param response http response
#' @param arg List of arguments returned from parent function
#' @param id_data User data returned from Smartabase
#' @noRd
#' @keywords internal
#' @return tibble
.json_to_df_handler <- function(response, arg, id_data = NULL) {
  data <- .extract_content(response, arg)
  if (nrow(data) == 0) {
    clear_progress_id()
    return(new_sb_tibble(response, data, arg))
  }
  if (arg$type == "user") {
    data <- .convert_user_json_to_df(response, data, arg)
  } else if (arg$type == "group") {
    data <- .convert_group_json_to_df(response, data, arg)
  } else {
    data <- .convert_export_json_to_df(response, data, id_data, arg)
  }
  if (isTRUE(arg$option$interactive_mode)) {
    clear_progress_id()
    .generate_export_success_msg(arg)
  }
  data
}
