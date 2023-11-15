
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
  arg$smartabase_url <- .build_export_url(arg)
  if (!arg$type %in% c("group", "user")) {
    id_data <- .get_user_id_for_export_body(arg)
  } else {
    id_data <- NULL
  }
  body <- .build_export_body(arg, id_data)
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


