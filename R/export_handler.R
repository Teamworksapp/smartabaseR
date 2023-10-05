
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
  login <- .get_cached_login(
    url = arg$url,
    username = arg$username,
    password = arg$password,
    env = arg$current_env
  )
  arg$endpoints <- .get_cached_endpoint(
    login = login,
    url = arg$url,
    username = arg$username,
    password = arg$password,
    interactive_mode = arg$interactive_mode,
    cache = arg$cache,
    env = arg$current_env,
    endpoints = NULL
  )
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
    cli::cli_progress_message(
      "Requesting {arg$type} data from Smartabase...",
      .envir = arg$current_env
    )
  }
  response <- .make_request(request, arg)
  if (isTRUE(arg$option$interactive_mode)) {
    cli::cli_progress_message(
      "Wrangling {arg$type} data...",
      .envir = arg$current_env
    )
  }
  .json_to_df_handler(response, arg, id_data)
}


