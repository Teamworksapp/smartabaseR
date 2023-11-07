#' .get_memoised_sb_login
#'
#' This function initializes a memoized version of the `sb_login()` function.
#'
#' @return memoised function
#' @noRd
#' @keywords internal
.get_memoised_sb_login <- function(
    url,
    username,
    password,
    env)  {
  if (!exists(".get_cached_login", envir = .GlobalEnv)) {
    .get_cached_login <<- memoise::memoise(
      sb_login,
      omit_args = c("interactive_mode", "env", "cache")
    )
  }
  .get_cached_login(
    url,
    username,
    password,
    env
  )
}


#' .get_memoised_endpoint
#'
#' This function initializes a memoized version of the `.get_endpoint()`
#` function.
#'
#' @return memoised function
#' @noRd
#' @keywords internal
.get_memoised_endpoint <- function(
    login,
    url,
    username,
    password,
    interactive_mode,
    cache,
    env,
    endpoints
)  {
  if (!exists(".get_cached_endpoint", envir = .GlobalEnv)) {
    .get_cached_endpoint <<- memoise::memoise(
      .get_endpoint,
      omit_args = c("interactive_mode", "env", "cache")
    )
  }
  .get_cached_endpoint(
    login,
    url,
    username,
    password,
    interactive_mode,
    cache,
    env,
    endpoints
  )
}


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
    arg$login <- .get_memoised_sb_login(
      url = arg$url,
      username = arg$username,
      password = arg$password,
      env = arg$current_env
    )
  }
  if (is.null(arg$endpoints)) {
    arg$endpoints <- .get_memoised_endpoint(
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


