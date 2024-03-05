#' @title
#' Export event data
#'
#' @description
#' `sb_delete_event()` deletes data from a Smartabase event form using
#' the Smartabase API. It requires the user to input a valid Smartabase
#' event ID, url and credentials.
#'
#' Please see \code{vignette("deleting-data")} for more
#' details.
#'
#' @param event_id IDs of Smartabase events to be deleted
#' @param url Smartabase url e.g. "example.smartabase.com/site"
#' @param username Smartabase username
#' @param password Smartabase password
#' @param ... These dots are for future extensions and must be empty
#' @param option More options accessible via [sb_delete_event_option()] object
#'
#' @return Success/fail message
#'
#' @examples
#' \dontrun{
#' # Delete one record with event_id = 999 from example.smartabase.com/site:
#' sb_delete_event(
#'   event_id = 999,
#'   url = "example.smartabase.com/site",
#'   username = "john.smith",
#'   password = "examplePassword"
#' )
#' }
#'
#' @export
sb_delete_event <- function(
    event_id,
    url,
    username,
    password,
    ...,
    option = sb_delete_event_option()) {
  rlang::check_dots_used()
  env <- rlang::current_env()
  .check_delete_class(option, env)
  url <- .validate_url(url)

  arg <- list(
    event_id = event_id,
    url = url,
    username = username,
    password = password,
    option = option,
    type = "delete",
    action = "delete",
    current_env = env,
    ...
  )

  if (!is.null(arg$dev_mode)) {
    if (isTRUE(arg$dev_mode)) {
      return(arg)
    }
  }

  .delete_handler(arg)
}


#' .delete_handler
#'
#' Builds http requests to the Smartabase event export API
#'
#' Encapsulates generic tasks common to delete-related API call such as
#' building a valid payload/URL and throwing relevant messages
#'
#' @param arg list of arguments passed from exposed delete functions
#'
#' @return Success/fail message
#' @noRd
#' @keywords internal
.delete_handler <- function(arg) {
  login <- sb_login(
    url = arg$url,
    username = arg$username,
    password = arg$password,
    option = arg$option
  )
  arg$endpoints <- .get_endpoint(
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
  body <- list(eventId = arg$event_id)
  request <- .build_request(body, arg)
  resp <- .make_request(request, arg)
  content <- .extract_content(resp, arg)
  msg_list <- content %>% dplyr::pull(.data$..JSON)
  msg <- glue::glue("{msg_list$state}: {msg_list$message}")
  if (msg_list$state == "FAILURE") {
    clear_progress_id()
    cli::cli_alert_warning(msg)
  } else {
    clear_progress_id()
    cli::cli_alert_success(msg)
  }
}

#' .check_delete_class
#'
#' Validates that options for `sb_delete_event()` have class "sb_delete_option"
#'
#' @param option Options for `sb_delete_event()`
#' @param env Environment of parent function
#'
#' @return error message
#' @noRd
#' @keywords internal
.check_delete_class <- function(option, env) {
  fun <- sys.call(1)[[1]]
  if (!is.null(option)) {
    if (!inherits(option, "sb_delete_option")) {
      fun <- glue::glue("{fun}_option")
      clear_progress_id()
      cli::cli_abort(
        "{.arg option} must be created by {.fun {fun}}.",
        call = env
      )
    }
  }
}


#' @title Set option parameters for [sb_delete_event()]
#'
#' @param interactive_mode If TRUE, all messages are printed to the console.
#' If FALSE, they are suppressed. The idea is that `interactive_mode` should be
#' set to FALSE in automated environments to ensure logs aren't clogged up with
#' progress messages.
#'
#' @return A list of options with class = "sb_delete_option"
#' @export
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_delete_event_option()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_get_group_option(guess_col_type = FALSE, interactive_mode = FALSE)
#' }
sb_delete_event_option <- function(interactive_mode = TRUE) {
  structure(
    class = "sb_delete_option",
    list(interactive_mode = interactive_mode)
  )
}
