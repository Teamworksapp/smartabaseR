#' sb_login_option
#'
#' @title Set option parameters for [sb_login()]
#'
#' @param interactive_mode If TRUE, all messages are printed to the console.
#' If FALSE, they are suppressed. The idea is that `interactive_mode` should be
#' set to FALSE in automated environments to ensure logs aren't clogged up with
#' progress messages.
#' @param cache_login Logical. If `TRUE`, the result of [sb_login()] will be
#' cached in memory and reused for the duration of the current R session, up to
#' the timeout specified by `cache_login_timeout`.
#'
#' This is useful because `sb_login()` is often called automatically behind the
#' scenes by import functions such as [sb_insert_event()]. If you're calling
#' multiple import functions repeatedly in the same R session, caching avoids
#' unnecessary re-authentication, reducing latency and API load.
#' @param cache_login_timeout Numeric. Time in seconds that a cached login
#' object remains valid. Once this timeout is exceeded, a new login will be
#' requested on the next function call.
#'
#' @returns list of named export options
#' @export
sb_login_option <- function(
    interactive_mode = TRUE,
    cache_login = TRUE,
    cache_login_timeout = 1800
) {
  structure(
    class = "sb_login_option",
    list(
      interactive_mode = interactive_mode,
      cache_login = cache_login,
      cache_login_timeout = cache_login_timeout
    )
  )
}
