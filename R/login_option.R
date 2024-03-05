#' sb_login_option
#'
#' Exports data from a Smartabase event form
#'
#' @param interactive_mode If TRUE, all messages are printed to the console.
#' If FALSE, they are suppressed. The idea is that `interactive_mode` should be
#' set to FALSE in automated environments to ensure logs aren't clogged up with
#' progress messages.
#'
#' @returns list of named export options
#' @export
sb_login_option <- function(interactive_mode = TRUE) {
  structure(
    class = "sb_login_option",
    list(interactive_mode = interactive_mode)
  )
}
