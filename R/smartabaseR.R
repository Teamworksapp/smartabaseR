#' \code{smartabaseR} package
#'
#' R wrapper for Smartabase API
#'
#'
#' @docType package
#' @name smartabaseR
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom utils capture.output
#' @keywords internal
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
