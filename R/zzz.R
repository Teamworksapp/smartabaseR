internal_env <- NULL

#' cache_function
#'
#' @noRd
#' @keywords internal
#' @returns NULL
cache_function <- function(function_name) {
  fn <- get(function_name, envir = rlang::ns_env("smartabaseR"))
  fn <- memoise::memoise(
    fn,
    omit_args = c("option")
  )
  assign(function_name, fn, envir = rlang::ns_env("smartabaseR"))
  return(invisible(TRUE))
}

#' .onLoad
#'
#' @noRd
#' @keywords internal
#' @returns NULL
.onLoad <- function(libname, pkgname) {
  internal_env <<- new.env()
  purrr::map(
    c("sb_login", "sb_get_user", ".get_endpoint"),
    ~ cache_function(.x)
  )
}
