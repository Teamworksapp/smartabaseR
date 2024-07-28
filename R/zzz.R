internal_env <- NULL
cache_env <- new.env()

#' cache_functions
#'
#' @noRd
#' @keywords internal
#' @returns NULL
cache_functions <- function() {
  cache_env$sb_get_user <- memoise::memoise(internal_env$original_sb_get_user, omit_args = c("option"))
  cache_env$sb_login <- memoise::memoise(internal_env$original_sb_login, omit_args = c("option"))
  cache_env$use_cache <- TRUE
}

#' uncache_functions
#'
#' @noRd
#' @keywords internal
#' @returns NULL
uncache_functions <- function() {
  cache_env$use_cache <- FALSE
}

#' sb_get_user_wrapper
#'
#' @noRd
#' @keywords internal
#' @returns result of sb_get_user
sb_get_user_wrapper <- function(...) {
  if (cache_env$use_cache) {
    cache_env$sb_get_user(...)
  } else {
    internal_env$original_sb_get_user(...)
  }
}

#' sb_login_wrapper
#'
#' @noRd
#' @keywords internal
#' @returns result of sb_login
sb_login_wrapper <- function(...) {
  if (cache_env$use_cache) {
    cache_env$sb_login(...)
  } else {
    internal_env$original_sb_login(...)
  }
}

#' .onLoad
#'
#' @noRd
#' @keywords internal
#' @returns NULL
.onLoad <- function(libname, pkgname) {
  internal_env <<- new.env()

  # Save the original functions in internal_env
  internal_env$original_sb_get_user <- get("sb_get_user", envir = rlang::ns_env("smartabaseR"))
  internal_env$original_sb_login <- get("sb_login", envir = rlang::ns_env("smartabaseR"))

  # Set the default value for the memoization option
  options(smartabaseR.cache_functions = TRUE)

  # Apply memoization by default
  cache_functions()

  # Assign wrappers to the namespace
  assign("sb_get_user", sb_get_user_wrapper, envir = rlang::ns_env("smartabaseR"))
  assign("sb_login", sb_login_wrapper, envir = rlang::ns_env("smartabaseR"))
}

#' toggle_memoization
#'
#' @export
#' @keywords internal
#' @returns NULL
sb_toggle_cache <- function(enable = TRUE) {
  if (enable) {
    cache_functions()
  } else {
    uncache_functions()
  }
}
