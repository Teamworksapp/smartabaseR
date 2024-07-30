#' @title
#' Enable caching for `sb_get_user()` and `sb_login()`
#'
#' @description
#' Enable caching for `sb_get_user()` and `sb_login()`. This function is useful
#' when you are making multiple calls to `sb_get_user()` and/or `sb_login()` with
#' the same user filters. By enabling caching, you can avoid redudantly calling
#' out to Smartabase for the same user data.
#'
#' @param timeout Measured in seconds, the time it takes until the cache
#' invalidates (within a given R session). By default, the timeout is 2 hours
#' (2 * 60 * 60 seconds).
#'
#' @returns NULL
#' @export
sb_enable_user_cache <- function(timeout = 2*60*60) {
  timeout <- cachem::cache_mem(max_age = timeout)

  cache_env$sb_get_user <- memoise::memoise(
    sb_internal_env$original_sb_get_user,
    cache = timeout,
    omit_args = c("option")
  )
  cache_env$sb_login <- memoise::memoise(
    sb_internal_env$original_sb_login,
    cache = timeout,
    omit_args = c("option")
  )
  cache_env$use_cache <- TRUE
}

#' @title
#' Disable caching for `sb_get_user()` and `sb_login()`
#'
#' @description
#' Disable caching for `sb_get_user()` and `sb_login()`. Use this function when
#' you always want the most up to date user and/or login data.
#'
#' @returns NULL
#' @export
sb_disable_user_cache <- function() {
  cache_env$use_cache <- FALSE
}

#' .sb_get_user
#'
#' @noRd
#' @keywords internal
.sb_get_user <- function(
    url,
    username,
    password,
    ...,
    filter = sb_get_user_filter(),
    option = sb_get_user_option()
) {
  if (cache_env$use_cache) {
    cache_env$sb_get_user(url, username, password, ..., filter, option)
  } else {
    sb_internal_env$original_sb_get_user(
      url, username, password, ..., filter, option
    )
  }
}

#' .sb_login
#'
#' @noRd
#' @keywords internal
.sb_login <- function(
    url,
    username,
    password,
    ...,
    option = sb_get_user_option()
) {
  if (cache_env$use_cache) {
    cache_env$sb_login(url, username, password, ..., option)
  } else {
    sb_internal_env$original_sb_login(url, username, password, ..., option)
  }
}
