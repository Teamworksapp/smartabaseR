internal_env <- NULL

cache_function <- function(function_name) {
  fn <- get(function_name, envir = rlang::ns_env("smartabaseR"))
  fn <- memoise::memoise(
    fn,
    omit_args = c("option")
  )
  assign(function_name, fn, envir = rlang::ns_env("smartabaseR"))
  return(invisible(TRUE))
}

.onLoad <- function(libname, pkgname) {
  internal_env <<- new.env()
  purrr::map(
    c("sb_login", "sb_get_user", ".get_endpoint"),
    ~ cache_function(.x)
  )

  # .get_cached_login <<- memoise::memoise(
  #   sb_login,
  #   omit_args = "option",
  #   envir = internal_env
  # )
  # .get_cached_user <<- memoise::memoise(
  #   sb_get_user,
  #   omit_args = c("interactive_mode", "env", "cache"),
  #   envir = internal_env
  # )
  # .get_cached_endpoint <<- memoise::memoise(
  #   .get_endpoint,
  #   omit_args = c("interactive_mode", "env", "cache"),
  #   envir = internal_env
  # )
}
