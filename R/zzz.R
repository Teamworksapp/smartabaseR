
.onLoad <- function(libname, pkgname) {
  .get_cached_login <<- memoise::memoise(
    sb_login,
    omit_args = c("interactive_mode", "env", "cache")
  )
  .get_cached_user <<- memoise::memoise(
    sb_get_user,
    omit_args = c("interactive_mode", "env", "cache")
  )
  .get_cached_endpoint <<- memoise::memoise(
    .get_endpoint,
    omit_args = c("interactive_mode", "env", "cache")
  )
}
