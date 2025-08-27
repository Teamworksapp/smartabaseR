internal_env <- NULL
.sb_cache_env <- new.env(parent = emptyenv())

#' .onLoad
#'
#' @noRd
#' @keywords internal
#' @returns NULL
.onLoad <- function(libname, pkgname) {
  internal_env <<- new.env(parent = emptyenv())
  .sb_cache_env <<- new.env(parent = emptyenv())
}

