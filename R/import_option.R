#' @title Set option parameters for [sb_insert_event()]
#'
#' @param interactive_mode If TRUE, all messages are printed to the console.
#' If FALSE, they are suppressed. The idea is that `interactive_mode` should be
#' set to FALSE to ensure logs aren't clogged up with progress messages in
#' automated environments.
#' @param id_col Name of column that `smartabaseR` should use
#' for user identifier. By default the Smartabase API requires a "user_id"
#' column, but other possible values are "about", "username" or "email".
#' @param table_field Smartabase tables are used when you want to store
#' multiple rows of data in one event. If you are importing data into a form
#' that contains table fields, you must manually supply the names of those
#' fields as a character vector to the `table_field` argument within the
#' relevant import option helper function. This is equivalent to ticking
#' 'Treat all records for the same user, on the same day as a single record?'
#' in the Smartabase web app.
#' @param cache_user If TRUE, user IDs for target athletes are cached on the
#' local cache. This saves API calls to get the user IDs before each import,
#' since user data tends to be slow-changing. This is particularly useful
#' for large AMS sites.
#' @param cache_user_timeout Defaults to 30 seconds. The time in seconds that
#' the user ID cache is valid for. Once invalidated, the user data will be
#' re-exported. Restarting the R session will also invalidate the cache.
#'
#' @returns A list of options with class = "sb_import_option"
#' @export
#'
#' @family import option helpers
#' @seealso
#' [sb_update_event_option()]
#' [sb_upsert_event_option()]
#' [sb_upsert_profile_option()]
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_insert_event_option()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_insert_event_option(
#'   id_col = "about",
#'   table_field = c("Example Table Field Name")
#' )
#' }
sb_insert_event_option <- function(
    interactive_mode = TRUE,
    id_col = c("user_id", "about", "username", "email"),
    table_field = NULL,
    cache_user = TRUE,
    cache_user_timeout = 1800
) {
  id_col <- rlang::arg_match(id_col)
  structure(
    class = "sb_import_option",
    list(
      interactive_mode = interactive_mode,
      id_col = id_col,
      table_field = table_field,
      cache_user = cache_user,
      cache_user_timeout = cache_user_timeout
    )
  )
}


#' @title Set option parameters for [sb_update_event_option()]
#'
#' @inheritParams sb_insert_event_option
#'
#' @returns A list of options with class = "sb_import_option"
#' @export
#'
#' @family import option helpers
#' @seealso
#' [sb_insert_event_option()]
#' [sb_upsert_event_option()]
#' [sb_upsert_profile_option()]
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_update_event_option()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_update_event_option(
#'   id_col = "about",
#'   table_field = c("Example Table Field Name")
#' )
#' }
sb_update_event_option <- function(
    interactive_mode = TRUE,
    id_col = c("user_id", "about", "username", "email"),
    table_field = NULL,
    cache_user = TRUE,
    cache_user_timeout = 1800
) {
  id_col <- rlang::arg_match(id_col)
  structure(
    class = "sb_import_option",
    list(
      interactive_mode = interactive_mode,
      id_col = id_col,
      table_field = table_field,
      cache_user = cache_user,
      cache_user_timeout = cache_user_timeout
    )
  )
}


#' @title Set option parameters for [sb_upsert_event_option()]
#'
#' @inheritParams sb_insert_event_option
#'
#' @returns A list of options with class = "sb_import_option"
#' @export
#'
#' @family import option helpers
#' @seealso
#' [sb_insert_event_option()]
#' [sb_update_event_option()]
#' [sb_upsert_profile_option()]
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_upsert_event_option()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_upsert_event_option(
#'   id_col = "about",
#'   table_field = c("Example Table Field Name")
#' )
#' }
sb_upsert_event_option <- function(
    interactive_mode = TRUE,
    id_col = c("user_id", "about", "username", "email"),
    table_field = NULL,
    cache_user = TRUE,
    cache_user_timeout = 1800
) {
  id_col <- rlang::arg_match(id_col)
  structure(
    class = "sb_import_option",
    list(
      interactive_mode = interactive_mode,
      id_col = id_col,
      table_field = table_field,
      cache_user = cache_user,
      cache_user_timeout = cache_user_timeout
    )
  )
}


#' @title Set option parameters for [sb_upsert_profile_option()]
#'
#' @inheritParams sb_insert_event_option
#'
#' @returns A list of options with class = "sb_import_option"
#' @export
#'
#' @family import option helpers
#' @seealso
#' [sb_insert_event_option()]
#' [sb_update_event_option()]
#' [sb_upsert_event_option()]
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_upsert_profile_option()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_upsert_profile_option(
#'   id_col = "about",
#'   table_field = c("Example Table Field Name")
#' )
#' }
sb_upsert_profile_option <- function(
    interactive_mode = TRUE,
    id_col = c("user_id", "about", "username", "email"),
    table_field = NULL,
    cache_user = TRUE,
    cache_user_timeout = 1800
) {
  id_col <- rlang::arg_match(id_col)
  structure(
    class = "sb_import_option",
    list(
      interactive_mode = interactive_mode,
      id_col = id_col,
      table_field = table_field,
      cache_user = cache_user,
      cache_user_timeout = cache_user_timeout
    )
  )
}
