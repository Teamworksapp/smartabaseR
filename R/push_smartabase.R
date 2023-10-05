
#' push_smartabase
#'
#' Uploads a data frame to a Smartabase event or profile form
#'
#' The push_smartabase() function uses the Smartabase API and imports an R
#' data frame into a specific Smartabase event or profile form. Set
#' 'type' = "profile" to push to a profile form. For more details see the
#' help vignette:
#' \code{vignette("get-started", package = "smartabaseR")}
#'
#' @param df data frame: data to be uploaded to Smartabase
#' @param url string: Smartabase url e.g. 'example.smartabase.com/site'
#' @param form string: Name of Smartabase form
#' @param username string: Smartabase username -- ignore if setup with
#' save_credentials()
#' @param password string: Smartabase password -- ignore if setup with
#' save_credentials()
#' @param entered_by_user_id string: user ID of user uploading the data --
#' ignore if setup with save_credentials()
#' @param type string: either 'event' or 'profile'
#' @param get_id boolean: if TRUE, searches for user IDs of athletes listed
#' in data frame (requires a value supplied to 'match_id_to_column')
#' @param match_id_to_column string: names of columns in data frame that match
#' either 'about', 'username' or 'email'. User IDs will be returned that match
#' data frame values. Requires 'get_id' = TRUE
#' @param table_fields vector: supply a vector of column names that are going
#' to be uploaded into a table. In Smartabase, this is equivalent to ticking
#' 'Treat all records for the same user, on the same day as a single record?'
#' @param start_date string: name of start date column in data frame. If NULL,
#' and 'Date' or 'start_date' are not already columns in the data frame, creates
#' a new start_date column based on current date
#' @param end_date string: name of end date column in data frame. If NULL, and
#' 'end_date' is not already a column in the data frame, creates a new
#' 'end_date' column based on 'start_date'
#' @param start_time string: name of start time column in data frame with
#' 'h:mm AM' or 'h:mm PM' records. If NULL, and 'start_time' is not already a
#' column in the data frame, creates a new 'start_time' column based on
#' Sys.time()
#' @param end_time string: name of end time column in data frame with
#' 'h:mm AM' or 'h:mm PM' records. If NULL, and 'end_time' is not already a
#' column in the data frame, creates a new 'end_time' column with values +1 hour
#' after 'start_time'
#' @param current_date_format string: current format of date variable of
#' interest. Set this argument to convert 'start_date' or 'end_date' to
#' dd/mm/yyyy
#' @param edit_event boolean: if TRUE, will look for 'event_id' in data frame.
#' Records with matching event_id on Smartabase will be overwritten with the
#' new data
#' @param cloud_mode boolean: if TRUE, confirmation pop-up will not appear when
#' editing events (i.e. for use in non-local, cloud environments)
#'
#' @keywords internal
#' @export
push_smartabase <- function(
    df,
    form,
    ...,
    url = NULL,
    username = NULL,
    password = NULL,
    entered_by_user_id = NULL,
    type = "event",
    get_id = FALSE,
    match_id_to_column = NULL,
    table_fields = NULL,
    start_date = NULL,
    end_date   = NULL,
    current_date_format = NULL,
    start_time = NULL,
    end_time   = NULL,
    edit_event = FALSE,
    cloud_mode = FALSE,
    shiny_progress_code = NULL,
    cache = TRUE
) {
  lifecycle::deprecate_warn(
    when = "0.0.0.9000",
    what = "push_smartabase()",
    details = glue::glue(
      "push_smartabase()` was deprecated in favour of the more explicitly \\
      named `sb_insert_event()`, `sb_update_event()`, `sb_update_profile()`, \\
      or `sb_upsert_event()`"
  ))

  env <- rlang::current_env()
  rlang::check_dots_used()
  .validate_import_df_class(df, env)
  username <- .get_username(username)
  password <- .get_password(password)
  url <-.get_url(url)

  if (is.null(match_id_to_column)) {
    match_id_to_column <- "user_id"
  }

  option <- .push_smartabase_option(
    interactive_mode = !cloud_mode,
    id_col = match_id_to_column,
    table_field = table_fields,
    cache = cache
  )

  .check_import_class(option, env)

  arg <- list(
    form = form,
    url = .validate_url(url),
    username = username,
    password = password,
    option = option,
    type = type,
    update_event = edit_event,
    current_env = env,
    ...
  )

  .validate_id_col(df, arg)
  if (!is.null(arg$dev_mode)) {
    if (isTRUE(arg$dev_mode)) {
      return(arg)
    }
  }
  .import_handler(df, arg)
}



.push_smartabase_option <- function(
    interactive_mode = TRUE,
    id_col = c("user_id", "about", "username", "email"),
    table_field = NULL,
    cache = TRUE
) {
  id_col <- rlang::arg_match(id_col)
  structure(
    class = "sb_import_option",
    list(
      interactive_mode = interactive_mode,
      id_col = id_col,
      table_field = table_field,
      cache = cache
    )
  )
}


