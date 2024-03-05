#' pull_smartabase
#'
#' Downloads data from a Smartabase event or profile form
#'
#' This function pulls data from a Smartabase event form or profile form using
#' the Smartabase API. For more details see the help vignette:
#' \code{vignette("pulling-data")}
#'
#' @param url string: Smartabase url e.g. 'example.smartabase.com/site'
#' @param form string: name of Smartabase form
#' @param username string: Smartabase username -- ignore if setup with
#' save_credentials()
#' @param password string: Smartabase password -- ignore if setup with
#' save_credentials()
#' @param type string: either 'event', 'profile' or 'synchronise'
#' @param download_attachment boolean: if TRUE, will download any files that
#' are included in the form via a file upload/multiple file upload field
#' @param start_date string: 'dd/mm/yyyy'
#' @param end_date string: 'dd/mm/yyyy'
#'
#' @returns Smartabase data
#'
#' @examples
#' \dontrun{
#' # Get one week of wellness data from example.smartabase.com/site:
#' wellness_data <- pull_smartabase(
#'   url = "example.smartabase.com/site",
#'   form = "Daily Wellness",
#'   username = "john.smith",
#'   password = "examplePassword",
#'   start_date = "15/04/2019",
#'   end_date = "22/04/2019"
#' )
#' }
#'
#' @keywords internal
#' @export
pull_smartabase <- function(
    form,
    ...,
    url = NULL,
    type = "event",
    download_attachment = FALSE,
    start_date = NULL,
    end_date = NULL,
    last = NULL,
    start_time = "12:00 am",
    end_time = "11:59 pm",
    username = NULL,
    password = NULL,
    filter_user_key = NULL,
    filter_user_value = NULL,
    filter_data_key = NULL,
    filter_data_value = NULL,
    filter_data_condition = "equal_to",
    include_missing_user = FALSE,
    guess_col_type = TRUE,
    get_uuid = FALSE,
    cloud_mode = FALSE,
    last_sync_time = NULL,
    shiny_progress_code = NULL,
    dev_mode = FALSE) {
  rlang::check_dots_used()
  env <- rlang::current_env()
  if (type == "event" && !is.null(filter_user_key)) {
    filter$data_filter <- .insert_form_data_filter(form, filter$data_filter)
  }
  username <- .get_username(username)
  password <- .get_password(password)
  url <- .get_url(url)

  filter <- .pull_smartabase_filter(
    user_key = filter_user_key,
    user_value = filter_user_value,
    data_key = filter_data_key,
    data_value = filter_data_value,
    data_condition = filter_data_condition
  )

  option <- .pull_smartabase_option(
    interactive_mode = !cloud_mode,
    download_attachment = download_attachment,
    include_missing_user = include_missing_user,
    guess_col_type = guess_col_type,
    include_uuid = get_uuid
  )

  .check_export_class(filter, option, env)
  date_range <- c(start_date, end_date)
  time_range <- c(start_time, end_time)

  arg <- list(
    form = form,
    url = url,
    username = username,
    password = password,
    date_range = date_range,
    time_range = time_range,
    start_date_clean = format(lubridate::dmy(date_range[[1]]), "%b %d %Y"),
    end_date_clean = format(lubridate::dmy(date_range[[2]]), "%b %d %Y"),
    filter = filter,
    option = option,
    type = "event",
    current_env = env,
    dev_mode = dev_mode,
    pull_smartabase = TRUE,
    ...
  )

  .validate_filter_user_key(arg)
  .validate_date_time_range(arg)
  arg$url <- .validate_url(url)

  if (!is.null(arg$dev_mode)) {
    if (isTRUE(arg$dev_mode)) {
      return(arg)
    }
  }

  if (isTRUE(arg$option$interactive_mode)) {
    pull_progress_id <- .generate_export_progress_msg(arg)
    set_progress_id("pull_progress_id", pull_progress_id)
  }
  .export_handler(arg)
}


#' .pull_smartabase_filter
#'
#' @noRd
#' @keywords internal
#' @returns A list containing filter object
.pull_smartabase_filter <- function(
    user_key = c(
      "about", "username", "email", "group", "current_group"
    ),
    user_value = NULL,
    data_key = NULL,
    data_value = NULL,
    data_condition = c(
      "equal_to", "not_equal_to", "contains", "less_than",
      "greater_than", "less_than_or_equal_to", "greater_than_or_equal_to"
    )) {
  if (!is.null(user_key)) {
    user_key <- rlang::arg_match(user_key)
  }
  if (!is.null(data_condition)) {
    data_condition <- rlang::arg_match(data_condition)
  }
  if (!is.null(data_key)) {
    data_filter <- .build_export_filter(
      data_key = data_key,
      data_value = data_value,
      data_condition = data_condition
    )
  } else {
    data_filter <- NULL
  }

  structure(
    class = "sb_export_filter",
    list(
      user_key = user_key,
      user_value = user_value,
      data_key = data_key,
      data_value = data_value
    )
  )
}

#' .pull_smartabase_option
#'
#' @noRd
#' @keywords internal
#' @returns A list containing options object
.pull_smartabase_option <- function(
    interactive_mode = TRUE,
    cache = TRUE,
    include_missing_user = FALSE,
    guess_col_type = TRUE,
    include_uuid = FALSE,
    download_attachment = FALSE) {
  structure(
    class = "sb_export_option",
    list(
      interactive_mode = interactive_mode,
      cache = cache,
      include_missing_user = include_missing_user,
      guess_col_type = guess_col_type,
      include_uuid = include_uuid,
      download_attachment = download_attachment
    )
  )
}
