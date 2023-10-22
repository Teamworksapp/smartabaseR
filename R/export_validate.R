
#' @title
#' Generates a date range relative to a supplied end date.
#'
#' @description
#' This function generates a date range based on a specified duration relative
#' to a supplied end date, where the default end date is today's date. It
#' returns a vector of length two containing the start date and end date in
#' "dd/mm/YYYY" strings.
#'
#' For more details see the help vignette:
#' \code{vignette("exporting-data")}
#'
#' @param duration_value Number of days, weeks, months or years from end date.
#' @param duration_unit Either "days", "weeks", "months" or "years". Defaults
#' to "days".
#' @param end_date Must be a string in "dd/mm/YYYY" format. Defaults to today's
#' date.
#'
#' @return A vector containing a start date and end date as strings in
#' "dd/mm/YYYY" format.
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate date range for the last 7 days
#' sb_date_range(duration_value = "7", duration_unit = "days")
#'
#' # Generate date range for one year ending on "01/06/2023"
#' sb_date_range("1", "years", end_date = "01/06/2023")
#' }
sb_date_range <- function(
    duration_value,
    duration_unit = c("days", "weeks", "months", "years"),
    end_date = format(lubridate::today(), "%d/%m/%Y")
) {
  duration_unit <- rlang::arg_match(duration_unit)
  duration_unit <- dplyr::if_else(
    duration_unit == "months",
    "month",
    duration_unit
  )
  date_check <- lubridate::is.Date(end_date)
  if (isTRUE(date_check)) {
    cli::cli_abort("{.arg end_date} must be character type, not a date type")
  }

  end_date <- lubridate::dmy(end_date)
  if (is.na(end_date)) {
    cli::cli_abort("{.arg end_date} must be in \"dd/mm/YYYY\" format")
  }
  period <- parse(text = glue::glue("lubridate::{duration_unit}"))
  period <- eval(period)(duration_value)
  start_date <- format(end_date - period, "%d/%m/%Y")
  end_date <- format(end_date, "%d/%m/%Y")
  c(start_date = start_date, end_date = end_date)
}


#' .validate_filter_user_key
#'
#' @noRd
#'
#' @keywords internal
#'
#' @return An error when the table_field inputs do not exist as columns in the
#' input data frame
.validate_filter_user_key <- function(arg) {
  filter <- arg$filter
  option <- arg$option

  if (!is.null(option$include_user_data)) {
    if (isFALSE(option$include_user_data)) {
      if (!is.null(filter$user_key)) {
        if (is.null(filter$user_key) || filter$user_key != "user_id") {
          cli::cli_abort(
            call = arg$current_env,
            c(
              "!" = "`user_key` must equal {.field \"user_id\"} when \\
              `include_user_data = {.field FALSE}`",
              "x" = "You supplied `user_key = {.field \"{filter$user_key}\"}`"
            ))
        }
      }
    }
  }

  if (!is.null(filter$user_key)) {
    if (filter$user_key == "current_group") {
      if (!is.null(filter$user_value)) {
        cli::cli_progress_done(result = "clear", .envir = arg$current_env)
        cli::cli_alert_warning(
          "{.arg user_value} will have no effect when
          {.code user_key = 'current_group'}."
        )
      }
    }
  }

  if (is.null(filter$user_key)) {
    if (!is.null(filter$user_value)) {
      cli::cli_progress_done(result = "clear", .envir = arg$current_env)
      cli::cli_alert_warning(
        "'user_value' will have no effect when 'user_key' is NULL."
      )
    }
  }
}



.validate_date_time_range <- function(arg) {
  .validate_date_range(arg$date_range, arg$current_env)
  .validate_time_range(arg$time_range, arg$current_env)

  date_time_range <- paste(arg$date_range, arg$time_range) %>%
    purrr::map(~ lubridate::dmy_hm(.x)) %>%
    purrr::set_names(c("start_date", "end_date"))

  start_date <- date_time_range["start_date"][[1]]
  end_date <-  date_time_range["end_date"][[1]]
  order_test <- start_date > end_date

  if (isTRUE(order_test)) {
    cli::cli_progress_done(result = "clear", .envir = arg$current_env)
    cli::cli_abort(
      "{.code start_date} must occur on or before {.code end_date}",
      call = arg$current_env
    )
  }
}


.validate_date_range <- function(date_range, env) {
  if (length(date_range) != 2) {
    cli::cli_abort(
      "{.arg date_range} must be a vector/list of length = 2;
      i.e. start_date and end_date",
      call = env
    )
  }
  start_date <- date_range[[1]]
  end_date <- date_range[[2]]

  .validate_export_date_character(start_date, "start_date", env)
  .validate_export_date_length(start_date, "start_date", env)
  .validate_export_date_character(end_date, "end_date", env)
  .validate_export_date_length(end_date, "end_date", env)

  order_test <- lubridate::dmy(start_date) <= lubridate::dmy(end_date)
  if (isFALSE(order_test)) {
    cli::cli_abort(
      "{.code start_date} must occur on or before {.code end_date}",
      call = env
    )
  }
}


.validate_time_range <- function(time_range, env) {
  if (length(time_range) != 2) {
    cli::cli_abort(
      "{.arg time_range} must be a vector/list of length = 2;
      i.e. start_time and end_time",
      call = env
    )
  }
  start_time <- time_range[[1]]
  end_time <- time_range[[2]]

  .validate_export_time_colon(start_time, "start_time", env)
  .validate_export_time_ampm(start_time, "start_time", env)
  .validate_export_time_hour(start_time, "start_time", env)
  .validate_export_time_length(start_time, "start_time", env)
  .validate_export_time_leading_zero(start_time, "start_time", env)

  .validate_export_time_colon(end_time, "end_time", env)
  .validate_export_time_ampm(end_time, "end_time", env)
  .validate_export_time_hour(end_time, "end_time", env)
  .validate_export_time_length(end_time, "end_time", env)
  .validate_export_time_leading_zero(end_time, "end_time", env)

}


#' .validate_export_date_character
#'
#' Checks individual `date_range` values are class character
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_export_date_character <- function(date, date_var_name, env) {
  if(inherits(date, "Date")) {
    cli::cli_abort(
      c("!" = "{.arg {date_var_name}} must be type 'character'.",
        "!" = "You supplied {.arg {date_var_name}} as class {.field Date}."),
      call = env
    )
  }
  if (typeof(date) != "character") {
    cli::cli_abort(
      c("!" = "{.arg {date_var_name}} must be type 'character'.",
        "!" = "You supplied class: {.field {class(date)[[1]]}}."),
      call = env
    )
  }
}


#' .validate_export_time_colon
#'
#' Checks individual `time_range` values contain a colon
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_export_time_colon <- function(time, time_var_name, env) {
  validate_colon <- stringr::str_detect(time, ":")
  if (!validate_colon) {
    cli::cli_abort(
      c("!" = "{.arg {time_var_name}} must contain a colon.",
        "!" = "You supplied {.field {time}}." ),
      call = env
    )
  }
}


#' .validate_export_time_ampm
#'
#' Checks individual `time_range` values contain either 'am' or 'pm'
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_export_time_ampm <- function(time, time_var_name, env) {
  pattern <- ".*\\s(?:am|pm)$"
  validate_ampm <- stringr::str_detect(time, pattern)
  if (isFALSE(validate_ampm)) {
    cli::cli_abort(
      c("!" = "{.arg {time_var_name}} must contain {.field am} or \\
        {.field pm}.",
        "!" = "You supplied {.field {time}}."),
      call = env
    )
  }
}

#' .validate_export_time_hour
#'
#' Checks individual `time_range` values contain valid hours
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_export_time_hour <- function(time, time_var_name, env) {
  validate_hour_max <- as.numeric(stringr::str_extract(time, ".*(?=:)")) > 12
  validate_hour_min <- as.numeric(stringr::str_extract(time, ".*(?=:)")) < 0

  if (any(validate_hour_max) | any(validate_hour_min)) {
    cli::cli_abort(
      c("!" = "{.arg {time_var_name}} must be in 12-hour format \\
        (hours must be > 0 and <= 12).",
        "!" = "You supplied {.field {time}}."),
      call = env
    )
  }
}


#' .validate_export_time_length
#'
#' Checks individual length of `time_range` values are no less
#' than 7 characters and no more than 8 characters
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_export_time_length <- function(time, time_var_name, env) {
  validate_length <- nchar(time) < 7 | nchar(time) > 8
  if (isTRUE(validate_length)) {
    cli::cli_abort(
      c("!" = "{.arg {time_var_name}} must be less than 7 characters and \\
        no more than 8 characters.",
        "!" = "You supplied {.field {time}}."),
      call = env
    )
  }
}


#' .validate_export_date_length
#'
#' Checks individual length of `date_range` values are 10 characters
#'
#' @noRd
#' @keywords internal
#' @return error message
.validate_export_date_length <- function(date, date_var_name, env) {
  validate_length <- nchar(date) == 10
  if (isFALSE(validate_length)) {
    cli::cli_abort(
      c("!" = "{.arg {time_var_name}} must be 10 characters.",
        "!" = "You supplied {.field {date}}."),
      call = env
    )
  }
}

.validate_export_time_leading_zero <- function(time, time_var_name, env) {
  validate_zero <- stringr::str_detect(time, "^0")
  if (isTRUE(validate_zero)) {
    cli::cli_abort(
      c("!" = "{.arg time_var_name} cannot start with a zero",
        "!" = "You supplied {.field {time}}."),
      call = env
    )
  }
}


#' .convert_argument_to_null
#'
#' do.call an internal function, but insert relevant arg inherited from
#' parent function.
#'
#' @noRd
#'
#' @keywords internal
#'
#' @return Data to be uploaded to Smartabase - 'JSON'
.convert_argument_to_null <- function(arg) {
  filter_arg <- which(
    stringr::str_detect(names(arg), "filter") & arg == ""
  )

  if (length(filter_arg) > 0) {
    arg[filter_arg[[1]]] <- list(NULL)
  }

  arg
}



.build_export_url <- function(arg) {
  if (arg$type == "event") {
    if (!is.null(arg$filter$data_key)) {
      selected_endpoint <- "get_filtered_event"

    } else {
      selected_endpoint <- "get_event"
    }
  } else if (arg$type == "synchronise") {
    selected_endpoint <- "synchronise_event"

  } else if (arg$type == "profile") {
    selected_endpoint <- "get_profile"

  } else if (arg$type == "user") {
    if (is.null(arg$filter$user_key)) {
      selected_endpoint <- "get_user"

    } else {
      if (arg$filter$user_key == "current_group") {
        selected_endpoint <- "get_current"

      } else if (arg$filter$user_key == "group") {
        selected_endpoint <- "get_group"

      } else {
        selected_endpoint <- "get_user"
      }
    }
  } else if (arg$type == "group") {
    selected_endpoint <- "get_group_names"
  } else if (arg$type == "delete") {
    selected_endpoint <- "delete_event"
  }
  .build_url(
    url = arg$url,
    endpoints = arg$endpoints,
    endpoint = selected_endpoint
  )
}


.build_id_url <- function(arg, endpoints) {
  if (is.null(arg$filter_user_key)) {
    selected_endpoint <- "get_user"
  } else {
    if (arg$filter_user_key == "current_group") {
      selected_endpoint <- "get_current"
    } else if (arg$filter_user_key == "group") {
      selected_endpoint <- "get_group"
    } else {
      selected_endpoint <- "get_user"
    }
  }
  arg$smartabase_url <- .build_url(
    url = arg$url,
    endpoints = endpoints,
    endpoint = selected_endpoint
  )
  arg
}


#' .check_export_class
#'
#' Validates that options for export functions have class "sb_export_filter"
#' and/or "sb_export_option"
#'
#' @return error message
#' @noRd
#' @keywords internal
.check_export_class <- function(filter, option, env) {
  fun <- sys.call(1)[[1]]
  if (!is.null(filter)) {
    if (!inherits(filter, "sb_export_filter")) {
      fun <- glue::glue("{fun}_filter")
      cli::cli_abort(
        "{.arg filter} must be created by {.fun {fun}}.",
        call = env
      )
    }
  }
  if (!is.null(option)) {
    if (!inherits(option, "sb_export_option")) {
      fun <- glue::glue("{fun}_option")
      cli::cli_abort(
        "{.arg option} must be created by {.fun {fun}}.",
        call = env
      )
    }
  }
}


