#' .remove_protected_column_names
#' @noRd
#' @keywords internal
#' @returns Data to be uploaded to Smartabase - 'JSON'
.remove_protected_column_names <- function(df) {
  if (any(c("first name", "last name") %in% tolower(names(df)))) {
    warning(
      paste(
        "'First Name' and 'Last Name' are protected names and will be removed",
        "from the dataset"
      )
    )

    df %>%
      dplyr::select(
        -dplyr::matches(c("first name", "last name"), ignore.case = TRUE)
      )
  } else {
    df
  }
}


#' .replace_na_with_empty_string
#' @noRd
#' @keywords internal
#' @returns Data to be uploaded to Smartabase - 'JSON'
.replace_na_with_empty_string <- function(df) {
  if ("event_id" %in% names(df)) {
    df %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_at(
        dplyr::vars(-"event_id"),
        ~ dplyr::if_else(is.na(.), paste(""), .)
      )
  } else {
    df %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(~ ifelse(is.na(.), paste(""), .))
  }
}


#' .convert_id_names_to_lower
#' @noRd
#' @keywords internal
#' @return Data to be uploaded to Smartabase - 'JSON'
.convert_id_names_to_lower <- function(df) {
  id_cols <- c("about", "user_id", "username", "email", "event_id")

  df %>% dplyr::rename_at(dplyr::vars(dplyr::matches(id_cols)), ~ tolower(.))
}


#' .detect_duplicate_user_ids
#' @noRd
#' @keywords internal
#' @returns Data to be uploaded to Smartabase - 'JSON'
.detect_duplicate_user_ids <- function(id_data, env) {
  if (any(duplicated(id_data$about))) {
    dup_names <- id_data %>%
      dplyr::count(.data$about) %>%
      dplyr::filter(.data$n > 1) %>%
      dplyr::pull(.data$about)

    dup_names <- sub(",([^,]*)$", " and\\1", paste(dup_names, collapse = ", "))

    clear_progress_id()
    cli::cli_abort(
      message = c(
        "!" = "There are multiple Smartabase accounts with the following \\
        first and last names: {paste(dup_names, collapse = ", ")}",
        "i" = "Try running `sb_get_user(...)` to find and match the correct \\
        user IDs manually."
      ),
      call = env
    )
  }
}


#' .attach_user_id_to_df
#' @noRd
#' @keywords internal
#' @returns Data to be uploaded to Smartabase - 'JSON'
.attach_user_id_to_df <- function(df, arg) {
  id_col <- arg$option$id_col
  if (id_col != "user_id") {
    # Remove user_id, if it exists, since we are about to join user_id below
    if ("user_id" %in% names(df) | "userid" %in% names(df)) {
      df <- df[!names(df) %in% c("user_id", "userid")]
    }

    user_key <- id_col
    user_value <- unique(df[[id_col]])

    id_data <- sb_get_user(
      url = arg$url,
      username = arg$username,
      password = arg$password,
      filter = sb_get_user_filter(
        user_key = user_key,
        user_value = user_value
      )
    ) %>%
      dplyr::select(.data$user_id, !!id_col) %>%
      dplyr::distinct(
        .data$user_id, !!dplyr::sym(id_col),
        .keep_all = TRUE
      )

    .detect_duplicate_user_ids(id_data, arg$current_env)
    df <- dplyr::left_join(df, id_data, by = id_col)
  } else {
    if (!"user_id" %in% names(df)) {
      clear_progress_id()
      cli::cli_abort(
        "{.arg user_id} must exist as a column in the data frame.",
        call = arg$current_env
      )
    }
    df
  }
}



#' .get_current_date_time
#' @noRd
#' @keywords internal
#' @returns df with metadata attached
.get_current_date_time <- function() {
  current_datetime <- Sys.time()

  end_datetime <- current_datetime + lubridate::hours(1)

  start_time <- strftime(current_datetime, "%I:%M %p")

  end_time <- strftime(end_datetime, "%I:%M %p")

  if (is.na(end_time)) {
    end_time <- "12:00 AM"
  }

  start_date <- format(current_datetime, "%d/%m/%Y")

  end_date <- format(end_datetime, "%d/%m/%Y")

  list(
    start_time = start_time,
    end_time = end_time,
    start_date = start_date,
    end_date = end_date
  )
}


#' .missing_argument_error_msg
#' @noRd
#' @keywords internal
#' @return df with metadata attached
.missing_argument_error_msg <- function(date_or_time) {
  paste0(
    "data frame must contain both start_", date_or_time, " and end_",
    date_or_time, " variables,", " or just start_", date_or_time,
    " variable. Cannot contain just end_", date_or_time, " variable",
    " without corresponding start_", date_or_time, " variable."
  )
}


#' .insert_current_date_time
#' @noRd
#' @keywords internal
#' @returns df with metadata attached
.insert_current_date_time <- function(df, current_datetime, env) {
  df <- df %>% dplyr::mutate(row_num = dplyr::row_number())

  if (!"start_time" %in% names(df)) {
    df <- df %>% dplyr::mutate(start_time = current_datetime[["start_time"]])
  } else {
    .validate_import_time_leading_zero(df, "start_time", env)
    .validate_import_time_colon(df, "start_time", env)
    .validate_import_time_ampm(df, "start_time", env)
    .validate_import_time_length(df, "start_time", env)
    .validate_import_time_hour(df, "start_time", env)
  }

  if (!"start_date" %in% names(df)) {
    df <- df %>% dplyr::mutate(start_date = current_datetime[["start_date"]])
  } else {
    .validate_import_date_character(df, "start_date", env)
    .validate_import_date_length(df, "start_date", env)
  }

  if ("end_time" %in% names(df)) {
    .validate_import_time_leading_zero(df, "end_time", env)
    .validate_import_time_colon(df, "end_time", env)
    .validate_import_time_ampm(df, "end_time", env)
    .validate_import_time_length(df, "end_time", env)
    .validate_import_time_hour(df, "end_time", env)
  }

  if ("end_date" %in% names(df)) {
    .validate_import_date_character(df, "end_date", env)
    .validate_import_date_length(df, "end_date", env)
  }

  if (any(!c("end_time", "end_date") %in% names(df))) {
    df <- df %>%
      dplyr::mutate(
        start_datetime = lubridate::parse_date_time(
          paste(.data$start_date, .data$start_time), " %d/%m/%Y %I:%M %p"
        ),
        end_datetime = .data$start_datetime + lubridate::hours(1)
      )

    if (!"end_time" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(
          end_time = format(.data$end_datetime, "%I:%M %p")
        )
    }

    if (!"end_date" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(
          end_date = format(.data$end_datetime, "%d/%m/%Y")
        )
    }

    df <- df %>% dplyr::select(-"start_datetime", -"end_datetime")
  }
  df %>% dplyr::select(-dplyr::any_of("row_num"))
}

#' .insert_date_time
#'
#' Adds start/end date columns and start/end time columns to df
#'
#' Uploading to Smartabase requires the variables start_date, end_date,
#' start_time and end_time to be attached to the input data frame. By default,
#' this function looks for variable names that match any of those arg,
#' and if they are not found will set start_date = current date,
#' end_date = current date, start_time = current time and end_time =
#' current time + 1 hour.
#' @noRd
#' @keywords internal
#' @returns df with metadata attached
.insert_date_time <- function(df, current_datetime, env) {
  # Check if end_date or end_time columns exist without start_date/start_time
  if ("end_date" %in% names(df) && !"start_date" %in% names(df)) {
    clear_progress_id()
    cli::cli_abort(
      c(
        "!" = "Can't find {.field start_date} column.",
        "x" = "{.field end_date} is present but not {.field start_date}.",
        "i" = "Either remove {.field end_date} from the data frame, or add a \\
        {.field start_date} column."
      ),
      call = env
    )
  }

  if ("end_time" %in% names(df) && !"start_time" %in% names(df)) {
    clear_progress_id()
    cli::cli_abort(
      c(
        "!" = "Can't find {.field start_time} column.",
        "x" = "{.field end_time} is present but not {.field start_time}.",
        "i" = "Either remove {.field end_time} from the data frame, or add a \\
        {.field start_time} column."
      ),
      call = env
    )
  }
  current_datetime <- .get_current_date_time()

  # If any date/time columns still missing, add current date/time
  .insert_current_date_time(df, current_datetime, env)
}


#' .convert_12am_time
#'
#' Converts 12 AM start/end times to 00 AM start times
#' @noRd
#' @keywords internal
.convert_12am_time <- function(df) {
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("start_time", "end_time")),
        ~ stringr::str_replace(., "^12:(\\d{2}) (?i)am$", "00:\\1 AM")
      )
    )
}


#' .prepare_import_df
#'
#' Convert data frame to required JSON structure for Smartabase API upload#'
#' @noRd
#' @keywords internal
#' @returns Data to be uploaded to Smartabase - 'JSON'
.prepare_import_df <- function(df, arg) {
  df %>%
    .remove_protected_column_names(.) %>%
    .replace_na_with_empty_string(.) %>%
    .convert_id_names_to_lower(.) %>%
    .split_import_df(., arg)
}
