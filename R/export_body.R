#' .build_export_filter
#'
#' Build data filter object for `sb_get_event()`
#'
#' @param data_key Smartabase field to be filtered on
#' @param data_value Value the data_key should be filtered for
#' @param data_condition How the data_key should relate to the data_value
#'
#' @noRd
#' @keywords internal
#' @returns A [list()]
.build_export_filter <- function(data_key, data_value, data_condition) {
  data_key <- eval(data_key)
  data_value <- eval(data_value)
  data_condition <- eval(data_condition)
  length_key <- length(data_key)
  length_condition <- length(data_condition)
  length_value <- length(data_value)

  if (length_key < length_value) {
    data_key <- rep(data_key, length_value)
    length_key <- length(data_key)
  }

  data_condition <- seq_len(length(data_condition)) %>%
    purrr::map(
      ~ .select_filter_condition(
        data_condition[[.x]]
      )
    ) %>%
    purrr::reduce(., c)

  all_equal_length <- all(
    purrr::map_lgl(
      list(length_condition, length_value), function(x) x == length_key
    )
  )

  if (any(duplicated(data_key))) {
    clear_progress_id()
    cli::cli_abort("data_key cannot have duplicate elements")
  }

  if (!all_equal_length) {
    if (length_key == 1) {
      if (length_condition > 1 | length_value > 1) {
        clear_progress_id()
        cli::cli_abort(
          "{.arg data_key}, {.arg data_condition} and {.arg data_value} must be
           the same length"
        )
      }
    } else {
      if (length_key == length_condition && length_value == 1) {
        data_value <- list(rep(data_value, length_key))
      } else if (length_key == length_value && length_condition == 1) {
        data_condition <- list(
          rep(data_condition, length_key)
        )
      } else if (length_condition == 1 && length_value == 1) {
        data_condition <- list(
          rep(data_condition, length_key)
        )
        data_value <- list(rep(data_value, length_key))
      } else {
        clear_progress_id()
        cli::cli_abort(
          "{.arg data_key}, {.arg data_condition} and {.arg data_value} must be
           the same length"
        )
      }
    }
  }

  if (!is.null(data_key[[1]])) {
    filter <- list(list(
      formName = "__formName__",
      filterSet = seq_along(data_value) %>%
        purrr::map(~ list(
          key = data_key[[.x]],
          value = as.character(data_value[[.x]]),
          filterCondition = data_condition[[.x]]
        ))
    ))
  } else {
    filter <- NULL
  }
  filter
}


#' .build_export_event_body
#'
#' Build body to be passed to event export endpoint
#'
#' @noRd
#' @keywords internal
#' @returns A character vector
.build_export_event_body <- function(arg, user_id) {
  data_filters <- arg$filter$data_filter
  filters <- arg$filter
  options <- arg$option
  key <- data_filters %>% purrr::pluck(1, "filterSet", 1, "key")

  body <- list(
    formNames   = arg$form,
    userIds     = user_id,
    startDate   = arg$date_range[[1]][[1]],
    finishDate  = arg$date_range[[2]][[1]],
    startTime   = arg$time_range[[1]][[1]],
    finishTime  = arg$time_range[[2]][[1]]
  )

  if (!is.null(key)) {
    body$filter <- data_filters
  }
  if (!is.null(filters$events_per_user)) {
    body$resultsPerUser <- filters$events_per_user
  }
  body
}


#' .build_profile_event_body
#'
#' Build body to be passed to profile export endpoint
#'
#' @noRd
#' @keywords internal
#' @returns A [list()]
.build_export_profile_body <- function(arg, user_id) {
  list(
    formNames = arg$form,
    userIds = user_id
  )
}


#' .build_export_synchronise_body
#'
#' Build body to be passed to synchronise export endpoint
#'
#' @noRd
#' @keywords internal
#' @returns A character vector
.build_export_synchronise_body <- function(arg, user_id) {
  if (is.null(arg$last_sync_time)) {
    body <- list(
      formName = arg$form,
      userIds = user_id
    )
  } else {
    body <- list(
      formName = arg$form,
      userIds = user_id,
      lastSynchronisationTimeOnServer = arg$last_sync_time
    )
  }
  body
}


#' .build_export_body
#'
#' Helper that encapsulates logic for passing to correct build function based
#' on arg$type
#'
#' @noRd
#' @keywords internal
#' @returns A character vector
.build_export_body <- function(arg, user_id = NULL) {
  if (arg$type == "synchronise") {
    .build_export_synchronise_body(arg, user_id)
  } else if (arg$type == "profile") {
    .build_export_profile_body(arg, user_id)
  } else if (arg$type == "event") {
    .build_export_event_body(arg, user_id)
  } else if (arg$type == "user") {
    .build_export_id_body(arg)
  } else if (arg$type == "group") {
    .build_export_group_body(arg)
  }
}


#' .select_filter_condition
#'
#' @noRd
#' @keywords internal
#' @returns A number
.select_filter_condition <- function(cond) {
  if (cond == "equal_to" | cond == "=") 1
  else if (cond == "not_equal_to" | cond == "!=") 2
  else if (cond == "contains" | cond == "%in%") 3
  else if (cond == "less_than" | cond == "<") 4
  else if (cond == "greater_than" | cond == ">") 5
  else if (cond == "less_than_or_equal_to" | cond == "<=") 6
  else if (cond == "greater_than_or_equal_to" | cond == ">=") 7
}


#' .build_id_body
#'
#' @noRd
#' @keywords internal
#' @returns A [list()]
.build_export_id_body <- function(arg) {
  user_key <- arg$filter$user_key
  user_value <- arg$filter$user_value
  if (is.null(user_key)) {
    body <- list(identification = NULL)
  } else {
    if (user_key == "current_group") {
      body <- list(name = "")
    } else if (user_key == "group") {
      body <- list(name = user_value)
    } else {
      if (user_key == "about") {
        identification <- user_value %>%
          purrr::map(
            ~ list(
              firstName = stringr::word(.x, 1),
              lastName  = stringr::word(.x, 2, -1)
            )
          )
      } else if (user_key == "user_id") {
        identification <- user_value %>%
          purrr::map(~ list(userId = as.numeric(.x)))
      } else if (user_key == "username") {
        identification <- user_value %>%
          purrr::map(~ list(username = .x))
      } else if (user_key == "email") {
        identification <- user_value %>%
          purrr::map(~ list(emailAddress = .x))
      }

      body <- list(identification = identification)
    }
  }

  body
}


#' .build_id_body
#'
#' @noRd
#' @keywords internal
#' @returns A [list()]
.build_export_group_body <- function(arg) {
  list(name = "")
}
