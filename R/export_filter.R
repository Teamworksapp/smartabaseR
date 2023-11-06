
#' @title
#' Set filter parameters for [sb_sync_event()]
#'
#' @param user_key The type of user variable to filter by. The possible values
#' are `c("about", "username", "email", "group", "current_group")`.
#' @param user_value The specific user value to filter for e.g. if
#' `user_key = "username"`, then perhaps `user_value = "john.smith"`.
#'
#' @return A list of filters with class = "sb_export_filter"
#' @export
#'
#' @family export filter helpers
#' @seealso
#' [sb_get_event_filter()]
#' [sb_get_profile_filter()]
#' [sb_get_user_filter()]
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_sync_event_filter()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_sync_event_filter(user_key = "group", user_value = "Example Group")
#' }
sb_sync_event_filter <- function(
    user_key = c(
      "user_id", "about", "username", "email", "group", "current_group"
    ),
    user_value = NULL
) {
  if (!is.null(user_key)) {
    user_key <- rlang::arg_match(user_key)
  }

  structure(
    class = "sb_export_filter",
    list(
      user_key = user_key,
      user_value = user_value
    )
  )
}

#' @title
#' Set filter parameters for [sb_get_event()]
#'
#' @inheritParams sb_sync_event_filter
#' @param data_key The name of the Smartabase field to filter by.
#' @param data_value The specific value to filter for in the selected data key
#' @param data_condition Specify the condition you want to apply when filtering
#' i.e. "equal_to", "not_equal_to", "contains", "less_than", "greater_than",
#' "less_than_or_equal_to", "greater_than_or_equal_to".
#' @param events_per_user The maximum number of events to return per user,
#' ordered by most recent. If not specified or set to NULL, all events (that
#' meet any other the filter conditions) will be retrieved for each user.
#'
#' @return A list of filters with class = "sb_export_filter"
#' @export
#'
#' @family export filter helpers
#' @seealso
#' [sb_get_profile_filter()]
#' [sb_get_user_filter()]
#' [sb_sync_event_filter()]
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_get_event_filter()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_get_event_filter(user_key = "group", user_value = "Example Group")
#' }
sb_get_event_filter <- function(
    user_key = c(
      "user_id", "about", "username", "email", "group", "current_group"
    ),
    user_value = NULL,
    data_key = NULL,
    data_value = NULL,
    data_condition = c(
      "equal_to", "not_equal_to", "contains", "less_than",
      "greater_than", "less_than_or_equal_to", "greater_than_or_equal_to"
    ),
    events_per_user = NULL
) {
  if (!is.null(user_key)) {
    user_key <- rlang::arg_match(user_key)
  }
  if (!is.null(data_condition)) {
    data_condition <- rlang::arg_match(data_condition)
  }
  if (!is.null(events_per_user)) {
    events_per_user <- as.integer(events_per_user)
    if (!any(class(events_per_user) == "integer")) {
      cli::cli_progress_done(result = "clear")
      cli::cli_abort("{.arg {events_per_user}} must be numeric")
    }
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
      data_value = data_value,
      data_filter = data_filter,
      events_per_user = events_per_user
    )
  )
}


#' @title Set filter parameters for [sb_get_profile()]
#'
#' @inheritParams sb_sync_event_filter
#'
#' @return A list of filters with class = "sb_export_filter"
#' @export
#'
#' @family export filter helpers
#' @seealso
#' [sb_get_event_filter()]
#' [sb_sync_event_filter()]
#' [sb_get_user_filter()]
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_get_profile_filter()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_get_profile_filter(user_key = "group", user_value = "Example Group")
#' }
sb_get_profile_filter <- function(
    user_key = c(
      "user_id", "about", "username", "email", "group", "current_group"
    ),
    user_value = NULL
) {
  if (!is.null(user_key)) {
    user_key <- rlang::arg_match(user_key)
  }

  structure(
    class = "sb_export_filter",
    list(
      user_key = user_key,
      user_value = user_value
    )
  )
}


#' @title Set filter parameters for [sb_get_profile()]
#'
#' @inheritParams sb_sync_event_filter
#'
#' @return A list of filters with class = "sb_export_filter"
#' @export
#'
#' @family export filter helpers
#' @seealso
#' [sb_get_event_filter()]
#' [sb_sync_event_filter()]
#' [sb_get_profile_filter()]
#'
#' @examples
#' \dontrun{
#' # Calling the function with no arguments returns all the defaults
#' sb_get_user_filter()
#'
#' # Specifying specific arguments will alter those arguments alone while still
#' # returning the other defaults
#' sb_get_user_filter(user_key = "group", user_value = "Example Group")
#' }
sb_get_user_filter <- function(
    user_key = c(
      "user_id", "about", "username", "email", "group", "current_group"
    ),
    user_value = NULL
) {
  user_key <- rlang::arg_match(user_key)
  if (user_key == "none") user_key <- NULL

  structure(
    class = "sb_export_filter",
    list(
      user_key = user_key,
      user_value = user_value
    )
  )
}