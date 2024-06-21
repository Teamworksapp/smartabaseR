#' @title
#' Set filter parameters for [sb_sync_event()]
#'
#' @param user_key The type of user variable to filter by. The possible values
#' are `c("about", "username", "email", "group", "current_group")`.
#' @param user_value The specific user value to filter for e.g. if
#' `user_key = "username"`, then perhaps `user_value = "john.smith"`.
#'
#' @returns A list of filters with class = "sb_export_filter"
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
    user_key = NULL,
    user_value = NULL) {
  .validate_user_key(user_key)
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
#' @param events_per_user The maximum number of events to return per athlete,
#' ordered by most recent. If not specified or set to NULL, all events will be
#' retrieved for each athlete
#'
#' @returns A list of filters with class = "sb_export_filter"
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
    user_key = NULL,
    user_value = NULL,
    data_key = NULL,
    data_value = NULL,
    data_condition = "equal_to",
    events_per_user = NULL) {
  .validate_user_key(user_key)
  .validate_data_condition(data_condition)
  .validate_events_per_user(events_per_user)
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
#' @returns A list of filters with class = "sb_export_filter"
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
    user_key = NULL,
    user_value = NULL) {
  .validate_user_key(user_key)
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
#' @returns A list of filters with class = "sb_export_filter"
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
    user_key = NULL,
    user_value = NULL) {
  .validate_user_key(user_key)
  structure(
    class = "sb_export_filter",
    list(
      user_key = user_key,
      user_value = user_value
    )
  )
}
