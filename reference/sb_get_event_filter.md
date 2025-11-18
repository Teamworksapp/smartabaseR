# Set filter parameters for [`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)

Set filter parameters for
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)

## Usage

``` r
sb_get_event_filter(
  user_key = NULL,
  user_value = NULL,
  data_key = NULL,
  data_value = NULL,
  data_condition = "equal_to",
  events_per_user = NULL
)
```

## Arguments

- user_key:

  The type of user variable to filter by. The possible values are
  `c("about", "username", "email", "group", "current_group")`.

- user_value:

  The specific user value to filter for e.g. if `user_key = "username"`,
  then perhaps `user_value = "john.smith"`.

- data_key:

  The name of the Smartabase field to filter by.

- data_value:

  The specific value to filter for in the selected data key

- data_condition:

  Specify the condition you want to apply when filtering i.e.
  "equal_to", "not_equal_to", "contains", "less_than", "greater_than",
  "less_than_or_equal_to", "greater_than_or_equal_to".

- events_per_user:

  The maximum number of events to return per athlete, ordered by most
  recent. If not specified or set to NULL, all events will be retrieved
  for each athlete

## Value

A list of filters with class = "sb_export_filter"

## See also

[`sb_get_profile_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile_filter.md)
[`sb_get_user_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_user_filter.md)
[`sb_sync_event_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event_filter.md)

Other export filter helpers:
[`sb_get_profile_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile_filter.md),
[`sb_get_user_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_user_filter.md),
[`sb_sync_event_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event_filter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calling the function with no arguments returns all the defaults
sb_get_event_filter()

# Specifying specific arguments will alter those arguments alone while still
# returning the other defaults
sb_get_event_filter(user_key = "group", user_value = "Example Group")
} # }
```
