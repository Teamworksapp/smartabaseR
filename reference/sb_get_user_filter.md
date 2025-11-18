# Set filter parameters for [`sb_get_profile()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile.md)

Set filter parameters for
[`sb_get_profile()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile.md)

## Usage

``` r
sb_get_user_filter(user_key = NULL, user_value = NULL)
```

## Arguments

- user_key:

  The type of user variable to filter by. The possible values are
  `c("about", "username", "email", "group", "current_group")`.

- user_value:

  The specific user value to filter for e.g. if `user_key = "username"`,
  then perhaps `user_value = "john.smith"`.

## Value

A list of filters with class = "sb_export_filter"

## See also

[`sb_get_event_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event_filter.md)
[`sb_sync_event_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event_filter.md)
[`sb_get_profile_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile_filter.md)

Other export filter helpers:
[`sb_get_event_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event_filter.md),
[`sb_get_profile_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile_filter.md),
[`sb_sync_event_filter()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event_filter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calling the function with no arguments returns all the defaults
sb_get_user_filter()

# Specifying specific arguments will alter those arguments alone while still
# returning the other defaults
sb_get_user_filter(user_key = "group", user_value = "Example Group")
} # }
```
