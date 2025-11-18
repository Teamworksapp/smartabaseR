# Set option parameters for [`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)

Set option parameters for
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)

## Usage

``` r
sb_get_user_option(
  interactive_mode = TRUE,
  include_all_cols = FALSE,
  cache = TRUE,
  include_missing_user = FALSE,
  guess_col_type = TRUE
)
```

## Arguments

- interactive_mode:

  If TRUE, all messages are printed to the console. If FALSE, they are
  suppressed. The idea is that `interactive_mode` should be set to FALSE
  in automated environments to ensure logs aren't clogged up with
  progress messages.

- include_all_cols:

  If FALSE, only basic user information is included in the returned data
  frame; e.g. user_id, username, about etc. If TRUE, extra user
  information like phone, address and date of birth are also returned.
  If only one user is returned, roles and groups information about that
  user is included.

- cache:

  If TRUE, login token and user details are cached once per session.
  Similar to `include_user_data`, caching can have performance benefits
  when the user data set is large, such as on large Smartabase sites
  with thousands of users. To invalidate the cache, restart your R
  session.

- include_missing_user:

  If TRUE, includes users who have not recorded data in that form as
  rows of NA values.

- guess_col_type:

  If TRUE,
  [`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)
  uses
  [`readr::type_convert()`](https://readr.tidyverse.org/reference/type_convert.html)
  to guess the column types of the exported data frame. If FALSE, all
  columns will come through as character.

## Value

A list of options with class = "sb_export_option"

## See also

[`sb_get_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event_option.md)
[`sb_sync_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event_option.md)
[`sb_get_profile_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile_option.md)
[`sb_get_group_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_group_option.md)

Other export option helpers:
[`sb_get_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event_option.md),
[`sb_get_group_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_group_option.md),
[`sb_get_profile_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile_option.md),
[`sb_sync_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event_option.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calling the function with no arguments returns all the defaults
sb_get_user_option()

# Specifying specific arguments will alter those arguments alone while still
# returning the other defaults
sb_get_user_option(guess_col_type = FALSE, interactive_mode = FALSE)
} # }
```
