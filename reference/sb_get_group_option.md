# Set option parameters for [`sb_get_group()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_group.md)

Set option parameters for
[`sb_get_group()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_group.md)

## Usage

``` r
sb_get_group_option(guess_col_type = TRUE, interactive_mode = TRUE)
```

## Arguments

- guess_col_type:

  If TRUE,
  [`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)
  uses
  [`readr::type_convert()`](https://readr.tidyverse.org/reference/type_convert.html)
  to guess the column types of the exported data frame. If FALSE, all
  columns will come through as character.

- interactive_mode:

  If TRUE, all messages are printed to the console. If FALSE, they are
  suppressed. The idea is that `interactive_mode` should be set to FALSE
  in automated environments to ensure logs aren't clogged up with
  progress messages.

## Value

A list of options with class = "sb_export_option"

## See also

[`sb_get_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event_option.md)
[`sb_sync_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event_option.md)
[`sb_get_user_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_user_option.md)
[`sb_get_profile_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile_option.md)

Other export option helpers:
[`sb_get_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event_option.md),
[`sb_get_profile_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile_option.md),
[`sb_get_user_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_user_option.md),
[`sb_sync_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event_option.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calling the function with no arguments returns all the defaults
sb_get_group_option()

# Specifying specific arguments will alter those arguments alone while still
# returning the other defaults
sb_get_group_option(guess_col_type = FALSE, interactive_mode = FALSE)
} # }
```
