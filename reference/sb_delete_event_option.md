# Set option parameters for [`sb_delete_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_delete_event.md)

Set option parameters for
[`sb_delete_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_delete_event.md)

## Usage

``` r
sb_delete_event_option(interactive_mode = TRUE)
```

## Arguments

- interactive_mode:

  If TRUE, all messages are printed to the console. If FALSE, they are
  suppressed. The idea is that `interactive_mode` should be set to FALSE
  in automated environments to ensure logs aren't clogged up with
  progress messages.

## Value

A list of options with class = "sb_delete_option"

## Examples

``` r
if (FALSE) { # \dontrun{
# Calling the function with no arguments returns all the defaults
sb_delete_event_option()

# Specifying specific arguments will alter those arguments alone while still
# returning the other defaults
sb_get_group_option(guess_col_type = FALSE, interactive_mode = FALSE)
} # }
```
