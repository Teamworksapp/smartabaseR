# pull_smartabase

**\[deprecated\]**

`pull_smartabase()` was deprecated in favour of the more explicitly
named
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md),
[`sb_get_profile()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_profile.md)
or
[`sb_sync_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event.md).
Please use these instead

Downloads data from a Smartabase event or profile form

This function pulls data from a Smartabase event form or profile form
using the Smartabase API. For more details see the help vignette:
`vignette("pulling-data")`

## Usage

``` r
pull_smartabase(
  form,
  ...,
  url = NULL,
  type = "event",
  download_attachment = FALSE,
  start_date = NULL,
  end_date = NULL,
  last = NULL,
  start_time = "12:00 am",
  end_time = "11:59 pm",
  username = NULL,
  password = NULL,
  filter_user_key = NULL,
  filter_user_value = NULL,
  filter_data_key = NULL,
  filter_data_value = NULL,
  filter_data_condition = "equal_to",
  include_missing_user = FALSE,
  guess_col_type = TRUE,
  get_uuid = FALSE,
  cloud_mode = FALSE,
  last_sync_time = NULL,
  shiny_progress_code = NULL,
  dev_mode = FALSE
)
```

## Arguments

- form:

  string: name of Smartabase form

- url:

  string: Smartabase url e.g. 'example.smartabase.com/site'

- type:

  string: either 'event', 'profile' or 'synchronise'

- download_attachment:

  boolean: if TRUE, will download any files that are included in the
  form via a file upload/multiple file upload field

- start_date:

  string: 'dd/mm/yyyy'

- end_date:

  string: 'dd/mm/yyyy'

- username:

  string: Smartabase username – ignore if setup with save_credentials()

- password:

  string: Smartabase password – ignore if setup with save_credentials()

## Value

Smartabase data

## Examples

``` r
if (FALSE) { # \dontrun{
# Get one week of wellness data from example.smartabase.com/site:
wellness_data <- pull_smartabase(
  url = "example.smartabase.com/site",
  form = "Daily Wellness",
  username = "john.smith",
  password = "examplePassword",
  start_date = "15/04/2019",
  end_date = "22/04/2019"
)
} # }
```
