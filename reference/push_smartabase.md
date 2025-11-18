# push_smartabase

**\[deprecated\]**

`push_smartabase()` was deprecated in favour of the more explicitly
named
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md),
[`sb_update_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_update_event.md),
`sb_update_profile()` or
[`sb_upsert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_upsert_event.md).
Please use those instead.

Uploads a data frame to a Smartabase event or profile form

The push_smartabase() function uses the Smartabase API and imports an R
data frame into a specific Smartabase event or profile form. Set 'type'
= "profile" to push to a profile form. For more details see the help
vignette:
[`vignette("get-started")`](https://teamworksapp.github.io/smartabaseR/articles/get-started.md)

## Usage

``` r
push_smartabase(
  df,
  form,
  ...,
  url = NULL,
  username = NULL,
  password = NULL,
  entered_by_user_id = NULL,
  type = "event",
  get_id = FALSE,
  match_id_to_column = NULL,
  table_fields = NULL,
  start_date = NULL,
  end_date = NULL,
  current_date_format = NULL,
  start_time = NULL,
  end_time = NULL,
  edit_event = FALSE,
  cloud_mode = FALSE,
  shiny_progress_code = NULL,
  cache = TRUE
)
```

## Arguments

- df:

  data frame: data to be uploaded to Smartabase

- form:

  string: Name of Smartabase form

- url:

  string: Smartabase url e.g. 'example.smartabase.com/site'

- username:

  string: Smartabase username – ignore if setup with save_credentials()

- password:

  string: Smartabase password – ignore if setup with save_credentials()

- entered_by_user_id:

  string: user ID of user uploading the data – ignore if setup with
  save_credentials()

- type:

  string: either 'event' or 'profile'

- get_id:

  boolean: if TRUE, searches for user IDs of athletes listed in data
  frame (requires a value supplied to 'match_id_to_column')

- match_id_to_column:

  string: names of columns in data frame that match either 'about',
  'username' or 'email'. User IDs will be returned that match data frame
  values. Requires 'get_id' = TRUE

- table_fields:

  vector: supply a vector of column names that are going to be uploaded
  into a table. In Smartabase, this is equivalent to ticking 'Treat all
  records for the same user, on the same day as a single record?'

- start_date:

  string: name of start date column in data frame. If NULL, and 'Date'
  or 'start_date' are not already columns in the data frame, creates a
  new start_date column based on current date

- end_date:

  string: name of end date column in data frame. If NULL, and 'end_date'
  is not already a column in the data frame, creates a new 'end_date'
  column based on 'start_date'

- current_date_format:

  string: current format of date variable of interest. Set this argument
  to convert 'start_date' or 'end_date' to dd/mm/yyyy

- start_time:

  string: name of start time column in data frame with 'h:mm AM' or
  'h:mm PM' records. If NULL, and 'start_time' is not already a column
  in the data frame, creates a new 'start_time' column based on
  Sys.time()

- end_time:

  string: name of end time column in data frame with 'h:mm AM' or 'h:mm
  PM' records. If NULL, and 'end_time' is not already a column in the
  data frame, creates a new 'end_time' column with values +1 hour after
  'start_time'

- edit_event:

  boolean: if TRUE, will look for 'event_id' in data frame. Records with
  matching event_id on Smartabase will be overwritten with the new data

- cloud_mode:

  boolean: if TRUE, confirmation pop-up will not appear when editing
  events (i.e. for use in non-local, cloud environments)

## Value

A data frame
