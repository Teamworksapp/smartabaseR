# Synchronising with Smartabase

[`sb_sync_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event.md)
is a way to determine which events have been entered/updated after a
certain time (`last_sync_time`):

``` r

sync_data <- sb_sync_event(
  form = "Example event form",
  type = "synchronise",
  last_sync_time = 1672531200000
)
```

Note: `last_sync_time` is represented in unix time, which means the
number of milliseconds since 1970-01-01. For reference we can convert
the unix time value to a UTC datetime object like so:
`as.POSIXct(1672531200000/1000, origin = "1970-01-01")`.

The above call to
[`sb_sync_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event.md)
will return a tibble with all of the events that have been
inserted/updated since `1672531200000` (that is,
`01/01/2023 00:00:00 UTC`).

    #> ℹ Form: Example event form
    #> ℹ HTTP status code: 200
    #> ℹ Export time: 2023-06-01 19:35:58 UTC
    #> ℹ Unique records: 4
    #> # A tibble: 4 × 8
    #>   about      user_id form  start_date Distance   RPE entered_by_user_id event_id
    #>   <chr>        <dbl> <chr> <chr>         <dbl> <dbl>              <dbl>    <dbl>
    #> 1 Charlie T…   31808 Exam… 15/04/2023     5411     7              37201    16517
    #> 2 Jack Will…   31811 Exam… 15/04/2023     2374     3              37201    36505
    #> 3 Jamie And…   37201 Exam… 15/04/2023     1600     6              37201    36014
    #> 4 Liam Walk…   37204 Exam… 15/04/2023     2564     5              37201    28970

The data returned from
[`sb_sync_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event.md)
also comes with specific attributes attached to the data frame. The most
important is the time in which Smartabase returned the data to you. You
can access that value by using the
[`attr()`](https://rdrr.io/r/base/attr.html) function:

``` r

new_sync_time <- attr(sync_example, "new_sync_time")
```

    #> [1] 1.686086e+12

Now, the next time you synchronise with that form, you can use the newly
acquired `new_sync_time` value to find any records that have been
inserted/updated since you last called
[`sb_sync_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event.md):

``` r

next_sync_example <- sb_sync_event(
  form = "Example profile form",
  type = "synchronise",
  last_sync_time = new_sync_time
)
```

    #> # A tibble: 1 × 8
    #>   about      user_id form  start_date Distance   RPE entered_by_user_id event_id
    #>   <chr>        <dbl> <chr> <chr>         <dbl> <dbl>              <dbl>    <dbl>
    #> 1 Jamie And…   37201 Exam… 18/04/2023     3100     7              37201    62331

So why is this useful? By querying which records have been
inserted/updated in a form beyond a certain time, you can set up
workflows that automatically trigger R scripts whenever new data is
detected. For more information on how to achieve this, contact
Teamworks.
