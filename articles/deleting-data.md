# Deleting Data

If you haven’t already, please read the
[`vignette("get-started")`](https://teamworksapp.github.io/smartabaseR/articles/get-started.md)
before preceding.

[`sb_delete_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_delete_event.md)
uses the Smartabase API to delete event data in Smartabase. It requires
an `event_id` value.

Use this function at your own risk. Once records are deleted, they
cannot be retrieved. It is advised that you practice how this function
works using test data before running on real data.

[`sb_delete_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_delete_event.md)
is fairly straightforward. Besides the usual Smartabase credentials, it
expects only one argument: `event_id`. This is the ID of the Smartabase
event that is returned by
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)
or `sb_synchronise_event()`.

An example workflow might start with first exporting the to-be-deleted
data with
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md):

``` r
example_data <- sb_get_event(
  form = "Example Form",
  date_range = c("01/03/2023", "07/03/2023"),
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

    #> # A tibble: 4 × 8
    #>   about      user_id form  start_date Distance   RPE entered_by_user_id event_id
    #>   <chr>        <dbl> <chr> <chr>         <dbl> <dbl>              <dbl>    <dbl>
    #> 1 Charlie T…   31808 Exam… 15/04/2019     5411     7              37201    16517
    #> 2 Jack Will…   31811 Exam… 15/04/2019     2374     3              37201    36505
    #> 3 Jamie And…   37201 Exam… 15/04/2019     1600     6              37201    36014
    #> 4 Liam Walk…   37204 Exam… 15/04/2019     2564     5              37201    28970

Let’s say that we need to delete the record with `user_id = 31808`. It
has `event_id = 16517`. We would pass that event ID to
[`sb_delete_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_delete_event.md)
like so:

``` r
sb_delete_event(
  event_id = 16517,
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

    #> SUCCESS: Deleted 16517

That event will now be deleted from Smartabase.

Once again, please use the function at your own risk and put appropriate
measures in place to test and verify that it is doing as you expect
before applying to real data.
