# Importing Data

If you haven’t already, please read the
[`vignette("get-started")`](https://teamworksapp.github.io/smartabaseR/articles/get-started.md)
and
[`vignette("exporting-data")`](https://teamworksapp.github.io/smartabaseR/articles/exporting-data.md)
vignettes before proceeding.

## Import functions

`smartabaseR` provides four general functions for importing data into
Smartabase:
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md),
[`sb_update_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_update_event.md),
[`sb_upsert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_upsert_event.md)
and
[`sb_upsert_profile()`](https://teamworksapp.github.io/smartabaseR/reference/sb_upsert_profile.md).

### sb_insert_event()

‘Inserting’ data into Smartabase means you are creating new events;
i.e. inserting new rows into the Smartabase event database.
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md)
is the most commonly used import function and ensures no chance of
existing events being updated/overwritten.

#### Example

``` r
training_data <- dplyr::tibble(
  start_date = "13/01/2023",
  user_id = c(37204, 37201),
  about = c("Aiden Thomas", "Jamie Anderson"),
  distance = c(2530, 5411),
  rpe = c(5, 7),
  entered_by_user_id = c(37204, 37201)
)

training_data
#> # A tibble: 2 × 6
#>   start_date user_id about          distance   rpe entered_by_user_id
#>   <chr>        <dbl> <chr>             <dbl> <dbl>              <dbl>
#> 1 13/01/2023   37204 Aiden Thomas       2530     5              37204
#> 2 13/01/2023   37201 Jamie Anderson     5411     7              37201
```

``` r
sb_insert_event(
  df = training_data,
  form = "Team Summary"
)
```

    #> ✔ SUCCESSFULLY_INSERTED: 2 out of 2 records successfully inserted into Team Summary.

### sb_update_event()

‘Updating’ data in Smartabase means you are editing existing events in
the Smartabase event database.
[`sb_update_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_update_event.md)
requires a valid `event_id` column in the supplied data frame. These are
Smartabase-generated unique IDs that ensure only the correct events are
updated/overwritten.

#### Updating is the same as overwriting

Important side note: we chose the term ‘update’ for
[`sb_update_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_update_event.md)
because it aligns with standard database terminology, but arguably a
more appropriate name would be `sb_overwrite_event()`.

That’s because every event in the data frame supplied to
[`sb_update_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_update_event.md)
completely overwrites the existing event in the Smartabase event
database.

Practically this also means that most calls to
[`sb_update_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_update_event.md)
should be preceded by a call to
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)/[`sb_sync_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_sync_event.md).
The latter return a data frame that contain the entire Smartabase form,
including every column/field that contains data; not to mention the
necessary `event_id` column.

#### Example

The workflow below involves exporting event data from Smartabase via
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md),
applying some transformation and then importing the results using
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md).

``` r
training_data <- sb_get_event(
  form = "Training Log",
  date_range = c("15/04/2023", "15/04/2023"),
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

    #> # A tibble: 2 × 7
    #>   form         start_date user_id event_id Distance   RPE Team Average Distanc…¹
    #>   <chr>        <chr>        <dbl>    <dbl>    <dbl> <dbl> <lgl>                 
    #> 1 Training Log 15/04/2023   37204  2381840     2530     5 NA                    
    #> 2 Training Log 15/04/2023   37201  2382033     5411     7 NA                    
    #> # ℹ abbreviated name: ¹​`Team Average Distance`

We have two records. Their event IDs are `2381840` and `238203`.

Notice that the field `Team Average Distance` is currently empty. Let’s
imagine we want to populate that field with the team’s average distance
without creating new records. First, let’s do the calculation:

``` r
library(dplyr)

avg_dist_data <- training_data %>%
  mutate(across("Team Average Distance", ~ round(mean(Distance), 0)))

avg_dist_data
#> # A tibble: 2 × 7
#>   form         start_date user_id event_id Distance   RPE Team Average Distanc…¹
#>   <chr>        <chr>        <dbl>    <dbl>    <dbl> <dbl>                  <dbl>
#> 1 Training Log 15/04/2023   37204  2381840     2530     5                   3970
#> 2 Training Log 15/04/2023   37201  2382033     5411     7                   3970
#> # ℹ abbreviated name: ¹​`Team Average Distance`
```

All we need to do now is import our `avg_dist_data` data back into our
`Training Log` form via
[`sb_update_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_update_event.md)
and the `Team Average Distance` field will be populated:

``` r
sb_update_event(
  df = avg_dist_data,
  form = "Training Log",
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

Running the above code will create a screen prompt that you will have to
interact with:

    #> 
    #> ── WARNING ─────────────────────────────────────────────────────────────────────
    #> ! You are about to UPDATE 2 records in Training Log.
    #> ℹ Please be aware the incoming data will overwrite the existing data.
    #> The Smartabase API will be called once.
    #> → Are you sure you want to continue?
    #> 1: Yes
    #> 2: No

If you input “1” or “Yes” into the console then you’ll see:

    #> ✔ SUCCESSFULLY_UPDATED: 2 out of 2 records successfully updated in Training Log.

### sb_upsert_event()

[‘Upserting’](https://en.wiktionary.org/wiki/upsert) data into
Smartabase means events can either be created or updated, depending on
the `event_id` column. Rows with valid event IDs will update the
matching rows in the Smartabase event database, whereas rows with no
`event_id` will be inserted as new rows in the Smartabase event
database.

#### Example

In this example we’ll get some training data from Smartabase using
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md).
We’ll then round down the `Distance` values to the nearest 100 and add
two more rows where `Distance = 3000`.

To illustrate how
[`sb_upsert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_upsert_event.md)
works, let’s first get some event data from Smartabase:

``` r
training_data <- sb_get_event(
  form = "Training Log",
  date_range = c("15/04/2023", "15/04/2023"),
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

    #> # A tibble: 2 × 6
    #>   form         start_date user_id event_id Distance   RPE
    #>   <chr>        <chr>        <dbl>    <dbl>    <dbl> <dbl>
    #> 1 Training Log 15/04/2023   37204  2381840     2530     5
    #> 2 Training Log 15/04/2023   37201  2382033     5411     7

Let’s say that we want to update this data in Smartabase such that the
`Distance` field is rounded down to the nearest 100:

``` r
library(dplyr)

training_data <- training_data %>%
  mutate(across(Distance, ~ round(., digits = -2)))

training_data
#> # A tibble: 2 × 6
#>   form         start_date user_id event_id Distance   RPE
#>   <chr>        <chr>        <dbl>    <dbl>    <dbl> <dbl>
#> 1 Training Log 15/04/2023   37204  2381840     2500     5
#> 2 Training Log 15/04/2023   37201  2382033     5400     7
```

Imagine then we had some new `Training Log` data to insert into
Smartabase. Here’s the new data:

    #> # A tibble: 2 × 4
    #>   start_date user_id Distance   RPE
    #>   <chr>        <dbl>    <dbl> <dbl>
    #> 1 16/04/2023   37207     3000     4
    #> 2 16/04/2023   37207     3000     4

Joining the data exported from Smartabase with this new data gives:

``` r
upsert_data <- full_join(training_data, extra_data)
#> Joining with `by = join_by(start_date, user_id, Distance, RPE)`

upsert_data
#> # A tibble: 4 × 6
#>   form         start_date user_id event_id Distance   RPE
#>   <chr>        <chr>        <dbl>    <dbl>    <dbl> <dbl>
#> 1 Training Log 15/04/2023   37204  2381840     2500     5
#> 2 Training Log 15/04/2023   37201  2382033     5400     7
#> 3 NA           16/04/2023   37207       NA     3000     4
#> 4 NA           16/04/2023   37207       NA     3000     4
```

Note how the new data for athlete `37207` does not contain event IDs.
This will mean that supplying the `upsert_data` frame to
[`sb_upsert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_upsert_event.md)
will invoke two API calls: one to insert the rows where `event_id = NA`
and another to update the remaining events:

``` r
sb_upsert_event(
  df = upsert_data,
  form = "Training Log",
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

    #> 
    #> ── WARNING ─────────────────────────────────────────────────────────────────────
    #> ! You are about to UPDATE 2 records in Training Log.
    #> ℹ Please be aware the incoming data will overwrite the existing data.
    #> ℹ You will also INSERT 2 new records.
    #> The Smartabase API will be called twice.
    #> → Are you sure you want to continue?
    #> 1: Yes
    #> 2: No

    #> ✔ 1: SUCCESSFULLY_UPDATED: 2 out of 2 records successfully updated in Training Log.
    #> ✔ 2: SUCCESSFULLY_INSERTED: 2 out of 2 records successfully inserted into updated in Training Log.

### sb_upsert_profile()

[`sb_upsert_profile()`](https://teamworksapp.github.io/smartabaseR/reference/sb_upsert_profile.md)
is the only function for importing profile data into Smartabase. Profile
forms can only store one record per athlete and therefore every import
into a profile form is an upsert. That is, for each athlete, the first
time data is imported into a profile form it will be an insert, but
future profile imports will all be updates.

#### Example

``` r
example_profile_data <- dplyr::tibble(
  start_date = "13/01/2023",
  user_id = 37204,
  about = "Aiden Thomas",
  Team = "First Team",
  entered_by_user_id = 37204
)

training_data
#> # A tibble: 2 × 6
#>   form         start_date user_id event_id Distance   RPE
#>   <chr>        <chr>        <dbl>    <dbl>    <dbl> <dbl>
#> 1 Training Log 15/04/2023   37204  2381840     2500     5
#> 2 Training Log 15/04/2023   37201  2382033     5400     7
```

``` r
sb_upsert_profile(
  df = example_profile_data,
  form = "Example Profile Form",
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

    #> ✔ SUCCESSFULLY_INSERTED: 1 out of 1 records successfully inserted into Example Profile Form.

## `option`

All `option` values must be generated by the relevant helper function.
So if we’re using
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md)
and we want to set `table_field = "Example Table Field"`, we would set
`table_field = "Example Table Field"` within
[`sb_get_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event_option.md).
For example:

``` r
sb_get_event(
  form = "Example Form",
  date_range = c("01/03/2023", "07/03/2023"),
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password",
  option = sb_insert_event_option(
    table_field = "Example Table Field"
  )
)
```

Each import function has an associated option function:

- `sb_update_event(..., option = sb_update_event_option())`
- `sb_upsert_event..., option = sb_upsert_event_option())`
- `sb_upsert_profile(..., option = sb_upsert_profile_option())`
- etc…

Below goes into more detail about each available option.

Note: even though the below examples reference
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md),
the same advice can be applied to the other import functions:
[`sb_update_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_update_event.md),
[`sb_upsert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_upsert_event.md)
or
[`sb_upsert_profile()`](https://teamworksapp.github.io/smartabaseR/reference/sb_upsert_profile.md).

### `cache`

The first time you hit the Smartabase API within a given R session (the
first time you call
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md),
for instance), `smartabaseR` in the background first calls
[`sb_login()`](https://teamworksapp.github.io/smartabaseR/reference/sb_login.md).
If the `cache` option is set to `TRUE` (default), then this login object
is then saved into memory and passed to further calls to the Smartabase
API. If the `cache` option is `FALSE`, then subsequent calls to the
Smartabase API will be preceded by a call to
[`sb_login()`](https://teamworksapp.github.io/smartabaseR/reference/sb_login.md).

### `id_col`

Whenever you import data into Smartabase, the API requires that there
exists a column called `user_id` in the data frame. This represents each
athlete’s Smartabase-generated user ID. The `id_col` defaults to
`"user_id"` for this reason.

However, there may be situations where you don’t have the relevant
Smartabase user IDs handy. There are two approaches here: firstly, you
could make a call to sb_get_user() to get the user IDs yourself, then
join that to the event data by “user_id”; or, secondly, if your data
frame contains some other kind of identifying column like “username”,
“email” or “about” (that is, full name), then you can simply set the
`id_col` argument to the name of the identifying column. For instance,
`id_col = "about"`.

`id_col` can either be set to “username”, “email” or “about”.

#### Example

Below is a data frame we want to insert into the “Training Log” form
using
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md).

    #> # A tibble: 2 × 4
    #>   start_date about          Distance   RPE
    #>   <chr>      <chr>             <dbl> <dbl>
    #> 1 15/04/2023 Aiden Thomas       2530     5
    #> 2 15/04/2023 Jamie Anderson     5411     7

Note how there is no `user_id` column, but there is an `about` column.
Our call to
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md)
would then set `option = sb_insert_event_option(id_col = "about")`:

``` r
sb_insert_event(
  df = training_data,
  form = "Training Log",
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password",
  option = sb_insert_event_option(
    id_col = "about"
  )
)
```

**Remember**: `id_col` can only be set to either “user_id”, “about”,
“username” or “email”.

### `table_field`

Smartabase tables are used when you want to store multiple rows of data
in one event. For example, they are prevalent in strength testing forms
where an athlete is performing multiple exercises in the same session.

#### Example

Here is some event data from a form that contains both table and
non-table fields:

``` r
strength_data <- sb_get_event(
  form = "Strength Testing",
  date_range = c("15/04/2023", end_date = "15/04/2023"),
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password",
  option = sb_get_event_option(
    user_key = "about",
    user_value = c("Aiden Thomas", "Jamie Anderson")
  )
)
```

``` r
strength_data
#> # A tibble: 6 × 8
#>   start_date user_id about      `Session RPE` Exercise  Load Repetition event_id
#>   <chr>        <dbl> <chr>              <dbl> <chr>    <dbl>      <dbl>    <dbl>
#> 1 15/04/2023   37204 Aiden Tho…             7 Bench P…   120          3  2381840
#> 2 15/04/2023   37204 Aiden Tho…            NA Bench P…   115          3  2381840
#> 3 15/04/2023   37204 Aiden Tho…            NA Bench P…   110          3  2381840
#> 4 15/04/2023   37201 Jamie And…             8 Back Sq…   170          3  2382033
#> 5 15/04/2023   37201 Jamie And…            NA Back Sq…   165          3  2382033
#> 6 15/04/2023   37201 Jamie And…            NA Back Sq…   185          3  2382033
```

Here `Exercise`, `Load` and `Repetition` are all table fields, whereas
`Session RPE` is not – it was only recorded once in the session.
`start_date`, `user_id`, `about` and `event_id` are metadata that will
always populate each row. You will notice that for each unique
start_date and user_id pairing, the non-table field (`Session RPE`) will
only populate the first row.

This structure is required whenever importing into forms with both table
and non-table fields. That is why the user must specify which columns
map to Smartabase table fields via `table_field`:

``` r
sb_insert_event(
  df = strength_data,
  form = "Strength Testing",
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password",
  option = sb_insert_event_option(
    table_field = c("Exercise", "Load", "Repetition")
  )
)
```

`table_field` takes a vector of column names.

#### Duplicate user_id/start_date when `table_field = NULL`

If `table_field = NULL` but duplicate `user_id`/`start_date` pairings
are detected, the data set will be split into a list of data frames.
Each element of the list, i.e., each data frame, contains a set of the
original data whereby there are no duplicate `user_id`/`start_date`
values internally. Each data set in the list is then imported via
individual API calls.

For example, let’s say we have the following data set

``` r
print(strength_data)
#> # A tibble: 6 × 5
#>   start_date user_id about          exercise     load
#>   <chr>        <dbl> <chr>          <chr>       <dbl>
#> 1 15/04/2023   37201 Jamie Anderson Back Squat    170
#> 2 15/04/2023   37201 Jamie Anderson Back Squat    165
#> 3 15/04/2023   37201 Jamie Anderson Back Squat    165
#> 4 15/04/2023   37204 Aiden Thomas   Bench Press   120
#> 5 15/04/2023   37204 Aiden Thomas   Bench Press   115
#> 6 16/04/2023   37204 Aiden Thomas   Bench Press   110
```

As you can see, Aiden Thomas has two records occurring on “15/04/2023”
while Jamie Anderson has three records occurring on the “15/04/2023”. If
we try to import this data while `table_field` is left empty,
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md)
will internally split the data like this

    #> $`1`
    #> # A tibble: 3 × 5
    #>   start_date user_id about          exercise     load
    #>   <chr>        <dbl> <chr>          <chr>       <dbl>
    #> 1 15/04/2023   37201 Jamie Anderson Back Squat    170
    #> 2 15/04/2023   37204 Aiden Thomas   Bench Press   120
    #> 3 16/04/2023   37204 Aiden Thomas   Bench Press   110
    #> 
    #> $`2`
    #> # A tibble: 2 × 5
    #>   start_date user_id about          exercise     load
    #>   <chr>        <dbl> <chr>          <chr>       <dbl>
    #> 1 15/04/2023   37201 Jamie Anderson Back Squat    165
    #> 2 15/04/2023   37204 Aiden Thomas   Bench Press   115
    #> 
    #> $`3`
    #> # A tibble: 1 × 5
    #>   start_date user_id about          exercise    load
    #>   <chr>        <dbl> <chr>          <chr>      <dbl>
    #> 1 15/04/2023   37201 Jamie Anderson Back Squat   165

The first data set contains the first unique `user_id`/`start_date`
pairing; the second data set contains the second unique
`user_id`/`start_date` pairing, and so on.

### `interactive_mode`

`interactive_mode = FALSE` suppresses progress messages that are
intended for human end-users. Set `interactive_mode = FALSE` when
running `smartabaseR` within automated pipelines.

## Miscellaneous

### start_date / end_date / start_time / end_time columns

When uploading data to Smartabase, the API requires metadata about when
the event started and stopped.
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md)
will first search for any columns in the data named “start_time” and
“end_time”. **Note: start_time and end_time values must be in h:mm AM or
h:mm PM format.** If those columns do not exist, `start_time` and
`end_time` will be set to the current time and one hour after the
current time, respectively.

Likewise,[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md)
will also search for any columns named “start_date” or “end_date”, which
**must have values formatted as dd/mm/YYYY**. If those columns do not
exist, both `start_date` and `end_date` will be set to the current date
(unless the difference between `start_time` and `end_time` spans
midnight, in which case the `end_date` will be set to the current date
plus one day).
