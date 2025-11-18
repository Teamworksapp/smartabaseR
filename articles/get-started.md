# Get Started

### Load smartabaseR

``` r
library(smartabaseR)
```

## Requirements

`smartabaseR` can only be used on Smartabase versions **6.14** or
greater (if your account has `superadmin` or `site owner` privileges,
then `smartabaseR` can also be used with **v6.13**).

In 2023, Smartabase switched to [calendar
versioning](https://calver.org/). `smartabaseR` can be used on any site
that uses calendar versioning e.g. **2023.1**, **2023.2** etc.

### Credentials

Please see
[`vignette("credentials")`](https://teamworksapp.github.io/smartabaseR/articles/credentials.md).

### Exporting data from Smartabase

Exporting Smartabase data with `smartabaseR` is easy with the
[`sb_get_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_get_event.md)
function. You will at a minimum need to supply values to the `form`,
`date_range`, `url`, `username` and `password` arguments. Note that
`date_range` contains a `start_date` and an `end_date` and each must be
in dd/mm/YYYY format.

``` r
sb_get_event(
  form = "Training Log",
  date_range = c("01/03/2023", "07/03/2023"),
  url = "example.smartabase.com/site",
  username = "example.username", # note: must be valid username, not email
  password = "example_password"
)
```

    #> ✔ Successfully logged example.username into example.smartabase.com/site.
    #> ✔ User details export successful.
    #> ✔ Training Log export successful (Mar 03 2020 - Mar 07 2023).

A successful export! We have `Training Log` data for two athletes named
Aiden Thomas and Jamie Anderson.

    #> # A tibble: 2 × 7
    #>   start_date user_id about          distance   rpe event_id entered_by_user_id
    #>   <chr>        <dbl> <chr>             <dbl> <dbl>    <dbl>              <dbl>
    #> 1 03/01/2023   37204 Aiden Thomas       2530     5  2381840              37204
    #> 2 03/01/2023   37201 Jamie Anderson     5411     7  2382033              37201

**Note**, frequently exporting data (especially large data sets) can put
stress on your Smartabase server which may affect your organisation’s
Smartabase experience. At a minimum, please save your exported data to a
variable (here we saved it to a variable called “training_data”). This
minimises unnecessary API calls.

## Importing data to Smartabase

We can also import an R data frame into Smartabase using
[`sb_insert_event()`](https://teamworksapp.github.io/smartabaseR/reference/sb_insert_event.md).

Imagine we want to calculate the difference between each athlete’s RPE
and the team average RPE and upload the results to a form called “Team
Summary”

First, to calculate the team average:

``` r
# Need to load dplyr
library(dplyr)

rpe_average <- training_data %>%
  select(-entered_by_user_id) %>%
  mutate(team_rpe = round(mean(rpe), 1))

rpe_average
#> # A tibble: 2 × 7
#>   start_date user_id about          distance   rpe event_id team_rpe
#>   <chr>        <dbl> <chr>             <dbl> <dbl>    <dbl>    <dbl>
#> 1 03/01/2023   37204 Aiden Thomas       2530     5  2381840        6
#> 2 03/01/2023   37201 Jamie Anderson     5411     7  2382033        6
```

Now to compare the group average to each athlete’s RPE:

``` r
rpe_average <- rpe_average %>%
  mutate(rpe_diff = rpe - team_rpe)

rpe_average
#> # A tibble: 2 × 8
#>   start_date user_id about          distance   rpe event_id team_rpe rpe_diff
#>   <chr>        <dbl> <chr>             <dbl> <dbl>    <dbl>    <dbl>    <dbl>
#> 1 03/01/2023   37204 Aiden Thomas       2530     5  2381840        6       -1
#> 2 03/01/2023   37201 Jamie Anderson     5411     7  2382033        6        1
```

Finally, to upload our results back up to Smartabase, we need to supply
a data frame and a form name as well as our credentials:

``` r
sb_insert_event(
  df = rpe_average,
  form = "Team Summary",
  url = "example.smartabase.com/site",
  username = "example.username", # note: must be valid username, not email
  password = "example_password"
)
```

    #> ✔ SUCCESSFULLY_INSERTED: 2 out of 2 records successfully inserted into Team Summary.

Success! Our data should now be imported into the “Team Summary” form on
Smartabase.

To learn more about how to get the most out of `smartabaseR`, please see
the `exporting-data` and `importing-data` vignettes.
