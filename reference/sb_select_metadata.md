# sb_select_metadata

Returns vector of metadata variables present in data frame

## Usage

``` r
sb_select_metadata(df)
```

## Arguments

- df:

  data frame: data to be uploaded to Smartabase

## Value

Vector of metadata variable names

## Details

Once data has been pulled in from Smartabase, it is often desirable to
retain the metadata variables (e.g. about, start_time etc.) in the data
frame before pushing back to Smartabase. Rather than having to
repeatedly write out the vector of metadata variables you want to
retain, this helper function will retain any metadata variables present
in a data frame; for instance, when used in the select() function

## Examples

``` r
if (FALSE) { # \dontrun{
example_df <- dplyr::tibble(
  about = c("Jamie Anderson", "Charlie Thompson"),
  start_date = c("14/02/2020", "14/02/2020"),
  form = "Hydration",
  `Body Weight pre training` = round(runif(2, 82, 92), 0),
  `Body Weight post training` = round(runif(2, 82, 92), 0),
  `Urine Colour` = round(runif(2, 1, 8), 0),
  end_date = c("14/02/2020", "14/02/2020")
)
example_df %>% select(sb_select_metadata(.))
select(example_df, sb_select_metadata(example_df))
} # }
```
