# Generates a date range relative to a supplied end date.

This function generates a date range based on a specified duration
relative to a supplied end date, where the default end date is today's
date. It returns a vector of length two containing the start date and
end date in "dd/mm/YYYY" strings.

For more details see the help vignette:
[`vignette("exporting-data")`](https://teamworksapp.github.io/smartabaseR/articles/exporting-data.md)

## Usage

``` r
sb_date_range(
  duration_value,
  duration_unit = c("days", "weeks", "months", "years"),
  end_date = format(lubridate::today(), "%d/%m/%Y")
)
```

## Arguments

- duration_value:

  Number of days, weeks, months or years from end date.

- duration_unit:

  Either "days", "weeks", "months" or "years". Defaults to "days".

- end_date:

  Must be a string in "dd/mm/YYYY" format. Defaults to today's date.

## Value

A vector containing a start date and end date as strings in "dd/mm/YYYY"
format.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate date range for the last 7 days
sb_date_range(duration_value = "7", duration_unit = "days")

# Generate date range for one year ending on "01/06/2023"
sb_date_range("1", "years", end_date = "01/06/2023")
} # }
```
