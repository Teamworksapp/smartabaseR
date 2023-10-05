
# smartabaseR: An R wrapper for the Smartabase API <img src="man/figures/logo.png" align="right" height="100" style="float:right; height:100px; width:100px">

<!-- badges: start -->

[![R-CMD-check](https://github.com/Teamworksapp/smartabaseR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Teamworksapp/smartabaseR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`smartabaseR` is an R package that lets you connect R to Smartabase. At
its core, `smartabaseR` acts as a wrapper for the Smartabase API.

`sb_get_event()` returns a flat export of your Smartabase data. From
there you can leverage R’s rich data wrangling, statistics and machine
learning capabilities.

`sb_insert_event()` sends data from R to Smartabase.

## Installation

You can install the development version of `smartabaseR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Teamworksapp/smartabaseR")
```

## Security

`smartabaseR` respects all the same permissions as if you were
interacting with the Smartabase web app or a Smartabase mobile app. For
example, if you don’t have access to certain form in Smartabase, then
you won’t have access to that form via `smartabaseR`. Likewise, if you
have delete access to a certain form in Smartabase, then you can also
delete data from that form with `smartabaseR`.

`smartabaseR` is a powerful tool. It can level up your automation and
analytics processes but there is potential for damage if you haven’t:

1.  read all the documentation
2.  undertaken extensive testing before putting into production
3.  reached out to your Smartabase consultant when/if you have any
    questions and/or concerns

## Getting started

`smartabaseR` provides some helper functions to setup various
credentials. You can see a vignette for explaining those below.

``` r
vignette("getting-started", package = "smartabaseR")
```

## Usage

The two main functions of `smartabaseR` allow you to export/import data
from/to your Smartabase site via the Smartabase API.

#### sb_get_event()

The `sb_get_event()` function allows you to export data from a specific
Smartabase form into an R session.

``` r
sb_get_event(
  form = "Example Form",
  date_range = c("01/03/2023", "07/03/2023"),
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

#### sb_insert_event()

The `sb_insert_event()` function allows you to send data back to a
Smartabase form.

``` r
sb_insert_event(
  df = data,
  form = "Example Form",
  url = "example.smartabase.com/site",
  username = "example.username",
  password = "example_password"
)
```

## Further Reading

Browse the vignettes below for in depth details on the workflow and for
details each function.

``` r
vignette("exporting-data", package = "smartabaseR")
vignette("importing-data", package = "smartabaseR")
vignette("helper-functions", package = "smartabaseR")
```
