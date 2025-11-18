# Log into Smartabase using credentials

`smartabaseR` uses this function internally to log the user in at the
start of each R session. We've exported this function since it can also
be useful when troubleshooting to ensure that your credentials are
indeed valid.

## Usage

``` r
sb_login(url, username, password, ..., option = sb_login_option())
```

## Arguments

- url:

  Smartabase url e.g. "example.smartabase.com/site"

- username:

  Smartabase username

- password:

  Smartabase password

- ...:

  These dots are for future extensions and must be empty

- option:

  More options accessible via
  [`sb_login_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_login_option.md)
  object calling `sb_login()`

## Value

login object

## Examples

``` r
if (FALSE) { # \dontrun{
sb_login(
  url = "example.smartabase.com/site",
  username = "john.smith",
  password = "example_password"
)
} # }
```
