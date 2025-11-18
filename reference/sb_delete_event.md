# Export event data

`sb_delete_event()` deletes data from a Smartabase event form using the
Smartabase API. It requires the user to input a valid Smartabase event
ID, url and credentials.

Please see
[`vignette("deleting-data")`](https://teamworksapp.github.io/smartabaseR/articles/deleting-data.md)
for more details.

## Usage

``` r
sb_delete_event(
  event_id,
  url,
  username,
  password,
  ...,
  option = sb_delete_event_option()
)
```

## Arguments

- event_id:

  IDs of Smartabase events to be deleted

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
  [`sb_delete_event_option()`](https://teamworksapp.github.io/smartabaseR/reference/sb_delete_event_option.md)
  object

## Value

Success/fail message

## Examples

``` r
if (FALSE) { # \dontrun{
# Delete one record with event_id = 999 from example.smartabase.com/site:
sb_delete_event(
  event_id = 999,
  url = "example.smartabase.com/site",
  username = "john.smith",
  password = "examplePassword"
)
} # }
```
