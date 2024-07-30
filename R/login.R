#'
#' @title
#' Log into Smartabase using credentials
#'
#' @description
#' `smartabaseR` uses this function internally to log the user in at the start
#' of each R session. We've exported this function since it can also be useful
#' when troubleshooting to ensure that your credentials are indeed valid.
#'
#' @param url Smartabase url e.g. "example.smartabase.com/site"
#' @param username Smartabase username
#' @param password Smartabase password
#' @param ... These dots are for future extensions and must be empty
#' @param option More options accessible via [sb_login_option()] object
#' calling `sb_login()`
#'
#' @returns login object
#' @export
#'
#' @examples
#' \dontrun{
#' sb_login(
#'   url = "example.smartabase.com/site",
#'   username = "john.smith",
#'   password = "example_password"
#' )
#' }
sb_login <- function(url, username, password, ..., option = sb_get_user_option()) {
  .sb_login(url, username, password, ..., option)
}


#' .get_endpoint
#'
#' Gets endpoint names to be passed onto other functions. Saves us from
#' hardcoding the endpoint names into the package itself; rather requires a
#' login via `sb_login()` first
#'
#' @returns Smartabase endpoint names
#' @noRd
#' @keywords internal
.get_endpoint <- function(
    login,
    url,
    username,
    password,
    interactive_mode,
    cache,
    env,
    endpoints = NULL) {
  if (!is.null(endpoints)) {
    return(endpoints)
  }
  response <- httr2::request(
    glue::glue("{url}/api/v3/endpoints?version=v1")
  ) %>%
    httr2::req_auth_basic(
      username = username,
      password = password
    ) %>%
    httr2::req_user_agent("smartabaseR") %>%
    httr2::req_headers(
      "X-GWT-Permutation" = "HostedMode",
      "session-header" = login$session_header,
      "Cookie" = glue::glue("JSESSIONID={login$session_header}")
    ) %>%
    httr2::req_perform()

  endpoints <- httr2::resp_body_json(response)
  .validate_endpoints(endpoints, url)
  endpoints
}
