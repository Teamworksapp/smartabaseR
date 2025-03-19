#' @title
#' Export license audit data
#'
#' @description
#' `r lifecycle::badge("experimental")` Download license audit. Requires login produced `sb_login()`.
#'
#' @param login Produced by `sb_login()`
#' @param url Smartabase url e.g. "example.smartabase.com/site"
#' @param username Smartabase username
#' @param password Smartabase password
#' @param ... These dots are for future extensions and must be empty
#' @return tibble: license audit data
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "example.smartabase.com/example"
#' username <- "<admin_username>"
#' password <- "<admin_password>"
#'
#' login <- sb_login(
#'   url = url,
#'   username = username,
#'   password = password
#' )
#'
#' license_audit <- sb_get_license_audit(
#'   login = login,
#'   url = url,
#'   username = username,
#'   password = password
#' )
#'
#' print(license_audit)
#' }

sb_get_license_audit <- function(login, url, username, password, ...) {
  lifecycle::signal_stage("experimental", "sb_get_license_audit()")
  path <- "/api/v2/membership/downloadLicenseAudit"
  url <- .validate_url(glue::glue("{url}{path}"))
  payload <- list(
    "username" = username,
    "password" = password,
    "clientTimeOffset" = "0",
    "__rpc_method_signature__" = "downloadLicenseAudit"
  )
  response <- httr2::request(url) %>%
    httr2::req_headers(
      "Cookie" = login$cookie,
      "session-header" = login$session_header,
      "X-GWT-Permutation" = "6DA389D10C28639EE3223A5A5FAB15CA"
    ) %>%
    httr2::req_body_json(payload) %>%
    httr2::req_perform()

  cookie <- response$headers$`session-header`
  content <- response %>% httr2::resp_body_json()
  file_url <- glue::glue("{content$value$url}&token={login$user$skypeName}")

  license_audit_resp <- httr2::request(file_url) %>%
    httr2::req_headers(
      "Cookie" = login$cookie,
      "session-header" = login$session_header
    ) %>%
    httr2::req_perform()

  httr2::resp_body_string(license_audit_resp) %>%
    readr::read_csv()
}
