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
#' @param env Environment value generally passed from other exported functions
#' calling `sb_login()`
#'
#' @return login object
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
sb_login <- function(
    url,
    username,
    password,
    ...,
    option = sb_login_option(),
    env = parent.frame()
) {
  if (isTRUE(option$interactive_mode)) {
    cli::cli_progress_message(
      "Logging {.field {username}} into {.field {url}}...",
      .envir = env
    )
  }
  url <- .validate_url(url)
  password <- password
  request_body <- list(
    username = username,
    password = password,
    loginProperties = list(
      appName = basename(url),
      clientTime = format(Sys.time(), "%Y-%m-%dT%H:%M")
    )
  )

  response <- httr2::request(
    glue::glue("{url}/api/v2/user/loginUser")
  ) %>%
    httr2::req_body_json(request_body, auto_unbox = TRUE, null = "list") %>%
    httr2::req_auth_basic(
      username = username,
      password = password
    ) %>%
    httr2::req_user_agent("smartabaseR") %>%
    httr2::req_headers(
      "X-GWT-Permutation" = "HostedMode",
      "session-header" = NULL
    ) %>%
    httr2::req_error(
      is_error = function(resp) httr2::resp_status(resp) == 401
    ) %>%
    httr2::req_perform()

  login <- httr2::resp_body_json(response)
  if (!is.null(login$`__is_rpc_exception__`)) {
    if (isTRUE(login$`__is_rpc_exception__`)) {
      cli::cli_abort(
        glue::glue("{login$value$detailMessage}")
      )
    }
  }
  if (isTRUE(option$interactive_mode)) {
    cli::cli_progress_done(result = "clear", .envir = env)
    cli::cli_alert_success(
      "Successfully logged {.field {username}} into {.field {url}}."
    )
  }

  login$cookie <- response$headers$`Set-Cookie`
  login$session_header <- response$headers$`session-header`
  login
}


#' .get_endpoint
#'
#' Gets endpoint names to be passed onto other functions. Saves us from
#' hardcoding the endpoint names into the package itself; rather requires a
#' login via `sb_login()` first
#'
#' @return Smartabase endpoint names
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
    endpoints = NULL
) {
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

