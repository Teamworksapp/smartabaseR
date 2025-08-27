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
sb_login <- function(
    url,
    username,
    password,
    ...,
    option = sb_login_option()
) {
  if (isTRUE(option$interactive_mode)) {
    login_progress_id <- cli::cli_progress_message(
      "Logging {.field {username}} into {.field {url}}..."
    )
    set_progress_id("login_progress_id", login_progress_id)
  }
  rlang::check_dots_used()
  env <- rlang::current_env()

  arg <- list(
    url = .validate_url(url),
    username = username,
    password = password,
    option = option,
    endpoint = "user/loginUser",
    endpoint_type = "login",
    api_version = "v2",
    current_env = env,
    ...
  )
  if (!is.null(arg$dev_mode)) {
    if (isTRUE(arg$dev_mode)) {
      return(arg)
    }
  }
  cache_args <- arg[!names(arg) %in% c("current_env")]
  key <- paste0(serialize(cache_args, NULL), collapse = "")
  .sb_cache(
    key = key,
    expr = quote(.login_handler(arg)),
    cache = arg$option[["cache_login"]],
    cache_timeout = arg$option[["cache_login_timeout"]],
    cache_label = "sb_login",
    interactive_mode = arg$option[["cache_login"]]
  )
}


.login_handler <- function(arg) {
  body <- .build_login_body(arg)
  arg$smartabase_url <- .build_url(arg)
  arg$action <- "login"
  request <- .build_request(body, arg)
  response <- .make_request(request, arg)
  login <- httr2::resp_body_json(response$response)
  if (!is.null(login$`__is_rpc_exception__`)) {
    if (isTRUE(login$`__is_rpc_exception__`)) {
      clear_progress_id()
      cli::cli_abort(
        call = arg$current_env,
        glue::glue("{login$value$detailMessage}")
      )
    }
  }

  if (isTRUE(arg$option$interactive_mode)) {
    clear_progress_id()
    cli::cli_alert_success(
      "Successfully logged {.field {arg$username}} into {.field {arg$url}}."
    )
  }

  login$cookie <- response$response$headers$`Set-Cookie`
  login$session_header <- response$response$headers$`session-header`
  login
}


#' .build_license_audit_body
#'
#' @noRd
#' @keywords internal
#' @returns A [list()]
.build_login_body <- function(arg) {
  list(
    username = arg$username,
    password = arg$password,
    loginProperties = list(
      appName = basename(arg$url),
      clientTime = format(Sys.time(), "%Y-%m-%dT%H:%M")
    )
  )
}

