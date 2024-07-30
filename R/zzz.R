sb_internal_env <- NULL
cache_env <- new.env()

#' .onLoad
#'
#' @noRd
#' @keywords internal
#' @returns NULL
.onLoad <- function(libname, pkgname) {
  sb_internal_env <<- new.env()

  # Save the original functions in sb_internal_env
  sb_internal_env$original_sb_get_user <- function(
    url,
    username,
    password,
    ...,
    filter = sb_get_user_filter(),
    option = sb_get_user_option()
  ) {
    rlang::check_dots_used()
    env <- rlang::current_env()
    .check_export_class(filter, option, env)
    arg <- list(
      url = .validate_url(url),
      username = username,
      password = password,
      filter = filter,
      option = option,
      type = "user",
      current_env = env,
      ...
    )
    .validate_filter_user_key(arg)

    if (!is.null(arg$dev_mode)) {
      if (isTRUE(arg$dev_mode)) {
        return(arg)
      }
    }
    .export_handler(arg)
  }

  sb_internal_env$original_sb_login <- function(
    url,
    username,
    password,
    ...,
    option = sb_login_option()) {
    if (isTRUE(option$interactive_mode)) {
      login_progress_id <- cli::cli_progress_message(
        "Logging {.field {username}} into {.field {url}}..."
      )
      set_progress_id("login_progress_id", login_progress_id)
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
        clear_progress_id()
        cli::cli_abort(
          glue::glue("{login$value$detailMessage}")
        )
      }
    }
    if (isTRUE(option$interactive_mode)) {
      clear_progress_id()
      cli::cli_alert_success(
        "Successfully logged {.field {username}} into {.field {url}}."
      )
    }

    login$cookie <- response$headers$`Set-Cookie`
    login$session_header <- response$headers$`session-header`
    login
  }

  # Apply memoization by default
  sb_enable_user_cache()
}
