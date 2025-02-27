#' sb_select_metadata
#'
#' Returns vector of metadata variables present in data frame
#'
#' Once data has been pulled in from Smartabase, it is often desirable to
#' retain the metadata variables (e.g. about, start_time etc.) in the data
#' frame before pushing back to Smartabase. Rather than having to repeatedly
#' write out the vector of metadata variables you want to retain, this helper
#' function will retain any metadata variables present in a data frame; for
#' instance, when used in the select() function
#'
#' @param df data frame: data to be uploaded to Smartabase
#'
#' @returns Vector of metadata variable names
#' @export
#'
#' @examples
#' \dontrun{
#' example_df <- dplyr::tibble(
#'   about = c("Jamie Anderson", "Charlie Thompson"),
#'   start_date = c("14/02/2020", "14/02/2020"),
#'   form = "Hydration",
#'   `Body Weight pre training` = round(runif(2, 82, 92), 0),
#'   `Body Weight post training` = round(runif(2, 82, 92), 0),
#'   `Urine Colour` = round(runif(2, 1, 8), 0),
#'   end_date = c("14/02/2020", "14/02/2020")
#' )
#' example_df %>% select(sb_select_metadata(.))
#' select(example_df, sb_select_metadata(example_df))
#' }
sb_select_metadata <- function(df) {
  vars <- c(
    "about", "user_id", "form", "start_date", "end_date", "start_time",
    "end_time", "entered_by_user_id", "event_id", "uuid", "export",
    "synchronise"
  )

  vars[vars %in% names(df)]
}

#' .export_join_cols
#'
#' @noRd
#' @keywords internal
#' @returns A character vector
.export_join_cols <- function() {
  c(
    "document.id",
    "export_object",
    "record_number",
    "results_number"
  )
}


#' get_metadata_names
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `get_metadata_names()` was renamed to `sb_select_metadata()` to create a more consistent API.
#' @returns A dataframe
#' @keywords internal
#' @export
get_metadata_names <- function(df) {
  lifecycle::deprecate_warn(
    when = "0.0.0.9000",
    what = "get_metadata_names()",
    with = "sb_select_metadata()"
  )

  sb_select_metadata(df)
}


#' .build_url
#'
#' @param url Smartabase url e.g. 'example.smartabase.com/site'
#' @param end_point API endpoint
#'
#' @noRd
#' @keywords internal
#' @returns url string
.build_url <- function(arg) {
  base <- paste0(arg$url, "/api/v1")
  endpoint <- arg$endpoint
  params <- "informat=json&format=json"
  glue::glue("{base}/{endpoint}?{params}")
}


#' .validate_url
#'
#' @noRd
#'
#' @keywords internal
#'
#' @returns string: entered_by_user_id saved in .Renviron
.validate_url <- function(url) {
  if (!stringr::str_detect(url, "^https://")) {
    if (stringr::str_detect(url, "^http://")) {
      url <- stringr::str_replace(
        url,
        "^http://",
        "https://"
      )
    } else {
      url <- paste0("https://", url)
    }
  }
  url
}

#' .build_http_error_msg
#'
#' @noRd
#' @keywords internal
#' @returns A message
.build_http_error_msg <- function(response) {
  code <- response$status_code
  if (code == 400) {
    c(
      "!" = "400: can't process request.",
      "i" = "Did you forget to include any required parameters?"
    )
  } else if (code == 401) {
    c(
      "!" = "401: couldn't log {.field {arg$username}} into {.url {arg$url}}.",
      "i" = "Did you spell your .field username}/{.field password} correctly?"
    )
  } else if (code == 403) {
    c(
      "!" = "403: you're missing permissions to access something."
    )
  } else if (code == 404) {
    c(
      "!" = "404: you tried to access something that doesn't exist."
    )
  } else {
    c(
      "!" = "500: server error.",
      "i" = "Possible causes include:",
      "*" = "An invalid or misspelled URL",
      "*" = "Malformed filters/options",
      "*" = "Other unexpected server-side issues"
    )
  }
}


#' .build_request
#'
#' Builds up http request using httr2
#'
#' @returns httr2 request object
#' @noRd
#' @keywords internal
.build_request <- function(body, arg) {
  httr2::request(arg$smartabase_url) %>%
    httr2::req_body_json(body, auto_unbox = TRUE, null = "list") %>%
    httr2::req_auth_basic(
      username = arg$username,
      password = arg$password
    ) %>%
    httr2::req_user_agent("smartabaseR")
}

#' .make_request
#'
#' Calls the Smartabase API and returns the response
#'
#' @noRd
#' @keywords internal
#' @returns Smartabase API response
.make_request <- function(request, arg) {
  response <- request %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  if (httr2::resp_is_error(response)) {
    clear_progress_id()
    msg <- .build_http_error_msg(response)
    cli::cli_abort(msg, call = arg$current_env)
  }

  response_time <- response$headers$Date %>%
    as.POSIXct(., format = "%a, %d %b %Y %H:%M:%S") %>%
    as.numeric() * 1000 %>%
      purrr::set_names(glue::glue("{arg$action}_time"))

  response_list <- list(
    "response" = response,
    "request" = response$url,
    "http_method" = "POST",
    "http_status_code" = response$status_code
  )
  c(response_time, response_list)
}

#' .endpoint_names
#'
#' Helper to remember endpoints aliases
#'
#' @noRd
#' @keywords internal
#' @returns Aliases for Smartabase API endpoint names
.endpoint_names <- function() {
  c(
    "insert_profile",
    "get_event",
    "get_user",
    "get_group",
    "get_current",
    "get_group_names",
    "get_filtered_event",
    "delete_event",
    "get_profile",
    "insert_event",
    "synchronise_event"
  )
}

#' .validate_endpoints
#'
#' Throws error if returned endpoints are not as expected
#'
#' @noRd
#' @keywords internal
#' @returns Error msg
.validate_endpoints <- function(endpoints, url, call) {
  error_flag <- FALSE
  if (!any(class(endpoints) == "list")) {
    error_flag <- TRUE
  } else if (any(!.endpoint_names() %in% names(endpoints))) {
    error_flag <- TRUE
  }

  if (error_flag) {
    clear_progress_id()
    cli::cli_abort(
      c(
        "!" = "Could not retrieve endpoint names.",
        "i" = "Is {.url {url}} up and responsive?",
        "i" = "Is {.url {url}} on at least version 6.14.0 or greater?"
      ),
      call = rlang::caller_env()
    )
  }
}

#' .convert_unix_time_to_utc
#'
#' Takes unix time returned from `sb_sync_event()` and convert to UTC time
#'
#' @noRd
#' @keywords internal
#' @returns Error msg
.convert_unix_time_to_utc <- function(unix) {
  as.character(
    as.POSIXct(unix, origin = "1970-01-01", tz = "UTC"),
    usetz = TRUE
  )
}


#' .missing_field_pattern
#'
#' Helper that returns commonly used missing field message
#'
#' @noRd
#' @keywords internal
#' @returns Message
.missing_field_pattern <- function() {
  paste(
    "\\sThe following names\\s\\[[^\\]]+\\]\\swere",
    "sent in the message but do not exist in the form"
  )
}


#' save_credentials
#'
#' Helper that opens .Renviron file for storing credentials. Deprecated: only
#' used to support other deprecated exported functions like [pull_smartabase()]
#' and [push_smartabase()]
#'
#' @returns .Renviron
#' @export
#'
#' @examples
#' \dontrun{
#' save_credentials()
#' }
save_credentials <- function() {
  lifecycle::deprecate_warn(
    when = "0.0.0.9000",
    what = "save_credentials()",
    details = glue::glue(
      "save_credentials() was deprecated in favour of more explicit \\
      credential strategies. Please see \\
      {.code vignette(\"credentials\")}"
    )
  )
  # writes message similar to brows_github_path()
  message("('.Renviron') file opened.\n")
  message("Store your Smartabase credentials in four lines like\n")
  message("SB_USER = 'john.smith'\n")
  message("SB_PASS = 'examplePassword'\n")
  message("SB_ID = 12345\n")
  message("SB_URL = 'example.smartabase.com/site'\n")
  message("Note: make sure ('.Renviron') ends with a newline!\n")
  usethis::edit_r_environ()
}


#' .get_username
#'
#' Checks for username in .Renviron file or function call
#'
#' This function checks if a username was supplied. If not, it will look in
#' .Renviron file.
#'
#' @noRd
#' @keywords internal
#' @returns username
.get_username <- function(username = NULL) {
  if (is.null(username)) {
    username <- Sys.getenv("SB_USER")
    if (identical(username, "")) {
      stop(
        "'username' not supplied nor set via 'save_credentials()'",
        call. = FALSE
      )
    }
  }
  username
}


#' .get_password
#'
#' Checks for password in .Renviron file or function call
#'
#' This function checks whether password was supplied. If not, will look in
#' .Renviron file.
#'
#' @param password string: if not saved with 'save_credentials()', will default
#' to asking user by opening a masked password input, otherwise expects
#' password as a string
#'
#' @noRd
#' @keywords internal
#' @returns password
.get_password <- function(password = NULL) {
  if (is.null(password)) {
    password <- Sys.getenv("SB_PASS")
    if (identical(password, "")) {
      stop("'password' not supplied nor set via 'save_credentials()'",
        call. = FALSE
      )
    }
  }
  password
}


#' .get_url
#'
#' Checks for url in .Renviron file or function call
#'
#' This function checks if a url was supplied. If not, it will look in
#' .Renviron file.
#'
#' @param username string: if not saved with 'save_credentials()', expects
#' Smartabase url
#'
#' @noRd
#' @keywords internal
#' @returns url
.get_url <- function(url = NULL) {
  if (is.null(url)) {
    url <- Sys.getenv("SB_URL")
    if (identical(url, "")) {
      stop("'url' not supplied nor set via 'save_credentials()'",
        call. = FALSE
      )
    }
  }
  url
}

#' set_progress_id
#'
#' @noRd
#' @keywords internal
#' @returns a progress ID
set_progress_id <- function(msg_name, progress_id) {
  internal_env[[msg_name]] <- progress_id
}

#' get_progress_id
#'
#' @noRd
#' @keywords internal
#' @returns A message
get_progress_id <- function(msg_name) {
  internal_env[[msg_name]]
}

#' clear_progress_id
#'
#' @noRd
#' @keywords internal
#' @returns A message
clear_progress_id <- function() {
  ids <- ls(envir = internal_env)
  ids <- ids[stringr::str_detect(ids, pattern = "progress_id$")]

  purrr::walk(
    ids,
    ~ cli::cli_progress_done(
      id = get(.x, envir = internal_env)
    )
  )
  rm(list = ids, envir = internal_env)
}
