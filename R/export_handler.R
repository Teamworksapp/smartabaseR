#' .export_handler
#'
#' Builds http requests to the Smartabase event export API
#'
#' Encapsulates generic tasks common to each export-related API call such as
#' building a valid payload/URL and throwing relevant messages.
#'
#' @param arg list of arguments passed from exposed export functions
#'
#' @returns A tibble containing Smartabase event or profile data
#' @noRd
#' @keywords internal
.export_handler <- function(arg) {
  if (arg$endpoint %in% c("groupmembers", "usersearch", "currentgroup")) {
    id_data <- NULL
    user_id <- NULL
  } else {
    if (isTRUE(arg$option$include_user_data)) {
      id_data <- .get_user_id_for_export_body(arg)
      user_id <- id_data %>% dplyr::pull(.data$user_id)
    } else {
      id_data <- arg$filter$user_value
      user_id <- id_data
    }
  }

  arg$smartabase_url <- .build_url(arg)
  arg$dry_run <- FALSE
  arg$action <- "export"

  # Endpoints that support cursor-based pagination
  paginating_endpoints <- c("eventsearch", "filteredeventsearch", "synchronise")

  if (arg$endpoint %in% paginating_endpoints) {
    if (isTRUE(arg$option$interactive_mode)) {
      export_request_progress_id <- cli::cli_progress_message(
        "Requesting {arg$endpoint_type} data from Smartabase...",
        .envir = arg$current_env
      )
      set_progress_id("export_request_progress_id", export_request_progress_id)
    }
    .paginate_export(arg, user_id, id_data)
  } else {
    body    <- .build_export_body(arg, user_id)
    request <- .build_request(body, arg)

    if (isTRUE(arg$option$interactive_mode)) {
      export_request_progress_id <- cli::cli_progress_message(
        "Requesting {arg$endpoint_type} data from Smartabase...",
        .envir = arg$current_env
      )
      set_progress_id("export_request_progress_id", export_request_progress_id)
    }
    response <- .make_request(request, arg)

    if (isTRUE(arg$option$interactive_mode)) {
      export_wrangle_progress_id <- cli::cli_progress_message(
        "Wrangling {arg$endpoint_type} data from Smartabase...",
        .envir = arg$current_env
      )
      set_progress_id("export_wrangle_progress_id", export_wrangle_progress_id)
    }
    .json_to_df_handler(response, arg, id_data)
  }
}


#' .paginate_export
#'
#' Transparently fetches all pages of a paginated export endpoint and returns
#' a single combined tibble. Handles the three AMS endpoints that support
#' cursor-based pagination: `eventsearch`, `filteredeventsearch`, and
#' `synchronise`.
#'
#' For `synchronise` responses, `new_sync_time` is taken from the **last** page
#' (the most recent server timestamp), while `deleted_event_id` is accumulated
#' across all pages.
#'
#' **Known server-side limitation (eventsearch / filteredeventsearch):**
#' The `eventsearch` and `filteredeventsearch` cursors are date-only and use a
#' strict `date > cursor_date` comparison. This means any event whose
#' `startDate` exactly matches a page-boundary date can be silently skipped
#' (~1 event per page boundary). The `synchronise` endpoint uses a composite
#' cursor that does not have this issue. This is an AMS API bug — our
#' implementation is correct per the documented protocol. Once the server-side
#' cursor is fixed to use a `(date, id)` composite key, paginated and
#' non-paginated `eventsearch` counts will agree exactly.
#'
#' @param arg  Named list of arguments from [.export_handler()]
#' @param user_id Vector of user IDs for the request body
#' @param id_data User data tibble (or vector) used for data joining
#'
#' @returns A single `sb_df` tibble combining all pages
#' @noRd
#' @keywords internal
.paginate_export <- function(arg, user_id, id_data) {
  # Use a local copy of arg so we can suppress per-page success messages
  # while still emitting the final success message once after all pages.
  page_arg           <- arg
  page_arg$is_paging <- TRUE

  max_pages   <- if (!is.null(arg$option$max_pages)) arg$option$max_pages else 1000L
  pages       <- list()
  cursor      <- NULL
  last_cursor <- NULL
  page_n      <- 0L

  repeat {
    page_n <- page_n + 1L

    if (page_n > max_pages) {
      clear_progress_id()
      cli::cli_abort(
        c(
          "Pagination safety limit reached after {max_pages} pages of \\
           {.field {arg$form}} data.",
          "i" = "This may indicate an infinite-loop bug in the server cursor. \\
                 The data collected so far has been discarded."
        ),
        call = arg$current_env
      )
    }

    if (isTRUE(arg$option$interactive_mode) && page_n > 1L) {
      cli::cli_progress_message(
        "Fetching page {page_n} of {.field {arg$form}} data...",
        .envir = arg$current_env
      )
    }

    body        <- .build_export_body(arg, user_id, cursor = cursor)
    request     <- .build_request(body, arg)
    response    <- .make_request(request, arg)
    last_cursor <- cursor
    cursor      <- .extract_cursor(response, arg$endpoint)

    if (!is.null(cursor) && identical(cursor, last_cursor)) {
      clear_progress_id()
      cli::cli_abort(
        c(
          "Pagination cursor did not advance on page {page_n} of \\
           {.field {arg$form}} data.",
          "i" = "The server returned the same cursor twice in a row, which \\
                 would cause an infinite loop. The data collected so far has \\
                 been discarded."
        ),
        call = arg$current_env
      )
    }

    if (isTRUE(arg$option$interactive_mode)) {
      export_wrangle_progress_id <- cli::cli_progress_message(
        "Wrangling {arg$endpoint_type} data from Smartabase...",
        .envir = arg$current_env
      )
      set_progress_id("export_wrangle_progress_id", export_wrangle_progress_id)
    }

    page_result <- .json_to_df_handler(response, page_arg, id_data)
    pages       <- c(pages, list(page_result))

    if (is.null(cursor)) break
  }

  # Emit success message once, after all pages are fetched
  if (isTRUE(arg$option$interactive_mode)) {
    clear_progress_id()
    .generate_export_success_msg(arg)
  }

  if (length(pages) == 1L) return(pages[[1]])

  .combine_paginated_pages(pages, arg)
}


#' .combine_paginated_pages
#'
#' Combines a list of per-page `sb_df` tibbles into a single `sb_df` tibble,
#' re-applying attributes correctly.
#'
#' - All page data rows are combined via [dplyr::bind_rows()].
#' - Request metadata (status code, URL) is taken from the **last** page.
#' - For `synchronise`: `new_sync_time` comes from the last page; any
#'   `deleted_event_id` values are accumulated across all pages.
#'
#' @param pages Non-empty list of `sb_df` tibbles, one per page
#' @param arg   Named list of arguments from [.export_handler()]
#'
#' @returns A single `sb_df` tibble
#' @noRd
#' @keywords internal
.combine_paginated_pages <- function(pages, arg) {
  last_page <- pages[[length(pages)]]
  combined  <- dplyr::bind_rows(purrr::map(pages, tibble::as_tibble))

  class_type <- if (isTRUE(arg$option$interactive_mode)) {
    "sb_df"
  } else {
    "sb_df_non_interactive"
  }

  result <- tibble::new_tibble(
    combined,
    nrow             = nrow(combined),
    class            = class_type,
    request          = attr(last_page, "request"),
    http_method      = attr(last_page, "http_method"),
    http_status_code = attr(last_page, "http_status_code")
  )

  if (!is.null(arg$form)) {
    attr(result, "form") <- arg$form
  }
  attr(result, "export_time") <- attr(last_page, "export_time")

  if (arg$endpoint == "synchronise") {
    # Use the last page's sync time (most recent server timestamp)
    attr(result, "new_sync_time") <- attr(last_page, "new_sync_time")

    # Accumulate deleted event IDs from every page
    all_deleted <- purrr::map(pages, ~ attr(.x, "deleted_event_id")) %>%
      purrr::compact() %>%
      unlist(use.names = FALSE)
    if (length(all_deleted) > 0L) {
      attr(result, "deleted_event_id") <- all_deleted
    }
  }

  tibble::validate_tibble(result)
}


#' .json_to_df_handler
#'
#' Generic handler that passes response to the right conversion function
#' according to arg$endpoint
#'
#' @param response http response
#' @param arg List of arguments returned from parent function
#' @param id_data User data returned from Smartabase
#' @noRd
#' @keywords internal
#' @returns A tibble
.json_to_df_handler <- function(response, arg, id_data = NULL) {
  data <- .extract_content(response, arg)
  if (nrow(data) == 0) {
    clear_progress_id()
    return(new_sb_tibble(response, data, arg))
  }
  if (arg$endpoint %in% c("usersearch", "currentgroup", "groupmembers")) {
    data <- .convert_user_json_to_df(response, data, arg)
  } else if (arg$endpoint == "listgroups") {
    data <- .convert_group_json_to_df(response, data, arg)
  } else {
    data <- .convert_export_json_to_df(response, data, id_data, arg)
  }
  # Suppress per-page success message when called from inside .paginate_export()
  if (isTRUE(arg$option$interactive_mode) && !isTRUE(arg$is_paging)) {
    clear_progress_id()
    .generate_export_success_msg(arg)
  }
  data
}
