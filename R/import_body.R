#' .build_import_key_value
#'
#' Convert data frame to required JSON structure for Smartabase API upload#'
#' @noRd
#' @keywords internal
#' @returns Data to be uploaded to Smartabase - 'JSON'
.build_import_key_value <- function(df, n_row, arg) {
  remove_vars <- c(
    "user_id", "about", "form", "username", "email", "start_date", "end_date",
    "start_time", "end_time", "entered_by_user_id", "event_id"
  )
  df <- df %>% dplyr::select(-dplyr::any_of(remove_vars))

  table_field <- arg$option$table_field
  if (!is.null(table_field)) {
    .validate_table_field(df, table_field, arg$current_env)
    num_row <- n_row
  } else {
    num_row <- 1
  }

  if (num_row == 1) {
    list(
      row = 0,
      pairs = seq_len(ncol(df)) %>%
        purrr::map(
          ~ list(
            key = names(df[.x]),
            value = paste(df[.x][[1]][[n_row]])
          )
        )
    )
  } else {
    n_col <- ncol(df[names(df) %in% table_field])
    list(
      row = (num_row - 1),
      pairs = seq_len(n_col) %>%
        purrr::map(~ list(
          key = names(
            df[names(df) %in% table_field][.x]
          ),
          value = paste(
            df[names(df) %in% table_field][.x][[1]][[n_row]]
          )
        ))
    )
  }
}


#' .build_import_metadata
#'
#' Convert data frame to required JSON structure for Smartabase API upload#'
#' @noRd
#'
#' @keywords internal
#'
#' @returns Data to be uploaded to Smartabase - 'JSON'
.build_import_metadata <- function(df, n_row, import_action, arg) {
  if (arg$endpoint == "profileimport") {
    metadata <- dplyr::tibble(
      formName = arg[["form"]],
      enteredByUserId = as.numeric(arg$entered_by_user_id)
    ) %>%
      as.list()
  } else {
    metadata <- dplyr::tibble(
      formName = arg[["form"]],
      startDate = df[["start_date"]][[n_row]],
      finishDate = df[["end_date"]][[n_row]],
      startTime = df[["start_time"]][[n_row]],
      finishTime = df[["end_time"]][[n_row]],
      enteredByUserId = as.numeric(arg$entered_by_user_id)
    )

    if (import_action == "update") {
      if (!"event_id" %in% names(df)) {
        clear_progress_id()
        cli::cli_abort(
          "{.arg event_id} must exist as a column in the data frame when \\
          update_event = TRUE",
          call = arg$current_env
        )
      }
      metadata <- metadata %>%
        dplyr::mutate(
          existingEventId = as.numeric(df[["event_id"]][[n_row]])
        ) %>%
        as.list()
    }
  }
  user_id <- list(userId = list(
    userId = df[n_row, ] %>%
      dplyr::pull(.data$user_id) %>%
      as.numeric(.)
  ))
  append(metadata, user_id)
}

#' .append_metadata_key_value_pairs
#'
#' @noRd
#' @keywords internal
#' @returns JSON data to be uploaded to Smartabase
.append_metadata_key_value_pairs <- function(df, import_action, arg) {
  df <- .split_df_userid_date(df, arg)
  metadata <- df %>%
    purrr::map(
      ~ .build_import_metadata(
        df = .x,
        n_row = nrow(.x),
        import_action = import_action,
        arg = arg
      )
    )
  key_value_pairs <- df %>%
    purrr::map(function(x) {
      list(
        rows = seq_len(nrow(x)) %>%
          purrr::map(function(y) {
            .build_import_key_value(x, y, arg)
          })
      )
    })
  purrr::map2(metadata, key_value_pairs, ~ append(.x, .y))
}

#' .build_import_body
#'
#' @noRd
#' @keywords internal
#' @returns A list
.build_import_body <- function(df, import_action, arg) {
  if (arg$endpoint == "profileimport") {
    make_json_profile <- function(df) {
      .append_metadata_key_value_pairs(df, import_action, arg) %>%
        purrr::flatten()
    }
    return(
      df %>% dplyr::select(-.data$row_num) %>% make_json_profile()
    )
  }
  list(list(
    .append_metadata_key_value_pairs(
      df = df,
      import_action = import_action,
      arg = arg
    )
  )) %>% purrr::reduce(., append)
}
