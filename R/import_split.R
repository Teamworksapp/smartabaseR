#' .split_df_userid_date
#'
#' Convert data frame to required JSON structure for Smartabase API upload
#' @noRd
#'
#' @keywords internal
#'
#' @returns Data to be uploaded to Smartabase - 'JSON'
.split_df_userid_date <- function(df, arg) {
  df <- df %>%
    split(.$user_id) %>%
    purrr::map(
      ~ .x %>%
        split(.$start_date) %>%
        purrr::map(~.x)
    )

  names(df) <- NULL
  df <- purrr::flatten(df)
  names(df) <- NULL
  df
}


#' .split_df_by_unique_date
#'
#' @noRd
#'
#' @keywords internal
#'
#' @returns A data frame
.split_df_by_unique_date <- function(df, import_action, arg) {
  # Create duplicate_row_id column which iterates the number of rows with same
  # user_id and start_date
  df %>%
    dplyr::group_by(.data$user_id, .data$start_date) %>%
    dplyr::mutate(duplicate_row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      dplyr::desc(lubridate::dmy(.data$start_date)), .data$user_id
    ) %>%
    split(.$duplicate_row_id) %>%
    purrr::map(~ dplyr::select(.x, -duplicate_row_id)) %>%
    purrr::set_names(import_action)
}

#' split_df_by_update_insert
#'
#' @noRd
#' @keywords internal
#' @returns A data frame
split_df_by_update_insert <- function(dat, arg) {
  if (arg$endpoint != "profileimport" && arg$update_event) {
    if ("event_id" %in% names(dat)) {
      dat <- dat %>%
        dplyr::mutate(event_id_flag = dplyr::if_else(
          is.na(.data$event_id), 1, 0
        )) %>%
        split(.$event_id_flag) %>%
        purrr::discard(~ nrow(.x) == 0) %>%
        purrr::map(~ remove_event_id_if_na(.x) %>%
          dplyr::select(-.data$event_id_flag))

      list_names <- dat %>%
        purrr::map(~ dplyr::if_else(
          "event_id" %in% names(.x), "update", "insert"
        ))

      dat <- dat %>% purrr::set_names(list_names)
      return(dat)
    }
  }
  list("insert" = dat)
}

#' remove_event_id_if_na
#'
#' @noRd
#' @keywords internal
#' @returns A data frame
remove_event_id_if_na <- function(dat) {
  length_is_na <- dat %>%
    dplyr::filter(!is.na(.data$event_id)) %>%
    nrow(.)
  if (length_is_na == 0) {
    dat %>% dplyr::select(-.data$event_id)
  } else {
    dat
  }
}

#' .split_import_df
#'
#' @noRd
#' @keywords internal
#' @returns A data frame
.split_import_df <- function(df, arg) {
  if (arg$endpoint == "profileimport") {
    df <- df %>%
      dplyr::mutate(row_num = dplyr::row_number()) %>%
      split(.$row_num) %>%
      purrr::set_names("upsert")
    return(df)
  }
  df <- split_df_by_update_insert(df, arg)

  if (!is.null(arg$duplicate_date_user_id)) {
    if (isTRUE(arg$duplicate_date_user_id) && is.null(arg$option$table_field)) {
      import_action <- names(df)
      df <- seq_len(length(df)) %>%
        purrr::map(
          ~ .split_df_by_unique_date(df[[.x]], import_action[[.x]], arg)
        ) %>%
        purrr::flatten()
    }
  }
  df
}
