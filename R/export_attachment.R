
#' .clean_attachment
#'
#' @noRd
#' @keywords internal
#' @return data
.build_attachment_metadata <- function(nested_data, id_data) {
  tryCatch({
    nested_data %>%
      tidyjson::enter_object("attachmentUrl") %>%
      tidyjson::gather_array() %>%
      tidyjson::spread_all() %>%
      dplyr::left_join(., id_data, by = "user_id") %>%
      dplyr::rename(attachment_url = .data$attachmentUrl) %>%
      dplyr::filter(!is.na(.data$attachment_url)) %>%
      tidyr::unite(
        col = "file_name",
        .data$form,
        .data$about,
        .data$start_date,
        .data$start_time,
        .data$name,
        remove = FALSE
      ) %>%
      dplyr::select(-.data$about) %>%
      dplyr::mutate_at("file_name", ~ stringr::str_remove_all(., "/")) %>%
      dplyr::mutate_at("file_name", ~ stringr::str_remove_all(., ":| "))
  }, error = function(e) {
    tibble::tibble()
  })
}


#' .process_attachment
#'
#' @noRd
#' @keywords internal
#' @return data
.process_attachment <- function(
    response,
    nested_data,
    unnested_data,
    id_data,
    arg
) {
  attachment_metadata <- .build_attachment_metadata(nested_data, id_data)
  url_flag <- !"attachment_url" %in% names(attachment_metadata)
  if (nrow(attachment_metadata) == 0 || url_flag) {
    clear_progress_id()
    cli::cli_alert_warning("No attachment found in event data")
    return(unnested_data)
  }
  arg$new_env <- rlang::current_env()
  attachment_response <- .get_export_attachment(
    response = response,
    attachment_metadata = attachment_metadata,
    arg = arg
  )
  null_response <- is.null(attachment_response)
  attachment_error <- any(class(attachment_response) == "error")
  if (null_response || attachment_error) {
    clear_progress_id()
    cli::cli_alert_warning("Could not download attachment")
    return(unnested_data)
  }
  .join_attachment_metadata(attachment_metadata, nested_data, unnested_data)
}


#' .join_attachment_metadata
#'
#' @noRd
#' @keywords internal
#' @return data
.join_attachment_metadata <- function(
    attachment_metadata,
    nested_data,
    unnested_data
) {
  dat <- dplyr::full_join(
    nested_data,
    attachment_metadata,
    by = intersect(
      sb_select_metadata(nested_data),
      sb_select_metadata(attachment_metadata)
    )
  )
  dat <- dplyr::full_join(
    dat,
    unnested_data,
    by = intersect(
      sb_select_metadata(dat),
      sb_select_metadata(unnested_data)
    )
  )
  dat
}



#' .get_export_attachment
#'
#' @noRd
#' @keywords internal
#' @return data
.get_export_attachment <- function(response, attachment_metadata, arg) {
  if (arg$option$interactive_mode) {
    attachment_progress_id <- cli::cli_progress_message(
      .envir = arg$new_env,
      "Exporting {nrow(attachment_metadata)} attachment{?s} from {arg$form}..."
    )
    set_progress_id("attachment_progress_id", attachment_progress_id)
  }

  attachment_response <- attachment_metadata %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    split(.$row_num) %>%
    purrr::map(
      ~ tryCatch({
        httr2::request(.x[["attachment_url"]]) %>%
          httr2::req_options(cookie = paste0(
            "JSESSIONID=", response$response$headers$`session-header`
          )) %>%
          httr2::req_auth_basic(
            username = arg$username,
            password = arg$password
          ) %>%
          httr2::req_user_agent("smartabaseR") %>%
          httr2::req_perform(path = .x[["file_name"]])
      },
      error = function(e) {
        e
      })
    )

  attachment_response %>%
    purrr::map(~ .generate_attachment_message(attachment_response = .x, arg))

  attachment_response
}
