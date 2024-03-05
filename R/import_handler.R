


#' .import_handler
#'
#' Builds http requests to the Smartabase event export API
#'
#' Encapsulates generic tasks common to each import-related API call such as
#' building a valid payload/URL and throwing relevant messages.
#'
#' @param df data frame being imported to Smartabase
#' @param arg list of arguments passed from exposed import functions
#'
#' @returns tibble: Smartabase event or profile data
#'
#' @noRd
#'
#' @keywords internal
.import_handler <- function(df, arg) {
  if (is.null(arg$login)) {
    arg$login <- sb_login(
      url = arg$url,
      username = arg$username,
      password = arg$password,
      option = arg$option
    )
  }
  if (is.null(arg$endpoints)) {
    arg$endpoints <- .get_endpoint(
      login = arg$login,
      url = arg$url,
      username = arg$username,
      password = arg$password,
      interactive_mode = arg$interactive_mode,
      cache = arg$cache,
      env = arg$current_env,
      endpoints = NULL
    )
  }
  arg$entered_by_user_id <- arg$login$user$id
  arg$smartabase_url <- .build_import_url(arg)
  arg$dry_run <- FALSE
  arg$action <- "import"

  df <- df %>%
    dplyr::ungroup() %>%
    .insert_date_time(., arg$current_env) %>%
    .attach_user_id_to_df(., arg)

  arg$duplicate_date_user_id <- .detect_duplicate_date_user_id(df, arg)

  df_list <- .prepare_import_df(df, arg)
  arg$total_length_body <- length(df_list)

  if (!is.null(arg$option$interactive_mode)) {
    if (isTRUE(arg$option$interactive_mode)) {
      .generate_duplicate_date_user_id_msg(arg)
      .generate_import_confirmation(df_list, arg)
    }
  }
  df <- seq_len(length(df_list)) %>%
    purrr::map_df(
      ~ .import_df_list_element(
        df = df_list[[.x]],
        import_action = names(df_list)[[.x]],
        index = .x,
        arg
      )
    )
  .build_import_response_df(df, arg)
}

#' .import_df_list_element
#'
#' @noRd
#' @keywords internal
#' @returns A charcter vector containing the response
.import_df_list_element <- function(df, import_action, index, arg) {
  prog_vals <- .calculate_import_progress_vals(df, index, arg)
  if (isTRUE(arg$option$interactive_mode)) {
    progress_msg <- cli::cli_progress_message(
      .generate_import_progress_msg(import_action, prog_vals, arg)
    )
    id_name <- glue::glue("{import_action}_{prog_vals$ix}_progress_id")
    set_progress_id(id_name, progress_msg)
  }
  if (arg$type == "profile") {
    body <- .build_import_body(df, import_action, arg)
  } else {
    body <- list(
      events = .build_import_body(df, import_action, arg) %>% purrr::flatten()
    )
  }
  request <- .build_request(body, arg)
  response <- .make_request(request, arg)
  .build_import_element_response(
    response = response,
    df = df,
    import_action = import_action,
    index = index,
    arg = arg

  )
}

