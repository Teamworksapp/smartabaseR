


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
#' @return tibble: Smartabase event or profile data
#'
#' @noRd
#'
#' @keywords internal
.import_handler <- function(df, arg) {
  login <- .get_cached_login(
    url = arg$url,
    username = arg$username,
    password = arg$password,
    env = arg$current_env
  )
  arg$entered_by_user_id <- login$user$id
  arg$endpoints <- .get_cached_endpoint(
    login = login,
    url = arg$url,
    username = arg$username,
    password = arg$password,
    interactive_mode = arg$interactive_mode,
    cache = arg$cache,
    env = arg$current_env,
    endpoints = NULL
  )
  arg$smartabase_url <- .build_import_url(arg)
  arg$dry_run <- FALSE
  arg$action <- "import"

  df <- df %>%
    dplyr::ungroup() %>%
    .insert_date_time(., arg) %>%
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


.import_df_list_element <- function(df, import_action, index, arg) {
  prog_vals <- .calculate_import_progress_vals(df, index, arg)
  if (arg$option$interactive_mode) {
    cli::cli_progress_message(
      .generate_import_progress_msg(import_action, prog_vals, arg)
    )
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


.get_cached_import_data <- function(arg) {
  if (isTRUE(arg$option$cache)) {
    get_endpoint_fun <- .get_cached_endpoint
    login_fun <- .get_cached_login
  } else {
    get_endpoint_fun <- .get_endpoint
    login_fun <- sb_login
  }
  login <- login_fun(
    url = arg$url,
    username = arg$username,
    password = arg$password,
    env = arg$current_env
  )
  arg$entered_by_user_id <- login$user$id
  arg$endpoints <- get_endpoint_fun(
    login = login,
    url = arg$url,
    username = arg$username,
    password = arg$password,
    interactive_mode = arg$option$interactive_mode,
    cache = arg$cache,
    env = arg$current_env
  )
  arg
}
