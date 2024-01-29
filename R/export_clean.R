

#' .clean_export
#'
#' Final steps to ready export for display to the user, including re-arranging
#' columns and converting columns from strings into their correct data types
#'
#' @param dat Data returned from Smartabase
#' @param id_data User data returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return data
.clean_export <- function(
    data,
    id_data,
    arg
) {
  options <- arg$option
  if (!is.null(options$include_user_data)) {
    if (isTRUE(options$include_user_data)) {
      data <- data %>% .join_user_data(., id_data, arg)
    }
  }
  if (options$guess_col_type) {
    data <- data %>%
      dplyr::select(-dplyr::any_of(c("start_time", "end_time"))) %>%
      readr::type_convert(col_types = readr::cols()) %>%
      dplyr::bind_cols(
        data %>% dplyr::select(dplyr::any_of(c("start_time", "end_time")))
      )
  }

  # if (!is.null(sync_time)) {
  #   data <- data %>%
  #     mutate(sync_time = sync_time) %>%
  #     tidyr::nest(data = -sync_time)
  # }
  #
  # if (!is.null(deleted_events)) {
  #   data <- data %>%
  #     bind_cols(deleted_events)
  # }
  data %>% dplyr::select(-dplyr::any_of("export_object"))
}





.clean_user_export <- function(data, include_all_cols) {
  if (nrow(data) == 0) return(data)
  if (isTRUE(include_all_cols)) {
    data <- .join_detailed_user_data(data)
  } else {
    data <- data %>%
      tidyjson::spread_all() %>%
      tibble::as_tibble()
  }

  data <- data %>%
    dplyr::select(
      -dplyr::all_of(.export_join_cols()),
      -dplyr::any_of("organisationalId")
    ) %>%
    dplyr::rename(
      user_id = .data$userId,
      first_name = .data$firstName,
      last_name = .data$lastName,
      email = .data$emailAddress,
      uuid = .data$uuid,
      middle_name = .data$middleName,
      known_as = .data$knownAs
    ) %>%
    tidyr::unite(
      col = "about",
      .data$first_name,
      .data$last_name,
      sep = " ",
      remove = FALSE
    ) %>%
    dplyr::relocate(
      .data$user_id,
      .data$about,
      .data$first_name,
      .data$last_name,
      .data$username,
      .data$email,
      .data$uuid
    )

  if (isFALSE(include_all_cols)) {
    data <- data %>%
      dplyr::select(
        .data$user_id,
        .data$about,
        .data$first_name,
        .data$last_name,
        .data$username,
        .data$email
      )
  }
  data
}

.clean_iam_data <- function(
    data,
    iam = c("group", "role"),
    user_type = c("athlete", "coach")
) {
  data <- data %>%
    tidyjson::enter_object("groupsAndRoles")

  if (iam == "group") {
    id_var <- glue::glue("{user_type}_group_id")
    name_var <- glue::glue("{user_type}_group_name")
    array_col <- glue::glue("{user_type}Groups")
    list_name <- glue::glue("{user_type}_group")
  } else {
    id_var <- "role_id"
    name_var <- "role_name"
    array_col <- "role"
    list_name <- "role"
  }

  data <- data %>%
    tidyjson::enter_object(!!array_col)

  if (nrow(data) > 0) {
    data <- data %>%
      tidyjson::gather_array(array_col) %>%
      tidyjson::spread_all() %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        !!dplyr::sym(id_var) := .data$id,
        !!dplyr::sym(name_var) := .data$name
      ) %>%
      dplyr::select(-dplyr::all_of(array_col)) %>%
      tidyr::nest(
        !!dplyr::sym(list_name) := c(
          !!id_var,
          !!name_var
        ))
  }
  data
}


.clean_phone_data <- function(data) {
  phone_df <- data %>%
    tidyjson::enter_object("phoneNumbers") %>%
    tidyjson::gather_array("phoneNumbers") %>%
    tidyjson::spread_all() %>%
    tibble::as_tibble()

  if (nrow(phone_df) > 0) {
    phone_df <- phone_df %>%
      tidyr::unite(
        col = "phone_number",
        .data$countryCode,
        .data$prefix,
        .data$number,
        sep = ""
      ) %>%
      dplyr::rename(phone_type = .data$type) %>%
      dplyr::mutate(
        dplyr::across(
          .data$phone_number,
          ~ dplyr::if_else(. == "", NA_character_, .)
        ),
        dplyr::across(
          .data$phone_type,
          ~ tolower(glue::glue("phone_{phone_type}")) %>% as.character(.)
        )
      ) %>%
      dplyr::select(-dplyr::any_of("phoneNumbers")) %>%
      dplyr::group_by(.data$results_number, .data$phone_type) %>%
      dplyr::mutate(phone_type_count = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::nest(phone = c(
        .data$phone_type_count,
        .data$phone_type,
        .data$phone_number
      ))
  }
  phone_df
}


.clean_address_data <- function(data) {
  address_df <- data %>%
    tidyjson::enter_object("addresses") %>%
    tidyjson::gather_array("addresses") %>%
    tidyjson::spread_all() %>%
    tibble::as_tibble()

  if (nrow(address_df) > 0) {
    address_df <- address_df %>%
      tidyr::unite(
        col = "address",
        .data$address,
        .data$suburb,
        .data$city,
        .data$country,
        .data$postcode,
        sep = " "
      ) %>%
      dplyr::rename(address_type = .data$type) %>%
      dplyr::mutate(
        dplyr::across(
          .data$address_type,
          ~ tolower(glue::glue("address_{address_type}")) %>% as.character(.)
          )
      ) %>%
      dplyr::select(-dplyr::any_of("addresses")) %>%
      dplyr::group_by(.data$address_type) %>%
      dplyr::mutate(address_type_count = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::nest(address = c(
        .data$address_type_count,
        .data$address_type,
        .data$address
      ))
  }

  address_df
}



#' .guess_col_type
#'
#' Write pulled data to temp folder and read back in with read_csv and utilise
#' col_guess()
#'
#' @param dat Data returned from Smartabase
#' @noRd
#' @keywords internal
#' @return data
.guess_col_type <- function(dat) {
  dat %>%
    readr::type_convert(col_types = readr::cols()) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::vars_select_helpers$where(lubridate::is.difftime),
        ~ format(strptime(., format = "%H:%M:%S"), "%I:%M %p")
      )
    )
}



#' .handle_null_rows
#'
#' @param data Data returned from Smartabase
#' @noRd
#' @keywords internal
#' @return data
.handle_null_rows <- function(data) {
  if (!"rows" %in% names(data)) {
    data <- NULL
  } else {
    data <- data %>%
      dplyr::filter(.data$rows != "NULL")

    if (nrow(data) == 0) {
      data <- NULL
    }
  }

  data
}


#' .rename_export_metadata
#'
#' Cleans metadata column names, largely converting camel case to snake case
#'
#' @param data Data returned from Smartabase
#' @param type Export type
#' @noRd
#' @keywords internal
#' @return tibble: cleaned metadata columns
.rename_export_metadata <- function(data, type) {
  if (type == "profile") {
    data %>%
      dplyr::rename(
        form               = .data$formName,
        user_id            = .data$userId,
        entered_by_user_id = .data$enteredByUserId,
        event_id           = .data$id
      )
  } else {
    data %>%
      dplyr::rename(
        form               = .data$formName,
        start_date         = .data$startDate,
        start_time         = .data$startTime,
        end_date           = .data$finishDate,
        end_time           = .data$finishTime,
        user_id            = .data$userId,
        entered_by_user_id = .data$enteredByUserId,
        event_id           = .data$id
      )
  }
}


#' .join_user_data
#'
#' Joins event data with user data and arranges columns
#'
#' @param data Data returned from Smartabase
#' @param id_data User data returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return tibble: user data joined with event/profile data
.join_user_data <- function(data, id_data, arg) {
  options <- arg$option
  if (arg$type != "profile") {
    first_metadata_cols <- c(
      "about",
      "user_id",
      "form",
      "start_date"
    )

    last_metadata_cols <- c(
      "start_time",
      "end_date",
      "end_time",
      "entered_by_user_id",
      "event_id",
      "file_name",
      "attachment_url",
      "name"
    )
  } else {
    first_metadata_cols <- c("about", "user_id", "form")
    last_metadata_cols <- c("entered_by_user_id", "event_id")
  }

  if (options$include_missing_user) {
    data <- data %>%
      dplyr::full_join(., id_data, by = "user_id")
  } else {
    data <- data %>%
      dplyr::left_join(., id_data, by = "user_id", copy = TRUE)
  }

  middle_metadata_cols <- data %>%
    dplyr::select(
      -dplyr::any_of(c(first_metadata_cols, last_metadata_cols))
    ) %>%
    colnames(.)

  col_order <- c(
    first_metadata_cols,
    middle_metadata_cols,
    last_metadata_cols
  )
  data %>% dplyr::select(dplyr::any_of(col_order))
}


.join_detailed_user_data <- function(data) {
  athlete_group_df <- .clean_iam_data(data, "group", "athlete")
  coach_group_df <- .clean_iam_data(data, "group", "coach")
  role_df <- .clean_iam_data(data, "role")
  phone_df <- .clean_phone_data(data)
  address_df <- .clean_address_data(data)

  detailed_list <- list(
    athlete_group_df,
    coach_group_df,
    role_df,
    phone_df,
    address_df
  )

  length_test <- detailed_list %>%
    purrr::map_lgl(~nrow(.) > 0) %>%
    any(isTRUE(.))

  if (length_test) {
    detailed_df <- detailed_list %>%
      purrr::discard(~nrow(.) == 0) %>%
      purrr::reduce(dplyr::full_join, by = .export_join_cols())
  } else {
    detailed_df <- NULL
  }

  data <- data %>%
    tidyjson::spread_all() %>%
    tibble::as_tibble()

  # data <- list(data, groups_roles, phone_address) %>%
  if (!is.null(detailed_df)) {
    data <- dplyr::full_join(data, detailed_df, by = .export_join_cols())
  }
  data
}


#' .replace_form
#'
#' Helper that inserts form name into data filter used in export functions
#'
#' @noRd
#' @keywords internal
#' @return list
.replace_form <- function(inner_list, new_form_value) {
  inner_list$formName <- new_form_value
  return(inner_list)
}


.insert_form_data_filter <- function(form, data_filter) {
  if (!is.null(data_filter)) {
    data_filter <- data_filter %>%
      purrr::map(~ .replace_form(.x, form))
  }
  data_filter
}

