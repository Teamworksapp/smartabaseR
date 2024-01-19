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


#' .rename_metadata
#'
#' Cleans metadata column names, largely converting camel case to snake case
#'
#' @param data Data returned from Smartabase
#' @param type Export type
#' @noRd
#' @keywords internal
#' @return tibble: cleaned metadata columns
.rename_metadata <- function(data, type) {
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


#' .join_id_data
#'
#' Joins event data with user data and arranges columns
#'
#' @param data Data returned from Smartabase
#' @param id_data User data returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return tibble: user data joined with event/profile data
.join_id_data <- function(data, id_data, arg) {
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

  if (options$include_uuid) {
    last_metadata_cols <- c(last_metadata_cols, "uuid")
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


#' .extract_content
#'
#' Safely extracts event json from http response
#'
#' Uses tidyjson package which creates a tibble that stores Smartabase event
#' data in a single `..JSON` column
#'
#' @param response http response
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return tibble_json: event/profile data stored in json column
.extract_content <- function(response, arg) {
  data <- tryCatch(
    {
      httr2::resp_body_string(response$response) %>%
        tidyjson::gather_object("export_object")
    },
    error = function(e) {
      clear_progress_id()
      .generate_no_data_msg(arg)
      return(tibble::tibble())
    }
  )

  if (rlang::is_empty(data) || nrow(data) == 0) {
    clear_progress_id()
    .generate_no_data_msg(arg)
    return(tibble::tibble())
  }
  data
}


#' .try_tbl_json
#'
#' Safely constructs a tbl_json object for further downstream manipulation by
#' other tidyjson functions
#'
#' @param data Data returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return tidyjson::tbl_json
.try_tbl_json <- function(data, arg) {
  tryCatch(
    {
      data %>% tidyjson::as.tbl_json()
    },
    error = function(e) {
      return(tibble::tibble())
    })
}

#' .convert_id_json_to_df
#'
#' Takes Smartabase user json returned by Smartabase API and wrangles into a
#' tibble
#'
#' Uses tidyjson to pluck out the user data from the json returned by
#' Smartabase and then converts this to a tibble. Various details about the
#' export (e.g. export time) are then attached to the tibble as attributes
#'
#' @param response http response
#' @param json JSON returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return tibble: Smartabase user data
.convert_id_json_to_df <- function(response, json, arg) {
  json <- .try_tbl_json(json, arg)
  if (nrow(json) == 0) {
    clear_progress_id()
    .generate_no_data_msg(arg)
    return(data)
  }
  data <- .clean_user_export(json, arg)
  new_sb_tibble(response, data, arg)
}

#' .convert_group_json_to_df
#'
#' Takes Smartabase group json returned by Smartabase API and wrangles into a
#' tibble
#'
#' Uses tidyjson to pluck out the groups data from the json returned by
#' Smartabase and then converts this to a tibble. Various details about the
#' export (e.g. export time) are then attached to the tibble as attributes
#'
#' @param response http response
#' @param json JSON returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return tibble: Smartabase user data
.convert_group_json_to_df <- function(response, json, arg) {
  dat <- .try_tbl_json(json, arg)

  groups <- dat %>%
    tidyjson::gather_array("record_number") %>%
    dplyr::pull(.data$..JSON) %>%
    purrr::reduce(c)

  dat <- tibble::tibble(group = groups)
  if (nrow(dat) == 0) {
    clear_progress_id()
    .generate_no_data_msg(arg)
    return(tibble::tibble())
  }
  new_sb_tibble(response, dat, arg)
}


#' .json_to_df_handler
#'
#' Generic handler that passes response to the right conversion function
#' according to arg$type
#'
#' @param response http response
#' @param arg List of arguments returned from parent function
#' @param id_data User data returned from Smartabase
#' @noRd
#' @keywords internal
#' @return tibble
.json_to_df_handler <- function(response, arg, id_data = NULL) {
  json <- .extract_content(response, arg)
  if (nrow(json) == 0) {
    return(json)
  }
  if (arg$type == "user") {
    dat <- .convert_id_json_to_df(response, json, arg)
  } else if (arg$type == "group") {
    dat <- .convert_group_json_to_df(response, json, arg)
  } else {
    dat <- .convert_export_json_to_df(response, json, id_data, arg)
  }
  if (arg$option$interactive_mode) {
    .generate_export_success_msg(arg)
  }
  dat
}


#' .flatten_export_metadata
#'
#' Takes the metadata columns out of the tbl_json and converts into standalone
#' columns. The user entered data remains in the `..JSON` column. Each row
#' represents a unique event ID.
#'
#' @param json JSON returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return tibble
.flatten_export_metadata <- function(json, arg) {
  if (arg$type == "synchronise") {
    json <- json %>%
      dplyr::filter(.data$export_object == "export") %>%
      tidyjson::enter_object("events")
  }

  if (nrow(json) > 0) {
    json <- json %>%
      tidyjson::gather_array("record_number") %>%
      tidyjson::spread_all() %>%
      .rename_metadata(., arg$type)
  }
  json
}

#' .flatten_export_data
#'
#' Extracts user-entered data into a flat tibble structure
#'
#' @param nested_data Event data that has been nested for ease of use
#' @noRd
#' @keywords internal
#' @return tibble
.flatten_export_data <- function(nested_data) {
  nested_data %>%
    tidyjson::enter_object("rows") %>%
    tidyjson::gather_array("row_number") %>%
    tidyjson::enter_object("pairs") %>%
    tidyjson::gather_array("row_number_long") %>%
    tidyjson::spread_values(
      key = tidyjson::jstring("key"),
      value = tidyjson::jstring("value")
    ) %>%
    tibble::as_tibble(.) %>%
    dplyr::select(.data$record_number, .data$row_number,
                  dplyr::everything()) %>%
    dplyr::select(-dplyr::any_of(c(
      "document.id",
      "row_number_long",
      "events",
      "profile"
    ))) %>%
    dplyr::group_by(.data$record_number, .data$row_number) %>%
    tidyr::pivot_wider(names_from = .data$key, values_from = .data$value) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.data$record_number, .data$row_number))
}


#' .convert_export_json_to_df
#'
#' Takes Smartabase export json returned by Smartabase API and wrangles into a
#' tibble
#'
#' Uses tidyjson to pluck out the groups data from the json returned by
#' Smartabase and then converts this to a tibble. Various details about the
#' export (e.g. export time) are then attached to the tibble as attributes
#'
#' @param response http response
#' @param json JSON returned from Smartabase
#' @param id_data User data returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return tibble
.convert_export_json_to_df <- function(response, json, id_data, arg) {
  json <- .try_tbl_json(json, arg)
  if (nrow(json) == 0) {
    clear_progress_id()
    .generate_no_data_msg(arg)
    return(json)
  }

  if (arg$type == "synchronise") {
    arg <- .flatten_deleted_event_id(json, arg)
    arg <- .extract_new_sync_time(json, arg)
  }

  metadata <- .flatten_export_metadata(json, arg)
  options <- arg$option
  filters <- arg$filter

  if (nrow(metadata) == 0) {
    clear_progress_id()
    .generate_no_data_msg(arg)
    return(new_sb_tibble(response, tibble::tibble(), arg))
  }
  export_data <- .flatten_export_data(metadata)
  metadata <- metadata %>%
    dplyr::select(-dplyr::any_of(c(
      "document.id", "export_object", "record_number", "array.index"
    )))

  if (!arg$type %in% c("profile", "synchronise")) {
    if (arg$option$download_attachment) {
      export_data <- .process_attachment(
        response = response,
        nested_data = metadata,
        unnested_data = export_data,
        id_data = id_data,
        arg = arg
      )
    }
  }
  if (nrow(export_data) == 0) {
    export_data <- metadata %>% tibble::as_tibble()
  }
  export_data <- .clean_export(data = export_data, id_data, arg)
  new_sb_tibble(response, export_data, arg)
  # TODO: %>% .handle_null_rows(.)
}


#' .flatten_deleted_event_id
#'
#' Flattens the IDs of any deleted events returned from [`sb_sync_event()`]
#' since the `last_sync_time`
#'
#' @param json JSON returned from Smartabase
#' @param arg List of arguments returned from parent function
#' @noRd
#' @keywords internal
#' @return list
.flatten_deleted_event_id <- function(json, arg) {
  deleted_events <- json %>%
    dplyr::filter(.data$export_object == "idsOfDeletedEvents")

  if (nrow(deleted_events) == 0) {
    return(arg)
  }
    deleted_events <- deleted_events %>%
      tidyjson::gather_array("record_number") %>%
      dplyr::pull(.data$`..JSON`) %>%
      purrr::reduce(c)

    if (length(deleted_events) > 0) {
      arg$deleted_event_id <- deleted_events
    }
    arg
}


.extract_new_sync_time <- function(json, arg) {
  arg$new_sync_time <- json %>%
    dplyr::filter(.data$export_object == "lastSynchronisationTimeOnServer") %>%
    dplyr::pull(.data$`..JSON`) %>%
    purrr::pluck(1)

  if (length(arg$new_sync_time) == 0 || is.null(arg$new_sync_time)) {
    clear_progress_id()
    cli::cli_abort(
      "Expected {.field new_sync_time} but got {arg$new_sync_time}.",
      call = arg$current_env
    )
  }
  arg
}


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
      data <- data %>% .join_id_data(., id_data, arg)
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

.export_join_cols <- function() {
  c(
    "document.id",
    "export_object",
    "record_number",
    "results_number"
  )
}


.clean_user_export <- function(json, arg) {
  data <- json %>%
    tidyjson::gather_array("record_number") %>%
    tidyjson::enter_object("results") %>%
    tidyjson::gather_array("results_number")

  if (nrow(data) == 0) return(data)
  if (arg$option$user_info_level == "detailed") {
    groups_roles <- .clean_groups_roles(data)
    phone_address <- .clean_phone_address(data)

    data <- data %>%
      tidyjson::spread_all() %>%
      tibble::as_tibble()

    # data <- list(data, groups_roles, phone_address) %>%
    if (nrow(groups_roles) > 0) {
      data <- dplyr::full_join(
        data,
        groups_roles,
        by = c("document.id", "export_object", "record_number", "results_number")
      )
    }

    if (nrow(phone_address) > 0) {
      data <- dplyr::full_join(
        data,
        phone_address,
        by = c(
          "document.id",
          "export_object",
          "record_number",
          "results_number",
          "userId",
          "firstName",
          "lastName",
          "dob",
          "middleName",
          "emailAddress",
          "knownAs",
          "organisationalId",
          "username",
          "sex",
          "uuid"
        )
      )
    }

  } else {
    data <- data %>%
      tidyjson::spread_all() %>%
      tibble::as_tibble() # TODO: %>% .handle_null_rows(.)
  }

  data <- data %>%
    dplyr::select(-dplyr::all_of(.export_join_cols())) %>%
    dplyr::rename(
      user_id = .data$userId,
      first_name = .data$firstName,
      last_name = .data$lastName,
      email = .data$emailAddress,
      uuid = .data$uuid,
      middle_name = .data$middleName,
      known_as = .data$knownAs,
      organisational_id = .data$organisationalId
    ) %>%
    dplyr::relocate(
      .data$user_id,
      .data$first_name,
      .data$last_name,
      .data$email,
      .data$uuid
    )

  if (arg$option$user_info_level == "basic") {
    data <- data %>%
      dplyr::select(
        .data$user_id,
        .data$first_name,
        .data$last_name,
        .data$username,
        .data$email,
        .data$uuid
      ) %>%
      tidyr::unite(
        col = "about",
        .data$first_name,
        .data$last_name,
        sep = " ",
        remove = FALSE
      )
  }
  data
}

.clean_groups_roles <- function(data) {
  groups_roles_df <- data %>%
    tidyjson::enter_object("groupsAndRoles")

  coach_groups_df <- groups_roles_df %>%
    tidyjson::enter_object("coachGroups")

  if (nrow(coach_groups_df) > 0) {
    coach_groups_df <- coach_groups_df %>%
      tidyjson::gather_array("coachGroups") %>%
      tidyjson::spread_all() %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        coach_group_id = id,
        coach_group = name
      ) %>%
      dplyr::select(-coachGroups) %>%
      tidyr::nest(coach_group = c(coach_group_id, coach_group))
  }

  athlete_groups_df <- groups_roles_df %>%
    tidyjson::enter_object("athleteGroups")

  if (nrow(athlete_groups_df) > 0) {
    athlete_groups_df <- athlete_groups_df %>%
      tidyjson::gather_array("athleteGroups") %>%
      tidyjson::spread_all() %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        athlete_group_id = id,
        athlete_group = name
      )  %>%
      dplyr::select(-athleteGroups) %>%
      tidyr::nest(athlete_group = c(athlete_group_id, athlete_group))
  }

  if (nrow(coach_groups_df) > 0 || nrow(athlete_groups_df) > 0) {
    groups_roles_df <- list(
      coach_groups_df,
      athlete_groups_df
    ) %>%
      purrr::discard(~nrow(.) == 0) %>%
      purrr::reduce(
        dplyr::full_join,
        by = c("document.id", "export_object", "record_number", "results_number")
      )
  }
  groups_roles_df
}


.clean_phone_address <- function(data) {
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
      dplyr::mutate(
        dplyr::across(
          phone_number,
          ~ dplyr::if_else(. == "", NA_character_, .)
        ),
        dplyr::across(type, ~tolower(glue::glue("phone_{type}")))
      ) %>%
      dplyr::group_by(results_number, type) %>%
      dplyr::mutate(phoneNumbers = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::unite(col = "type", type, .data$phoneNumbers, sep = "_") %>%
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(.export_join_cols()),
        names_from = "type",
        values_from = "phone_number"
      )

    phone_cols <- phone_df %>%
      dplyr::select(-dplyr::any_of("phoneNumbers")) %>%
      dplyr::select(dplyr::contains("phone")) %>%
      names(.)

    phone_df <- phone_df %>%
      tidyr::nest(phone_number = dplyr::all_of(phone_cols))
  }

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
        postcode,
        sep = " "
      ) %>%
      dplyr::mutate(
        dplyr::across(type, ~tolower(glue::glue("address_{type}")))
      ) %>%
      tidyr::pivot_wider(
        id_cols = c("addresses", dplyr::all_of(.export_join_cols())),
        names_from = "type",
        values_from = "address"
      )

    address_cols <- address_df %>%
      dplyr::select(-dplyr::any_of("addresses")) %>%
      dplyr::select(dplyr::contains("address")) %>%
      names(.)

    address_df <- address_df %>%
      tidyr::nest(address = dplyr::all_of(address_cols)) %>%
      dplyr::select(-dplyr::any_of("addresses"))
  }

  data <- data %>%
    tidyjson::spread_all() %>%
    tibble::as_tibble()

  detailed_df <- dplyr::full_join(
    phone_df,
    address_df,
    by = .export_join_cols()
  )
  data <- dplyr::full_join(data, detailed_df, by = .export_join_cols())
  data
}


.insert_form_data_filter <- function(form, data_filter) {
  if (!is.null(data_filter)) {
    data_filter <- data_filter %>%
      purrr::map(~ .replace_form(.x, form))
  }
  data_filter
}


#' .get_user_id_for_export_body
#'
#' Populates user ID values for export filter
#'
#' Every call to the Smartabase event export API requires a list of athlete
#' user IDs. This function invokes [sb_get_user()] and caches the results
#'
#' @param arg List of arguments passed from export functions
#'
#' @return tibble: Smartabase user data
#'
#' @noRd
#'
#' @keywords internal
.get_user_id_for_export_body <- function(arg) {
  if (!arg$type %in% c("event", "profile", "synchronise")) {
    return()
  }

  if (!is.null(arg$pull_smartabase)) {
    if (isTRUE(arg$pull_smartabase)) {
      filter_fun <- .pull_smartabase_filter
      option_fun <- .pull_smartabase_option
    } else {
      filter_fun <- sb_get_user_filter
      option_fun <- sb_get_user_option
    }
  }

  id_filter_names <- intersect(names(arg$filter), names(filter_fun()))
  id_filters <- arg$filter[names(arg$filter) %in% id_filter_names]
  id_option_names <- intersect(names(arg$option), names(option_fun()))
  id_options <- arg$option[names(arg$option) %in% id_option_names]
  get_id_flag <- TRUE

  if (!is.null(id_options$include_user_data)) {
    if (isFALSE(id_options$include_user_data)) {
      if (!is.null(id_filters$user_key)) {
        if (id_filters$user_key == "user_id") {
          if (!is.null(id_filters$user_value)) {
            get_id_flag <- FALSE
          }
        }
      }
    }
  }

  if (isTRUE(get_id_flag)) {
    id_data <- sb_get_user(
      url = arg$url,
      username = arg$username,
      password = arg$password,
      filter = do.call(filter_fun, id_filters),
      option = do.call(option_fun, id_options),
      endpoints = arg$endpoints,
      login = arg$login
    ) %>%
      dplyr::select(-c(.data$username, .data$email)) %>%
      dplyr::distinct()

  } else {
    id_data <- tibble::tibble(user_id = id_filters$user_value)
  }
  id_data
}
