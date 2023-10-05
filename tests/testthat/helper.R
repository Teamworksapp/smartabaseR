###############################################################################
# .insert_metadata() validation data

insert_metadata_enteredby <- tibble::tibble(
  about = "Jamie Anderson",
  entered_by_user_id = 11111
)

insert_metadata_no_enteredby <- tibble::tibble(
  about = "Jamie Anderson"
)

insert_metadata_arg <- list()

###############################################################################
# Date time validation data

validate_date_and_time_fail <- tibble::tibble(
  start_date = c(
    "01-01-2021",
    "01/13/2021",
    "NA",
    "01/01/20021"
  ),
  start_time = c(
    "0000 AM",
    "13:00 PM",
    "01:00 SM",
    "01:100 AM"
  ),
  fail_type = c(
    "separator",
    "format",
    "ampm",
    "length"
  )
)

separator_fail <- validate_date_and_time_fail %>%
  dplyr::filter(fail_type == "separator")

format_fail <- validate_date_and_time_fail %>%
  dplyr::filter(fail_type == "format")

ampm_fail <- validate_date_and_time_fail %>%
  dplyr::filter(fail_type == "ampm")

length_fail <- validate_date_and_time_fail %>%
  dplyr::filter(fail_type == "length")

date_arg_fail_list <- list(
  start_date = "01/13/20201",
  end_date = "01012021",
  start_time = "0000 AM",
  end_time = "01:00 SM"
)

date_arg_success_list <- list(
  start_date = "01/01/2021",
  end_date = "01/01/2021",
  start_time = "01:00 AM",
  end_time = "01:15 PM"
)


###############################################################################
# helpers_push_smartabase

names_to_lower <- tibble::tibble(
  ABOUT = "test",
  USER_id = "test",
  UseRnAme = "test",
  EMAIL = "test",
  event_Id = "test"
)

metadata_list <- list(
  formName = "Test Form",
  startDate = "31/12/2021",
  finishDate = "31/12/2021",
  startTime = "01:00 AM",
  finishTime = "02:00 AM",
  enteredByUserId = 11111,
  userId = list(userId = 12345)
)

push_arg_event <- c(
  list(
    form = "Test Form",
    type = "event",
    get_id = FALSE,
    match_id_to_column = NULL,
    table_field = NULL,
    current_date_format = NULL,
    update_event = FALSE,
    cloud_mode = FALSE,
    shiny_progress_code = NULL
  ),
  metadata_list
)

push_arg_profile <- push_arg_event
push_arg_profile[["type"]] <- "profile"

push_arg_event_table <- push_arg_event
push_arg_event_table[["table_field"]] <- "table_field"

build_list_one_row_data <- c(
  metadata_list,
  list(
    rows = list(list(
      row = 0,
      pairs = list(list(
        key = "normal_field",
        value = "normal_row_1"
      ))
    ))
  )
)

build_list_multi_row_table <- c(
  metadata_list,
  list(
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          ),
          list(
            key = "table_field",
            value = "table_row_0"
          )
        )
      ),
      list(
        row = 1,
        pairs = list(
          list(
            key = "table_field",
            value = "table_row_1"
          )
        )
      )
    )
  )
)

same_user_different_dates_df <- tibble::tibble(
  user_id = c(12345, 12345),
  entered_by_user_id = c(11111, 11111),
  event_id = c(55555, 66666),
  start_date = c("01/01/2021", "02/01/2021"),
  end_date = c("01/01/2021", "02/01/2021"),
  start_time = c("01:00 AM", "01:00 AM"),
  end_time = c("02:00 AM", "02:00 AM"),
  normal_field = c("normal_row_0", "normal_row_0")
)


same_user_diff_dates_list <- list(
  list(
    formName = "Test Form",
    startDate = "01/01/2021",
    finishDate = "01/01/2021",
    startTime = "01:00 AM",
    finishTime = "02:00 AM",
    enteredByUserId = 11111,
    existingEventId = 55555,
    userId = list(userId = 12345),
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          )
        )
      )
    )
  ),
  list(
    formName = "Test Form",
    startDate = "02/01/2021",
    finishDate = "02/01/2021",
    startTime = "01:00 AM",
    finishTime = "02:00 AM",
    enteredByUserId = 11111,
    existingEventId = 66666,
    userId = list(userId = 12345),
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          )
        )
      )
    )
  )
)

same_user_diff_dates_pro_list <- list(
  list(
    formName = "Test Form",
    enteredByUserId = 11111,
    userId = list(userId = 12345),
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          )
        )
      )
    )
  ),
  list(
    formName = "Test Form",
    enteredByUserId = 11111,
    userId = list(userId = 12345),
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          )
        )
      )
    )
  )
)

diff_user_diff_dates <- tibble::tibble(
  user_id = c(12345, 12345, 98765, 98765),
  entered_by_user_id = c(11111, 11111, 11111, 11111),
  event_id = c(55555, 66666, 77777, 88888),
  start_date = c("01/01/2021", "02/01/2021", "01/01/2021", "02/01/2021"),
  end_date = c("01/01/2021", "02/01/2021", "01/01/2021", "02/01/2021"),
  start_time = c("01:00 AM", "01:00 AM", "01:00 AM", "01:00 AM"),
  end_time = c("02:00 AM", "02:00 AM", "02:00 AM", "02:00 AM"),
  normal_field = c(
    "normal_row_0", "normal_row_0", "normal_row_0", "normal_row_0"
  )
)

diff_user_diff_dates_list <- list(
  list(
    formName = "Test Form",
    startDate = "01/01/2021",
    finishDate = "01/01/2021",
    startTime = "01:00 AM",
    finishTime = "02:00 AM",
    enteredByUserId = 11111,
    existingEventId = 55555,
    userId = list(userId = 12345),
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          )
        )
      )
    )
  ),
  list(
    formName = "Test Form",
    startDate = "02/01/2021",
    finishDate = "02/01/2021",
    startTime = "01:00 AM",
    finishTime = "02:00 AM",
    enteredByUserId = 11111,
    existingEventId = 66666,
    userId = list(userId = 12345),
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          )
        )
      )
    )
  ),
  list(
    formName = "Test Form",
    startDate = "01/01/2021",
    finishDate = "01/01/2021",
    startTime = "01:00 AM",
    finishTime = "02:00 AM",
    enteredByUserId = 11111,
    existingEventId = 77777,
    userId = list(userId = 98765),
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          )
        )
      )
    )
  ),
  list(
    formName = "Test Form",
    startDate = "02/01/2021",
    finishDate = "02/01/2021",
    startTime = "01:00 AM",
    finishTime = "02:00 AM",
    enteredByUserId = 11111,
    existingEventId = 88888,
    userId = list(userId = 98765),
    rows = list(
      list(
        row = 0,
        pairs = list(
          list(
            key = "normal_field",
            value = "normal_row_0"
          )
        )
      )
    )
  )
)
