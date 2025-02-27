
test_that("Check for environment variables", {
  skip_on_cran()
  skip_on_ci()

  form <- Sys.getenv("TEST_FORM")
  url <- Sys.getenv("TEST_URL")
  username <- Sys.getenv("TEST_USERNAME")
  password <- Sys.getenv("TEST_PASSWORD")

  expect_true(nzchar(form), info = "form should not be empty")
  expect_true(nzchar(url), info = "url should not be empty")
  expect_true(nzchar(username), info = "username should not be empty")
  expect_true(nzchar(password), info = "password should not be empty")
})

# Delete any existing data in form
test_that("sb_delete_event() integration test", {
  skip_on_cran()
  skip_on_ci()
  date_range_value <- c("01/01/1900", "01/01/2050")

  export_data <- sb_get_event(
    form = Sys.getenv("TEST_FORM"),
    url = Sys.getenv("TEST_URL"),
    username = Sys.getenv("TEST_USERNAME"),
    password = Sys.getenv("TEST_PASSWORD"),
    date_range = date_range_value
  )

  expect_s3_class(export_data, "data.frame")

  if (nrow(export_data) > 0) {
    export_data$event_id %>%
      purrr::map(
        ~sb_delete_event(
          event_id = .x,
          form = Sys.getenv("TEST_FORM"),
          url = Sys.getenv("TEST_URL"),
          username = Sys.getenv("TEST_USERNAME"),
          password = Sys.getenv("TEST_PASSWORD")
        )
      )
  }
  # TODO sb_delete_event() should actually return a data.frame, until then will
  # need to just visually check the deletion has worked
})


# Insert initial dummy data
## Insert 24 rows of data, one for each hour of the day
test_that("sb_insert_event() integration test", {
  skip_on_cran()
  skip_on_ci()
  date_range_value <- c("01/01/2025", "01/01/2025")

  start_times <- lubridate::ymd_hm("2025-01-01 00:00") +
    lubridate::hours(0:23)
  end_times <- start_times + lubridate::minutes(59)
  start_times <- format(start_times, "%I:%M %p")
  end_times <- format(end_times, "%I:%M %p")

  dummy_data <- purrr::map2(
    start_times,
    end_times,
    ~tibble::tibble(start_time = .x, end_time = .y)) %>%
    purrr::reduce(rbind) %>%
    dplyr::mutate(
      user_id = 12024,
      start_date = "01/01/2025",
      `Body Weight pre training table` = 35,
      `Urine Colour` = "1"
    )

  insert_results <- sb_insert_event(
    df = dummy_data,
    form = Sys.getenv("TEST_FORM"),
    url = Sys.getenv("TEST_URL"),
    username = Sys.getenv("TEST_USERNAME"),
    password = Sys.getenv("TEST_PASSWORD")
  )

  expect_s3_class(insert_results, "data.frame")
  failed_rows <- insert_results %>%
    dplyr::filter(import_result != "SUCCESSFULLY_IMPORTED")
  expect_true(nrow(failed_rows) == 0)
})


# Update a single row
## Export all data, change one value, then update that value.
test_that("sb_update_event() integration test", {
  skip_on_cran()
  skip_on_ci()
  date_range_value <- c("01/01/2025", "01/01/2025")

  export_12am <- sb_get_event(
    form = Sys.getenv("TEST_FORM"),
    url = Sys.getenv("TEST_URL"),
    username = Sys.getenv("TEST_USERNAME"),
    password = Sys.getenv("TEST_PASSWORD"),
    date_range = date_range_value,
    # Also tests that 12:00 AM was correctly converted to 00:00 AM on insert
    time_range = c("12:00 AM", "12:59 AM")
  )

  expect_s3_class(export_12am, "data.frame")
  expect_true(nrow(export_12am) == 1)
  expect_true(export_12am$start_time[[1]] == "12:00 AM")

  update_body_weight <- export_12am %>%
    dplyr::mutate(`Body Weight pre training table` = 999)

  update_results <- sb_update_event(
    df = update_body_weight,
    form = Sys.getenv("TEST_FORM"),
    url = Sys.getenv("TEST_URL"),
    username = Sys.getenv("TEST_USERNAME"),
    password = Sys.getenv("TEST_PASSWORD"),
    option = sb_update_event_option(
      interactive_mode = FALSE
    )
  )

  expect_s3_class(update_results, "data.frame")
  failed_rows <- update_results %>%
    dplyr::filter(import_result != "SUCCESSFULLY_IMPORTED")
  expect_true(nrow(failed_rows) == 0)

  export_999_update <- sb_get_event(
    form = Sys.getenv("TEST_FORM"),
    url = Sys.getenv("TEST_URL"),
    username = Sys.getenv("TEST_USERNAME"),
    password = Sys.getenv("TEST_PASSWORD"),
    date_range = date_range_value,
    filter = sb_get_event_filter(
      data_key = "Body Weight pre training table",
      data_value = 999
    )
  )

  expect_s3_class(export_999_update, "data.frame")
  expect_true(nrow(export_999_update) == 1)
  expect_true(export_999_update$`Body Weight pre training table`[[1]] == 999)
})

# Upsert data
## Export all data, remove event_id from one row and change that value, then
## change all other values, then upsert. Result is that there should be one
## new record, and all other existing records should be updated.
test_that("sb_upsert_event() integration test", {
  skip_on_cran()
  skip_on_ci()
  date_range_value <- c("01/01/2025", "01/01/2025")

  export_data <- sb_get_event(
    form = Sys.getenv("TEST_FORM"),
    url = Sys.getenv("TEST_URL"),
    username = Sys.getenv("TEST_USERNAME"),
    password = Sys.getenv("TEST_PASSWORD"),
    date_range = date_range_value
  )

  expect_s3_class(export_data, "data.frame")
  export_999_value <- export_data %>%
    dplyr::filter(`Body Weight pre training table` == 999)
  expect_true(nrow(export_999_value) == 1)

  # Remove event_id from one value, which will thus become new record, also
  # change value to 123. Change all other values to 543, which will be updated.
  upsert_data <- export_data %>%
    dplyr::mutate(
      event_id = dplyr::if_else(
        `Body Weight pre training table` == 999,
        NA_integer_,
        event_id
      ),
      `Body Weight pre training table` = dplyr::if_else(
        is.na(event_id),
        123,
        543
      )
    )

  upsert_results <- sb_upsert_event(
    df = upsert_data,
    form = Sys.getenv("TEST_FORM"),
    url = Sys.getenv("TEST_URL"),
    username = Sys.getenv("TEST_USERNAME"),
    password = Sys.getenv("TEST_PASSWORD"),
    option = sb_update_event_option(
      interactive_mode = FALSE
    )
  )

  expect_s3_class(upsert_results, "data.frame")
  failed_rows <- upsert_results %>%
    dplyr::filter(import_result != "SUCCESSFULLY_IMPORTED")
  expect_true(nrow(failed_rows) == 0)

  check_upsert_results <- sb_get_event(
    form = Sys.getenv("TEST_FORM"),
    url = Sys.getenv("TEST_URL"),
    username = Sys.getenv("TEST_USERNAME"),
    password = Sys.getenv("TEST_PASSWORD"),
    date_range = date_range_value
  )

  unique_values <- check_upsert_results %>%
    dplyr::pull(`Body Weight pre training table`) %>%
    unique()

  expect_true(all(unique_values %in% c(123, 999, 543)))
})
