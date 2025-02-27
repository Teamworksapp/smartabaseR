source("helper.R")

current_datetime_list <- .get_current_date_time()
current_datetime <- as.data.frame(current_datetime_list)

test_that("Check .get_current_date_time() produces list", {
  expect_type(current_datetime_list, "list")
})

test_that("Check .insert_date_time() works", {
  expect_equal(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson"),
      current_datetime = current_datetime
    ),
    tibble::tibble(
      about = "Jamie Anderson",
      start_time = current_datetime[["start_time"]],
      start_date = current_datetime[["start_date"]],
      end_time = current_datetime[["end_time"]],
      end_date = current_datetime[["end_date"]]
    )
  )

  expect_equal(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          start_date = "01/09/2021",
          end_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    ),
    tibble::tibble(
      about = "Jamie Anderson",
      start_time = "1:00 am",
      start_date = "01/09/2021",
      end_time = "2:00 am",
      end_date = "01/09/2021"
    )
  )

  # Error with end_date but no start_date
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          end_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with end_time but no start_time
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          end_time = "2:00 am",
          start_date = "01/09/2021",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with no colon in start_time
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1-00 am",
          start_date = "01/09/2021",
          end_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with no colon in end_time
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          end_time = "1-00 am",
          start_date = "01/09/2021",
          start_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with start_time length
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:0000 am",
          start_date = "01/09/2021",
          end_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with end_time length
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          start_date = "01/09/2021",
          end_time = "2:0000 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )


  # Error with start_date length
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:000 am",
          start_date = "01/09/20210",
          end_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with end_time length
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          start_date = "01/09/2021",
          end_time = "2:000 am",
          end_date = "01/09/20210"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with start_date type Date
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          start_date = lubridate::dmy("01/09/2021"),
          end_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with start_date type Date
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          start_date = "01/09/2021",
          end_time = "2:00 am",
          end_date = lubridate::dmy("01/09/2021")
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with start_date type character
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          start_date = 1,
          end_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with end_date type character
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          start_date = "01/09/2021",
          end_time = "2:00 am",
          end_date = 1
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with start_date hour
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "13:00 am",
          start_date = "01/09/2021",
          end_time = "2:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )

  # Error with end_date type
  expect_error(
    .insert_date_time(
      df = tibble::tibble(about = "Jamie Anderson") %>%
        dplyr::mutate(
          start_time = "1:00 am",
          start_date = "01/09/2021",
          end_time = "22:00 am",
          end_date = "01/09/2021"
        ),
      current_datetime = current_datetime,
      env = rlang::current_env()
    )
  )
})


test_that("Check .convert_12am_time() converts 12 AM to 00 AM", {
  df <- tibble::tibble(
    start_time = "12:00 AM",
    end_time = "12:30 AM"
  ) %>%
    .convert_12am_time(.)

  expect_equal(df$start_time[[1]], "00:00 AM")
  expect_equal(df$end_time[[1]], "00:30 AM")

  df <- tibble::tibble(
    start_time = "12:00 PM",
    end_time = "12:30 PM"
  ) %>%
    .convert_12am_time(.)

  expect_equal(df$start_time[[1]], "12:00 PM")
  expect_equal(df$end_time[[1]], "12:30 PM")
})
