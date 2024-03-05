test_that(".validate_filter_user_key", {
  fun_list <- list(
    sb_get_event_filter,
    sb_get_profile_filter,
    sb_get_event_filter
  )

  purrr::walk(fun_list, ~ expect_error(
    .x(
      user_key = c("about", "username"),
      user_value = "Jamie Anderson"
    )
  ))

  purrr::walk(fun_list, ~ expect_error(
    .x(
      user_key = "about1",
      user_value = "Jamie Anderson"
    )
  ))
})

test_that(".validate_filter_data_condition", {
  expect_error(
    sb_get_event_filter(
      data_condition = "about1"
    )
  )
})

test_that(".validate_filter_events_per_user", {
  expect_error(
    sb_get_event_filter(
      events_per_user = "one"
    )
  )
})
