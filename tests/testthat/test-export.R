test_that(".validate_filter_user_key", {
  expect_error(
    sb_get_event(
      dev_mode = T,
      form = "test",
      url = "test",
      username = "test",
      password = "test",
      date_range = c("01/01/2020", "01/01/2020"),
      option = sb_get_event_option(
        include_user_data = F
      ),
      filter = sb_get_event_filter(
        user_key = "about",
        user_value = "Zac Pross"
      )
    )
  )
})
