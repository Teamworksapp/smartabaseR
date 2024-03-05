test_that("user export cleaning functions", {
  user_export <- user_json %>%
    tidyjson::gather_object("export_object") %>%
    .try_tbl_json(.)

  flat_user_export <- .flatten_user_export_data(user_export)
  athlete_group_df <- .clean_iam_data(flat_user_export, "group", "athlete")
  coach_group_df <- .clean_iam_data(flat_user_export, "group", "coach")
  role_df <- .clean_iam_data(flat_user_export, "role")
  phone_df <- .clean_phone_data(flat_user_export)
  address_df <- .clean_address_data(flat_user_export)

  user_data_detailed <- .join_detailed_user_data(flat_user_export)
  user_data_clean <- .clean_user_export(
    flat_user_export,
    include_all_cols = FALSE
  )

  expect_s3_class(flat_user_export, "tbl_json")
  expect_type(flat_user_export, "list")
  expect_equal(ncol(flat_user_export), 5)
  expect_equal(nrow(flat_user_export), 1)

  expect_s3_class(athlete_group_df, "tbl_df")
  expect_equal(ncol(athlete_group_df), 5)
  expect_equal(nrow(athlete_group_df), 1)
  expect_equal(athlete_group_df$athlete_group, user_data_joined$athlete_group)

  expect_s3_class(coach_group_df, "tbl_df")
  expect_equal(ncol(coach_group_df), 5)
  expect_equal(nrow(coach_group_df), 1)
  expect_equal(coach_group_df$coach_group, user_data_joined$coach_group)

  expect_s3_class(role_df, "tbl_df")
  expect_equal(ncol(role_df), 5)
  expect_equal(nrow(role_df), 1)
  expect_equal(role_df$role, user_data_joined$role)

  expect_s3_class(phone_df, "tbl_df")
  expect_equal(ncol(phone_df), 5)
  expect_equal(nrow(phone_df), 1)
  expect_equal(phone_df$phone, user_data_joined$phone)

  expect_s3_class(address_df, "tbl_df")
  expect_equal(ncol(address_df), 5)
  expect_equal(nrow(address_df), 1)
  expect_equal(address_df$address, user_data_joined$address)

  expect_s3_class(user_data_detailed, "tbl_df")
  expect_equal(ncol(user_data_detailed), 20)
  expect_equal(nrow(user_data_detailed), 1)
  expect_equal(user_data_detailed, user_data_joined)
})
