test_that("Columns matching id_cols are converted to lowercase", {
  df <- tibble::tibble(
    About = 1:3,
    User_ID = 4:6,
    Username = 7:9,
    Value = 10:12
  )
  result <- .convert_id_names_to_lower(df)

  expect_named(result, c("about", "user_id", "username", "Value"))
})

test_that("Columns not in id_cols remain unchanged", {
  df <- tibble::tibble(
    Value = 1:3,
    Description = 4:6
  )
  result <- .convert_id_names_to_lower(df)

  expect_named(result, c("Value", "Description"))
})

test_that("Partial matches do not cause renaming", {
  df <- tibble::tibble(
    "What about me" = 1:3,
    "User_ID" = 4:6
  )
  result <- .convert_id_names_to_lower(df)

  expect_named(result, c("What about me", "user_id"))
})

test_that("No duplicate column names are created", {
  df <- tibble::tibble(
    About = 1:3,
    about = 4:6,
    User_ID = 7:9
  )
  expect_error(.convert_id_names_to_lower(df))
})

test_that("Handles empty data frames correctly", {
  df <- tibble::tibble()
  result <- expect_error(.convert_id_names_to_lower(df))
})

test_that("Handles data frames with no matching columns", {
  df <- tibble::tibble(
    Value = 1:3,
    Description = 4:6
  )
  result <- .convert_id_names_to_lower(df)

  expect_named(result, c("Value", "Description"))
})
