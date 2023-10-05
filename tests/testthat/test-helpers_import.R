
test_that(".detect_duplicate_user_ids", {
  expect_error(
    .detect_duplicate_user_ids(
      tibble::tibble(
        about = c("Test", "Test")
      )
    )
  )
})

test_that("check .remove_protected_column_names ", {
  expect_warning(
    expect_equal(
      .remove_protected_column_names(
        tibble::tibble(
          about = "Test User",
          `First Name` = "Test",
          `Last Name` = "User"
        )
      ),
      tibble::tibble(
        about = "Test User"
      )
    )
  )
})

test_that("check .replace_na_with_empty_string  ", {
  expect_equal(
    .replace_na_with_empty_string(
      tibble::tibble(event_id = 123456, test = NA)
    ),
    tibble::tibble(event_id = "123456", test = "")
  )

  expect_equal(
    .replace_na_with_empty_string(
      tibble::tibble(test = NA)
    ),
    tibble::tibble(test = "")
  )
})


test_that("check .convert_id_names_to_lower", {
  expect_equal(
    .convert_id_names_to_lower(
      names_to_lower
    ),
    tibble::tibble(
      about = "test",
      user_id = "test",
      username = "test",
      email = "test",
      event_id = "test"
    )
  )
})

