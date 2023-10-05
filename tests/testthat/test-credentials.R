test_that("Check credentials functions can retrieve environment variables", {
  Sys.setenv("SB_USER" = "test.user")
  Sys.setenv("SB_PASS" = "test123")
  Sys.setenv("SB_ID" = 12345)
  Sys.setenv("SB_URL" = "test.smartabase.com/test")

  expect_equal(.get_username(), "test.user")
  expect_equal(.get_username("test.user2"), "test.user2")
  expect_type(.get_username(), "character")

  expect_equal(.get_password(), "test123")
  expect_equal(.get_username("test321"), "test321")
  expect_type(.get_password(), "character")

  expect_equal(.get_username(54321), 54321)

  expect_equal(.get_url(), "test.smartabase.com/test")
  expect_equal(
    .get_username("test2.smartabase.com/test2"),
    "test2.smartabase.com/test2"
  )
  expect_type(.get_url(), "character")

  Sys.unsetenv("SB_USER")
  Sys.unsetenv("SB_PASS")
  Sys.unsetenv("SB_ID")
  Sys.unsetenv("SB_URL")

  expect_error(.get_username())
  expect_error(.get_password())
  expect_error(.get_url())
})
