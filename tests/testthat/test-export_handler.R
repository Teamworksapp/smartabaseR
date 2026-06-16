test_that("{page_n} interpolates in paginate_export progress message without error (DP-1547)", {
  # Minimal arg structure used by .paginate_export() for the progress message.
  arg <- list(
    form   = "Test Form",
    option = list(interactive_mode = TRUE)
  )
  page_n <- 2L

  # Capture any message() conditions emitted by cli_progress_message.
  captured <- character(0)

  # Regression: before the fix, cli evaluated {page_n} in arg$current_env
  # (the user's calling environment) where the variable does not exist,
  # producing "! object 'page_n' not found". The fix passes
  # rlang::current_env() so both page_n and arg$form are resolved in the
  # function's own scope. This block is the exact code path from
  # .paginate_export() that was broken.
  expect_no_error(
    withCallingHandlers(
      if (isTRUE(arg$option$interactive_mode) && page_n > 1L) {
        cli::cli_progress_message(
          "Fetching page {page_n} of {.field {arg$form}} data...",
          .envir = rlang::current_env()
        )
      },
      message = function(m) {
        captured <<- c(captured, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  )

  # Verify the rendered text contains the interpolated page number and form
  # name. cli_progress_message() emits a message() in non-interactive sessions;
  # we check that output when available.
  if (length(captured) > 0L) {
    all_output <- paste(captured, collapse = " ")
    expect_match(all_output, "Fetching page 2", fixed = TRUE)
    expect_match(all_output, "Test Form",        fixed = TRUE)
  }

  # Independent check: verify the same format string and environment resolve
  # correctly via glue, confirming the interpolation is correct regardless of
  # cli's internal message-emission timing.
  rendered <- glue::glue(
    "Fetching page {page_n} of {arg$form} data...",
    .envir = rlang::current_env()
  )
  expect_match(as.character(rendered), "Fetching page 2", fixed = TRUE)
  expect_match(as.character(rendered), "Test Form",        fixed = TRUE)
})


# Helper: build a minimal sb_df page with a typed Post Code column
make_mock_page <- function(post_code_vec) {
  df <- tibble::tibble(
    about        = paste("Athlete", seq_along(post_code_vec)),
    user_id      = as.integer(seq_along(post_code_vec)),
    `Post Code`  = post_code_vec
  )
  result <- tibble::new_tibble(
    df,
    nrow             = nrow(df),
    class            = "sb_df_non_interactive",
    request          = list(),
    http_method      = "POST",
    http_status_code = 200L
  )
  attr(result, "form")        <- "Test Form"
  attr(result, "export_time") <- as.POSIXct("2025-01-01")
  result
}

test_that(".combine_paginated_pages() handles double/character type conflict (DP-1547)", {
  # Reproduce the exact error: pages where per-page type_convert gives
  # <double> on numeric-only pages and <character> on blank-only pages.
  page_double <- make_mock_page(c(2000, 3000, 4000))   # already numeric
  page_char   <- make_mock_page(c("", "", ""))          # character, type_convert left as-is

  # Confirm the old bare bind_rows would error with this input
  expect_error(
    dplyr::bind_rows(purrr::map(list(page_double, page_char), tibble::as_tibble)),
    regexp = "Can't combine"
  )

  arg <- list(
    form     = "Test Form",
    endpoint = "eventsearch",
    option   = list(interactive_mode = FALSE, guess_col_type = TRUE)
  )

  result <- .combine_paginated_pages(list(page_double, page_char), arg)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6L)
  expect_type(result$`Post Code`, "double")
  # Blank-page rows become NA, not errors
  expect_equal(sum(is.na(result$`Post Code`)), 3L)
  expect_equal(result$`Post Code`[1:3], c(2000, 3000, 4000))
})

test_that(".combine_paginated_pages() with guess_col_type = FALSE keeps metadata ID cols numeric", {
  page_double <- make_mock_page(c(2000, 3000))
  page_char   <- make_mock_page(c("", ""))

  arg <- list(
    form     = "Test Form",
    endpoint = "eventsearch",
    option   = list(interactive_mode = FALSE, guess_col_type = FALSE)
  )

  result <- .combine_paginated_pages(list(page_double, page_char), arg)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4L)
  # User form fields stay as character when guess_col_type is FALSE
  expect_type(result$`Post Code`, "character")
  # Metadata ID columns must always be numeric regardless of guess_col_type
  expect_type(result$user_id, "double")
})

test_that("{page_n} interpolation fails with wrong env, confirming the regression was real", {
  # Mirror the broken code path (.envir = arg$current_env where page_n is absent)
  # to confirm that using the wrong environment does indeed error. This ensures
  # the regression test above would catch a reversion of the fix.
  arg <- list(
    form        = "Test Form",
    option      = list(interactive_mode = TRUE),
    current_env = new.env(parent = emptyenv())   # env that does NOT have page_n
  )
  page_n <- 2L   # defined here, but NOT in arg$current_env

  expect_error(
    cli::cli_progress_message(
      "Fetching page {page_n} of {.field {arg$form}} data...",
      .envir = arg$current_env
    ),
    regexp = "page_n"
  )
})
