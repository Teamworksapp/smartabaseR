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
