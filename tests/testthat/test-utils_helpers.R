# Tests for the error-handling wrappers in R/utils_helpers.R
# Run with devtools::test() or testthat::test_file() after devtools::load_all().

test_that("safe_render returns the value when nothing errors", {
  expect_equal(safe_render(42), 42)
})

test_that("safe_render swallows errors, alerts, and returns NULL", {
  called <- 0L
  # shinyalert needs a live session; mock it so the error path is testable.
  local_mocked_bindings(
    shinyalert = function(...) {
      called <<- called + 1L
      invisible(NULL)
    },
    .package = "shinyalert"
  )

  expect_null(safe_render(stop("boom")))
  expect_equal(called, 1L)
})

test_that("safe_render re-throws shiny.silent.error instead of swallowing it", {
  silent <- structure(
    class = c("shiny.silent.error", "error", "condition"),
    list(message = "", call = NULL)
  )
  expect_error(safe_render(stop(silent)), class = "shiny.silent.error")
})

# NOTE: safe_reactive() and safe_reactive_quoted() wrap shiny::reactive(), so
# they need a reactive context. Test those with shiny::testServer() (or
# shiny::reactiveConsole(TRUE) + isolate()) plus the same shinyalert mock --
# that's a separate layer from these plain-function unit tests.
