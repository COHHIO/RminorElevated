test_that("which_cols returns numeric indices unchanged", {
  df <- data.frame(A = 1:3, B = 4:6, C = 7:9)
  expect_equal(which_cols(c(1, 3), df), c(1, 3))
})

test_that("which_cols resolves column names to positions", {
  df <- data.frame(A = 1:3, B = 4:6, C = 7:9)
  # Pins the documented example. If UU::regex_or anchors differently than
  # expected, adjust the expected positions here.
  expect_equal(as.integer(which_cols(c("A", "B"), df)), c(1L, 2L))
})

test_that("datatable_default injects the ZWNJ Excel-export fix", {
  # Regression guard for the PersonalID Excel export: every cell is prefixed
  # with a zero-width non-joiner so Excel doesn't strip leading zeros.
  dt <- datatable_default(head(iris))
  buttons <- dt$x$options$buttons
  excel <- Filter(function(b) is.list(b) && identical(b$extend, "excel"), buttons)

  expect_length(excel, 1)
  expect_match(as.character(excel[[1]]$customizeData), "\u200c", fixed = TRUE)
})

test_that("datatable_default merges add_options over the defaults", {
  dt <- datatable_default(head(iris), add_options = list(pageLength = 25))
  expect_equal(dt$x$options$pageLength, 25)
})

test_that("styleDivergentBar scales to the max absolute value", {
  js <- styleDivergentBar(c(-4, 0, 4), "#28a745", "#dc3545")
  expect_s3_class(js, "JS_EVAL")
  expect_match(as.character(js), "#28a745", fixed = TRUE)
  expect_match(as.character(js), "#dc3545", fixed = TRUE)
  expect_match(as.character(js), "4", fixed = TRUE) # max(abs(c(-4,0,4)))
})