test_that("non-data-frame input is returned unchanged", {
  x <- list(a = 1, b = 2)
  expect_identical(add_clarity_links_df(x), x)
})

test_that("a data frame without ID columns is returned unchanged", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  expect_identical(add_clarity_links_df(df), df)
})

test_that("a linkable data frame gets its ID column rewritten as a link", {
  skip_if_not_installed("clarity.looker")

  df <- data.frame(PersonalID = 1:2, UniqueID = c("a", "b"))
  out <- add_clarity_links_df(df)

  # make_linked_df() rewrites UniqueID in place; it doesn't add a column
  expect_identical(names(out), names(df))
  expect_identical(out$PersonalID, df$PersonalID)
  expect_match(out$UniqueID, "^<a href=", all = TRUE)
  expect_match(out$UniqueID[[1]], ">a</a>", fixed = TRUE)
})
