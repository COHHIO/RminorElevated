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

test_that("datatable_default applies the Excel fix to a string 'excel' button from add_options", {
  # add_options replaces the default buttons wholesale, so the normalizer must
  # still upgrade a bare-string "excel" to carry the ZWNJ customizeData.
  dt <- datatable_default(head(iris), add_options = list(buttons = list("excel")))
  excel <- Filter(function(b) is.list(b) && identical(b$extend, "excel"), dt$x$options$buttons)

  expect_length(excel, 1)
  expect_match(as.character(excel[[1]]$customizeData), "\u200c", fixed = TRUE)
})

test_that("datatable_default applies the Excel fix to a list-form 'excel' button from add_options", {
  # Regression guard: a list-form excel button (e.g. one carrying its own
  # filename) previously slipped past the normalizer and lost the ZWNJ fix,
  # silently reintroducing the large-number / NULL Excel export bug.
  dt <- datatable_default(
    head(iris),
    add_options = list(buttons = list(list(extend = "excel", filename = "keepme")))
  )
  excel <- Filter(function(b) is.list(b) && identical(b$extend, "excel"), dt$x$options$buttons)

  expect_length(excel, 1)
  expect_match(as.character(excel[[1]]$customizeData), "\u200c", fixed = TRUE)
  expect_equal(excel[[1]]$filename, "keepme")  # caller-set filename must survive
})

test_that("datatable_default stamps the download filename onto export buttons", {
  default_excel <- Filter(
    function(b) is.list(b) && identical(b$extend, "excel"),
    datatable_default(head(iris))$x$options$buttons
  )[[1]]
  expect_match(default_excel$filename, "^RminorElevated_\\d{4}-\\d{2}-\\d{2}$")

  buttons <- datatable_default(head(iris), filename = "client_counts")$x$options$buttons
  excel <- Filter(function(b) is.list(b) && identical(b$extend, "excel"), buttons)[[1]]
  csv   <- Filter(function(b) is.list(b) && identical(b$extend, "csvHtml5"), buttons)[[1]]
  expect_equal(excel$filename, "client_counts")
  expect_equal(csv$filename, "client_counts")
})

test_that("datatable_default suffixes a 'Full CSV' button filename with _full", {
  dt <- datatable_default(
    head(iris),
    filename = "base",
    add_options = list(buttons = list(list(extend = "csvHtml5", text = "Full CSV")))
  )
  full <- Filter(function(b) is.list(b) && identical(b$text, "Full CSV"), dt$x$options$buttons)[[1]]
  expect_equal(full$filename, "base_full")
})

test_that("datatable_default with export_buttons = FALSE removes the native button bar", {
  # Server-side tables download through the module instead, so the page-limited
  # native buttons must be gone and B dropped from the dom.
  dt <- datatable_default(head(iris), export_buttons = FALSE)
  expect_null(dt$x$options$buttons)
  expect_false(grepl("B", dt$x$options$dom, fixed = TRUE))
})

test_that("datatable_add_bars returns the table unchanged when there is nothing to draw", {
  empty <- DT::datatable(data.frame(val = numeric(0)), rownames = FALSE)
  expect_identical(
    datatable_add_bars(empty, columns = "val", valueColumns = "val", color = "lightblue"),
    empty
  )

  all_na <- DT::datatable(data.frame(val = c(NA_real_, NA_real_)), rownames = FALSE)
  expect_identical(
    datatable_add_bars(all_na, columns = "val", valueColumns = "val", color = "lightblue"),
    all_na
  )
})

test_that("datatable_add_bars draws bars when finite values are present", {
  tbl <- DT::datatable(data.frame(val = c(1, 5, 9)), rownames = FALSE)
  out <- datatable_add_bars(tbl, columns = "val", valueColumns = "val", color = "lightblue")

  expect_s3_class(out, "datatables")
  expect_false(identical(out, tbl))  # formatStyle applied -> object changed
})

test_that("mod_dt_download_server strips HTML from character columns before export", {
  df <- data.frame(
    name = c("<b>Alice</b>", "Bob &amp; Co"),
    n    = c(1L, 2L),
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    mod_dt_download_server,
    args = list(data = shiny::reactive(df), filename_prefix = "test"),
    {
      cleaned <- clean()
      expect_equal(cleaned$name, c("Alice", "Bob & Co"))  # tags stripped, entity unescaped
      expect_equal(cleaned$n, c(1L, 2L))                   # non-character column untouched
    }
  )
})