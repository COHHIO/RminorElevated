# Tests for the QPR calculation logic in R/qpr_expr_*.R
#
# These aren't functions -- each report's math lives in a quoted expression at
# qpr_expr$<report>$expr, evaluated later in a reactive context. So we evaluate
# the expression in a child of the package namespace with `input` injected,
# feed fixtures through set_app_data(), and mock the HMIS::*_between() date
# helpers so the test exercises OUR ProjectType/Destination logic, not HMIS's
# date-window code. We test $expr only -- $infobox/$datatable are presentation.
#
# eval_qpr() and reset_store() are good candidates to move into
# tests/testthat/helper-qpr.R if other test files end up needing them.

eval_qpr <- function(report, part = "expr", input = list(), data = NULL) {
  if (!is.null(data)) set_app_data(data)
  env <- rlang::env(
    rlang::ns_env("RminorElevated"), # get_app_data, qpr_* helpers, imported shiny
    input = input,
    req   = function(...) invisible(TRUE) # inputs are always present in tests
  )
  rlang::eval_bare(qpr_expr[[report]][[part]], env = env)
}

reset_store <- function() {
  for (nm in c("APP_DATA", "DEFERRED_LOADERS")) {
    if (exists(nm, envir = .app_data_env, inherits = FALSE)) {
      rm(list = nm, envir = .app_data_env)
    }
  }
}

# ---- length_of_stay ---------------------------------------------------------

test_that("length_of_stay$expr keeps only in-region leavers with a valid stay", {
  withr::defer(reset_store())
  # exited_between passthrough -> the date window includes everything, so the
  # test isolates the ProjectType + ProjectName + NA logic.
  local_mocked_bindings(add_clarity_links_df = function(x) x)
  local_mocked_bindings(exited_between = function(data, ...) data, .package = "HMIS")

  leavers <- tibble::tibble(
    UniqueID         = 1:5,
    ProjectName      = c("Region A", "Region A", "Region A", "Region B", "Region A"),
    ProjectType      = c(13, 1, 13, 1, 2),
    MoveInDateAdjust = as.Date(c("2024-01-01", NA, NA, NA, NA)),
    ExitDate         = as.Date(c(NA, "2024-02-01", NA, "2024-02-01", NA))
  )

  result <- eval_qpr(
    "length_of_stay",
    input = list(
      date_range = as.Date(c("2024-01-01", "2024-12-31")),
      region     = "Region A"
    ),
    data = list(qpr_leavers = leavers)
  )

  # row 1: PT13 with a move-in date -> kept
  # row 2: PT1 with an exit date    -> kept
  # row 3: PT13 but no move-in date -> dropped
  # row 4: right criteria, wrong region -> dropped
  # row 5: PT2 but no exit date     -> dropped
  expect_equal(sort(result$UniqueID), c(1L, 2L))
})

# ---- permanent_housing ------------------------------------------------------

test_that("permanent_housing$expr builds the placed / total household sets", {
  withr::defer(reset_store())
  local_mocked_bindings(add_clarity_links_df = function(x) x)
  # Drive .exited / .served straight off fixture columns so each row's
  # membership is explicit and hand-verifiable.
  local_mocked_bindings(
    exited_between = function(data, ...) data$exited_flag,
    served_between = function(data, ...) data$served_flag,
    .package = "HMIS"
  )

  leavers <- tibble::tibble(
    UniqueID         = 1:7,
    ProjectName      = c(rep("R", 6), "Z"),
    ProjectType      = c(3, 9, 1, 1, 12, 5, 3),
    DestinationGroup = c("Permanent", "Temporary", "Permanent",
                         "Temporary", "Permanent", "Permanent", "Permanent"),
    ExitDate         = as.Date(c("2024-03-01", NA, "2024-03-01",
                                 "2024-03-01", "2024-03-01", "2024-03-01", "2024-03-01")),
    served_flag      = c(TRUE,  TRUE,  FALSE, FALSE, FALSE, TRUE,  TRUE),
    exited_flag      = c(FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,  TRUE)
  )

  result <- eval_qpr(
    "permanent_housing",
    input = list(
      date_range = as.Date(c("2024-01-01", "2024-12-31")),
      region     = "R"
    ),
    data = list(qpr_leavers = leavers)
  )

  # region Z (row 7) is filtered out before any of this.
  # Successfully placed: 1 (PSH served, perm), 2 (HP served, still in),
  #                      3 (RRH exited to perm).                     -> 3
  # Total:               the above + 4 (RRH exited, temp dest).      -> 4
  # row 5 (PSH, not served) and row 6 (PT5, out of scope) are in neither.
  expect_equal(nrow(result$SuccessfullyPlaced), 3L)
  expect_equal(nrow(result$TotalHHsSuccessfulPlacement), 4L)
  expect_setequal(result$SuccessfullyPlaced$UniqueID, c(1L, 2L, 3L))
  expect_setequal(result$TotalHHsSuccessfulPlacement$UniqueID, c(1L, 2L, 3L, 4L))
})