# Deployment smoke test: boot the whole app offline and confirm it serves.
#
# AppDriver runs the app in a SEPARATE process, so test-process set_app_data()
# won't reach it. Injection goes through the RME_APP_DATA_FIXTURE env var, which
# the guard in global.R reads to load an RDS fixture instead of hitting S3.
#
# Needs a headless Chrome (shinytest2 drives chromote) -- runs locally and in
# CI once Chrome is installed on the runner.

# Minimal app data: enough columns for global.R to build programs / regions /
# qpr_tab_choices without erroring. No PersonalID+UniqueID pairs, so clarity
# decoration stays a no-op and clarity.looker isn't exercised. This is a
# STARTING POINT -- expect to add datasets/columns as first runs surface them
# (see note at the bottom on snapshotting a real boot instead).
make_smoke_fixture <- function() {
  list(
    validation = tibble::tibble(
      ProjectID   = 1:2,
      ProjectName = c("Alpha ES", "Beta TH")
    ),
    Regions = tibble::tibble(
      Region     = c(1L, 1L, 2L),
      RegionName = c("Region A", "Region A", "Region B"),
      County     = c("County X", "County Y", "County Z")
    ),
    qpr_leavers = tibble::tibble(
      ProjectName = c("Alpha ES", "Beta TH"),
      ProjectType = c(1L, 2L)
    ),
    qpr_benefits     = tibble::tibble(ProjectName = "Alpha ES"),
    qpr_income       = tibble::tibble(ProjectName = "Alpha ES"),
    qpr_rrh_enterers = tibble::tibble(ProjectName = "Gamma RRH"),
    qpr_reentries    = tibble::tibble(ExitingHP = "Alpha ES"),
    qpr_spending     = tibble::tibble(OrganizationName = "Org 1"),
    rm_dates = list(
      meta_HUDCSV = list(
        Export_Start = as.Date("2024-01-01"),
        Export_End   = as.Date("2024-12-31")
      )
    ),
    program_lookup = tibble::tibble(
      ProjectID   = 1:2,
      ProjectName = c("Alpha ES", "Beta TH")
    )
  )
}

test_that("the app boots offline and serves the UI", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  # Local R (4.4.1) is older than the R the installed packages were built
  # under (4.4.3), so the app subprocess promotes the benign build-version
  # warning to a fatal error before app.R is ever evaluated. Docker/CI builds
  # and runs under one R version, so this runs for real there.
  shiny_built <- package_version(
    sub("^R ([0-9.]+).*", "\\1", packageDescription("shiny")$Built)
  )
  skip_if(getRversion() < shiny_built,
          "Local R older than installed package builds; smoke test runs in CI.")

  fixture_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(make_smoke_fixture(), fixture_path)
  withr::local_envvar(RME_APP_DATA_FIXTURE = fixture_path)

  app <- shinytest2::AppDriver$new(
  app_dir      = testthat::test_path("..", ".."),
  name         = "rme-smoke",
  options      = list(warn = 1),
  timeout      = 60 * 1000,
  load_timeout = 120 * 1000
)
  withr::defer(app$stop())

  # Boot alone is the real assertion: a startup error in global.R or module
  # wiring means AppDriver$new() never reaches ready, and this fails loudly.
  expect_true(app$get_js("!!window.Shiny"))

  # Sanity that the shell rendered. Tighten this to a known navbar/sidebar
  # selector once you've eyeballed the DOM (e.g. app$get_html(".main-sidebar")).
  expect_false(is.null(app$get_html("body")))
})

# ---------------------------------------------------------------------------
# On the fixture: this app has ~40 modules, so a hand-built fixture will likely
# miss a column or dataset that some module touches at boot. Two ways forward:
#
#   1. Iterate: run the test, read app$get_logs() (or the failure output) for
#      the child's startup error, add the missing piece to make_smoke_fixture(),
#      repeat. A few rounds usually gets there.
#
#   2. More reliable -- snapshot the real shape once, from a dev session with S3
#      access, then trim/anonymize (HMIS data is sensitive) into a small file:
#         saveRDS(get_app_data(), testthat::test_path("fixtures", "app_data.rds"))
#      and point the fixture at that instead of make_smoke_fixture().
# ---------------------------------------------------------------------------