# Regression guard for issue #60. Supersedes the earlier, narrower guards:
#   - test-qpr_expr-migration.R (step 2): only checked the qpr_expr object
#   - test-accessor-migration.R (step 3): only checked function bodies, so it
#     could not see global.R's top-level lookups
#
# This scans the raw text of every R/ source file directly, so it catches
# accessor calls regardless of whether they live inside a function, a quoted
# expr, or top-level package code (global.R). That completeness is what step 4
# needs: the remaining call sites before this step were all top-level in
# global.R, invisible to a function-body scan.
#
# Scope notes (do not "fix" these — they are intentional exclusions):
# - server_debounce() creates debounced-input closures (county(), vet_status(),
#   program(), date_range(), mpo_type(), chronic_status()) that read like
#   dataset accessors but are not datasets. None of their names appear below.
# - The no-paren `rm_dates$meta_HUDCSV$Export_End` reference in
#   mod_DQunshIncorrectEEType.R is a separate, pre-existing bug unrelated to
#   #60 and is out of scope for this guard.

legacy_accessors <- c(
  # local datasets (app_data.R)
  "Regions", "rm_dates", "co_clients_served", "program_lookup",
  # data quality
  "validation", "dq_main", "dq_APs", "dq_aps_no_referrals", "dq_overlaps",
  "dq_eligibility_detail", "dq_providers_df", "dq_unsheltered",
  "unsheltered_by_month",
  # QPR
  "qpr_leavers", "qpr_benefits", "qpr_income", "qpr_reentries",
  "qpr_rrh_enterers", "qpr_spending", "qpr_spdats_county", "qpr_spdats_project",
  # other reports
  "prioritization", "prioritization_colors", "utilization_beds",
  "utilization_clients", "veteran_active_list",
  # performance evaluation (CoC competition), plain + Mahoning variants
  "pe_summary_validation", "pe_summary_final_scoring", "pe_exits_to_ph",
  "pe_return_to_homelessness", "pe_res_prior", "pe_benefits_at_exit",
  "pe_entries_no_income", "pe_increase_income", "pe_increase_earned_income",
  "pe_homeless_history_index", "pe_scored_at_ph_entry",
  "pe_summary_validation_mahoning", "pe_summary_final_scoring_mahoning",
  "pe_exits_to_ph_mahoning", "pe_return_to_homelessness_mahoning",
  "pe_res_prior_mahoning", "pe_benefits_at_exit_mahoning",
  "pe_entries_no_income_mahoning", "pe_increase_income_mahoning",
  "pe_increase_earned_income_mahoning", "pe_homeless_history_index_mahoning",
  "pe_scored_at_ph_entry_mahoning"
)

# Read every R/ source file, stripping line comments first. Some files (e.g.
# app_data.R, global.R) intentionally mention accessor names in prose comments
# explaining the migration; a raw text match would flag those false-positive.
# This is a simple `#`-to-end-of-line strip, not a full R tokenizer, which is
# adequate here because none of the affected lines contain a literal "#"
# inside a string.
strip_comments <- function(lines) {
  sub("#.*$", "", lines)
}

r_dir <- testthat::test_path("..", "..", "R")
r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
names(r_files) <- basename(r_files)

file_code <- vapply(
  r_files,
  function(f) paste(strip_comments(readLines(f, warn = FALSE)), collapse = "\n"),
  character(1)
)

test_that("R/ source files were located", {
  # Guards against the test silently no-op'ing if the path above ever breaks
  # again (e.g. a future testthat version changes test_path() behavior).
  expect_gt(length(r_files), 0)
  expect_true("global.R" %in% names(r_files))
})

test_that("get_app_data() is defined and usable", {
  expect_true(exists("get_app_data", mode = "function"))
})

test_that("create_data_accessors() no longer exists anywhere in R/", {
  offenders <- names(file_code)[grepl("create_data_accessors\\s*(<-|=)\\s*function", file_code)]
  expect_identical(offenders, character(0),
    info = paste("create_data_accessors() still defined in:", paste(offenders, collapse = ", ")))

  offenders <- names(file_code)[grepl("\\bcreate_data_accessors\\(", file_code)]
  expect_identical(offenders, character(0),
    info = paste("create_data_accessors() still called in:", paste(offenders, collapse = ", ")))
})

test_that("no source file calls a legacy per-name accessor", {
  for (accessor in legacy_accessors) {
    # Bare call `accessor(`. get_app_data("accessor") is followed by a quote,
    # not "(", so it never matches this pattern.
    pattern <- paste0("\\b", accessor, "\\(")
    offenders <- names(file_code)[grepl(pattern, file_code)]
    expect_identical(
      offenders, character(0),
      info = sprintf(
        "Legacy accessor `%s()` still called in: %s",
        accessor, paste(offenders, collapse = ", ")
      )
    )
  }
})

test_that("global.R's load-time lookups route through get_app_data()", {
  gr <- file_code[["global.R"]]
  expect_true(grepl('get_app_data("validation")', gr, fixed = TRUE))
  expect_true(grepl('get_app_data("Regions")', gr, fixed = TRUE))
  expect_true(grepl('get_app_data("qpr_leavers")', gr, fixed = TRUE))
  # The exists()-based guards this replaced should be gone too.
  expect_false(grepl('exists("validation")', gr, fixed = TRUE))
  expect_false(grepl('exists("Regions")', gr, fixed = TRUE))
})