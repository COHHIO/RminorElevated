# Helpers ---------------------------------------------------------------------

# Clear the internal store between tests so state doesn't leak. Covers loaders
# too, so deferred-path tests can't bleed into each other.
clear_app_data <- function() {
  for (nm in c("APP_DATA", "DEFERRED_LOADERS")) {
    if (exists(nm, envir = .app_data_env, inherits = FALSE)) {
      rm(list = nm, envir = .app_data_env)
    }
  }
}

# get_app_data() stamps a `clarity_linked` marker on everything it returns,
# including datasets decoration leaves untouched. Drop it when comparing
# content against a raw fixture.
strip_marker <- function(x) {
  attr(x, "clarity_linked") <- NULL
  x
}

# Accessor contract -----------------------------------------------------------

test_that("get_app_data() errors before data is initialized", {
  withr::defer(clear_app_data())
  clear_app_data()
  expect_error(get_app_data(), "not initialized")
  expect_error(get_app_data("Regions"), "not initialized")
})

test_that("get_app_data() round-trips a named dataset", {
  withr::defer(clear_app_data())
  fake <- list(
    Regions = data.frame(Region = 1:2, RegionName = c("A", "B")),
    validation = data.frame(ProjectID = 1L, ProjectName = "X")
  )
  set_app_data(fake)

  expect_identical(strip_marker(get_app_data("Regions")), fake$Regions)
  expect_identical(strip_marker(get_app_data("validation")), fake$validation)
})

test_that("get_app_data() with no name returns the full list", {
  # name = NULL returns the raw list: no decoration, no deferred loaders.
  # This is the documented contract, not an oversight. See #77.
  withr::defer(clear_app_data())
  fake <- list(Regions = data.frame(Region = 1L))
  set_app_data(fake)

  expect_identical(get_app_data(), fake)
})

test_that("get_app_data() rejects unknown and malformed names", {
  withr::defer(clear_app_data())
  set_app_data(list(Regions = data.frame(Region = 1L)))

  expect_error(get_app_data("does_not_exist"), "not found")
  expect_error(get_app_data(c("a", "b")), "single string")
})

test_that("get_app_data() matches the legacy per-name accessor", {
  # Proves PR1 equivalence: what create_data_accessors() would have produced
  # for a name is identical to get_app_data(name).
  withr::defer(clear_app_data())
  fake <- list(Regions = data.frame(Region = 1:3))
  set_app_data(fake)

  legacy_Regions <- local({ dataset <- fake$Regions; function() dataset })
  expect_identical(strip_marker(get_app_data("Regions")), legacy_Regions())
})

# Lazy decoration (#77) -------------------------------------------------------

test_that("first access decorates and memoizes", {
  withr::defer(clear_app_data())
  df <- data.frame(a = 1:3)
  set_app_data(list(plain = df))

  first <- get_app_data("plain")

  expect_true(isTRUE(attr(first, "clarity_linked", exact = TRUE)))
  expect_identical(strip_marker(first), df)

  # written back into the store, not just returned to the caller
  expect_true(
    isTRUE(attr(get_app_data()[["plain"]], "clarity_linked", exact = TRUE))
  )

  expect_identical(get_app_data("plain"), first)
})

test_that("decoration runs exactly once per dataset", {
  withr::defer(clear_app_data())
  calls <- 0L
  local_mocked_bindings(
    add_clarity_links_df = function(df) {
      calls <<- calls + 1L
      df
    }
  )

  set_app_data(list(plain = data.frame(a = 1:3)))

  get_app_data("plain")
  get_app_data("plain")
  get_app_data("plain")

  expect_identical(calls, 1L)
})

test_that("deferred datasets are decorated on first access", {
  # The consistency fix: pre-#77 the loader's return value was stored raw.
  withr::defer(clear_app_data())
  set_app_data(list())
  set_deferred_loaders(list(lazy_one = function() data.frame(a = 1:3)))

  out <- get_app_data("lazy_one")

  expect_true(isTRUE(attr(out, "clarity_linked", exact = TRUE)))
})

test_that("get_app_data() with no name does not trigger deferred loaders", {
  withr::defer(clear_app_data())
  loader_ran <- FALSE
  set_app_data(list(plain = data.frame(a = 1:3)))
  set_deferred_loaders(list(lazy_one = function() {
    loader_ran <<- TRUE
    data.frame(b = 1)
  }))

  all_data <- get_app_data()

  expect_named(all_data, "plain")
  expect_null(attr(all_data[["plain"]], "clarity_linked", exact = TRUE))
  expect_false(loader_ran)
})