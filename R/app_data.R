DEFERRED_S3_DATASETS <- c(
  "enrollment_small"
)

# Deferred datasets that come from somewhere other than the app's S3 folder.
# Each entry is a zero-arg function returning the dataset. co_clients_served
# is not in the app's S3 folder — it's fetched through HMISdata, which is why
# it needs an explicit loader rather than a name in DEFERRED_S3_DATASETS.
DEFERRED_LOADERS <- list(
  co_clients_served = function() {
    HMISdata::load_hmis_parquet("co_clients_served.parquet")
  }
)

set_deferred_loaders <- function(loaders) {
  assign("DEFERRED_LOADERS", loaders, envir = .app_data_env)
  invisible(loaders)
}

get_deferred_loaders <- function() {
  get0(
    "DEFERRED_LOADERS",
    envir = .app_data_env,
    ifnotfound = rlang::set_names(character())
  )
}

# ---- App data accessor ------------------------------------------------------
# Single seam for retrieving loaded datasets. Backed by an internal environment
# so it can be populated at boot (global.R) and mocked in tests without touching
# .GlobalEnv. See issue #60.

.app_data_env <- new.env(parent = emptyenv())

#' Set the loaded app data
#'
#' Stores the assembled `APP_DATA` list in an internal environment so
#' [get_app_data()] can retrieve it. Called once at boot from `global.R`, and
#' usable in tests to inject fixtures.
#'
#' @param data A named list of datasets (the return value of [load_app_data()]).
#' @return `data`, invisibly.
#' @noRd
set_app_data <- function(data) {
  assign("APP_DATA", data, envir = .app_data_env)
  invisible(data)
}

#' Retrieve loaded app data
#'
#' The documented way to access datasets loaded by [load_app_data()]. Replaces
#' the per-name global accessor functions previously created by
#' `create_data_accessors()`.
#'
#' @param name Optional. The name of a single dataset to return. If `NULL`
#'   (the default), the full named list of datasets is returned.
#' @return The requested dataset, or the full named list when `name` is `NULL`.
#' @export
get_app_data <- function(name = NULL) {
  data <- get0("APP_DATA", envir = .app_data_env, ifnotfound = NULL)
  if (is.null(data)) {
    rlang::abort(
      "App data not initialized. Did load_app_data()/set_app_data() run?"
    )
  }
  if (is.null(name)) {
    return(data)
  }

  if (!is.character(name) || length(name) != 1L) {
    rlang::abort("`name` must be a single string or NULL.")
  }
  if (!name %in% names(data)) {
    loaders <- get_deferred_loaders()

    if (name %in% names(loaders)) {
      cli::cli_alert_info("Lazy-loading '{name}' on first access...")

      value <- loaders[[name]]()

      if (is.null(value)) {
        rlang::abort(
          sprintf("Failed to lazy-load dataset '%s'.", name)
        )
      }

      return(decorate_and_store(name, value, data))
    }
    
    rlang::abort(
      sprintf(
        "Dataset '%s' not found. Available: %s",
        name, paste(names(data), collapse = ", ")
      )
    )
  }
  
  decorate_and_store(name, data[[name]], data)

}

#' Decorate a dataset on first access and memoize the result
#'
#' Applies [add_clarity_links_df()] to `value` unless it's already been
#' decorated (tracked via a `clarity_linked` attribute, since
#' `make_linked_df()` isn't assumed idempotent), then writes the result back
#' into the app data store so later calls skip re-decorating. Shared by both
#' the already-loaded and deferred-loader branches of [get_app_data()] so
#' they can't diverge in whether/when decoration happens.
#'
#' @param name Name of the dataset, used as the key into `data`.
#' @param value The raw (possibly undecorated) dataset.
#' @param data The current full app data list, used as the base to update.
#' @return The decorated `value`.
#' @noRd
decorate_and_store <- function(name, value, data) {
  if (isTRUE(attr(value, "clarity_linked", exact = TRUE))) {
    return(value)
  }

  value <- add_clarity_links_df(value)
  attr(value, "clarity_linked") <- TRUE

  data[[name]] <- value
  set_app_data(data)

  value
}

load_local_data <- function () {
  local_data <- list()
  
  local_data$Regions <- HMISdata::Regions
  local_data$rm_dates <- HMISprep::load_dates()
  
  local_data$program_lookup <-
    HMISdata::load_hmis_parquet("program_lookup.parquet")
  
  local_data
}

load_app_data <- function() {
  
tictoc::tic()
  
  cli::cli_alert_info("Initializing app data...")
  
  s3_data <- list()
  s3_loaders <- list()
  
  tryCatch({
    
    s3_folder <- get_golem_config("data_env")

    s3_objects <- aws.s3::get_bucket(
      bucket = "shiny-data-cohhio",
      prefix = paste0(get_golem_config("data_env"), "/"),
      region = "us-east-2"
    )


    s3_files <- purrr::map_chr(s3_objects, ~.x$Key) |>
      basename()
    
    s3_files <- s3_files[
      s3_files != "" &
        tools::file_ext(s3_files) %in% c("rds", "parquet")
    ]
    
    deferred_names <- c(DEFERRED_S3_DATASETS, names(DEFERRED_LOADERS))
    is_deferred <- tools::file_path_sans_ext(s3_files) %in% deferred_names

    eager_files <- s3_files[!is_deferred]
    deferred_files <- s3_files[is_deferred]

    # Deferred files aren't downloaded here; build a loader closure per file
    # instead, so get_app_data() can fetch it on first access (see #73).
    s3_loaders <- purrr::map(
      rlang::set_names(
        deferred_files,
        tools::file_path_sans_ext(deferred_files)
      ),
      function(file_name) {
        force(file_name)
        function() load_s3_file(file_name)
      }
    )

    s3_data <- purrr::map(
      rlang::set_names(
        eager_files,
        tools::file_path_sans_ext(eager_files)
      ),
      load_s3_file
    ) |>
      purrr::compact()
    
    
  }, error = function(e) {
    
    cli::cli_alert_danger(
      "S3 loading failed: {e$message}"
    )
    
  })
  
  # Registered outside the tryCatch so the non-S3 loaders are still available
  # even if the bucket listing above failed.
  set_deferred_loaders(c(s3_loaders, DEFERRED_LOADERS))

  local_data <- load_local_data()
  
  # Stored raw — clarity-link decoration happens per-dataset on first access
  # in get_app_data(). See #77
  APP_DATA <- c(s3_data, local_data)
  
  cli::cli_alert_info("Total download time...")
  tictoc::toc()

  tibble::tibble(
    dataset = names(APP_DATA),
    size = purrr::map_dbl(APP_DATA, ~as.numeric(lobstr::obj_size(.x)))
  ) |>
    dplyr::arrange(dplyr::desc(size)) |>
    dplyr::mutate(size = scales::comma(size, suffix = " bytes")) |>
    print(n = Inf)

  APP_DATA
}