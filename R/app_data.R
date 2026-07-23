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
    rlang::abort(
      sprintf(
        "Dataset '%s' not found. Available: %s",
        name, paste(names(data), collapse = ", ")
      )
    )
  }
  data[[name]]
}

load_local_data <- function () {
  local_data <- list()
  
  local_data$Regions <- HMISdata::Regions
  local_data$rm_dates <- HMISprep::load_dates()
  local_data$co_clients_served <-
    HMISdata::load_hmis_parquet("co_clients_served.parquet")
  
  local_data$program_lookup <-
    HMISdata::load_hmis_parquet("program_lookup.parquet")
  
  local_data
}

load_app_data <- function() {
  
  cli::cli_alert_info("Initializing app data...")
  
  s3_data <- list()
  
  tryCatch({
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
    
    s3_data <- purrr::map(
      rlang::set_names(
        s3_files,
        tools::file_path_sans_ext(s3_files)
      ),
      load_s3_file
    ) |>
      purrr::compact()
    
  }, error = function(e) {
    
    cli::cli_alert_danger(
      "S3 loading failed: {e$message}"
    )
    
  })
  
  local_data <- load_local_data()
  
  APP_DATA <- add_clarity_links(
    c(s3_data, local_data)
  )
  
  APP_DATA
}