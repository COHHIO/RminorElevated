# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.
#' @include utils_helpers.R
Sys.setenv(TZ = "America/New_York")

# global.R
# Ensure S3 accessor functions are created first
if (!exists("create_accessors_s3")) {
  # If running in development, load the function
  if (file.exists("R/fct_s3.R")) {
    source("R/fct_s3.R")
  }
}

# Function to apply data linking (extracted from deps_to_destination logic)
apply_data_linking <- function(data_list) {
  cli::cli_alert_info("Applying data linking to datasets...")
  
  linked_data <- purrr::map(data_list, function(df) {
    if (!is.data.frame(df) || !UU::is_legit(names(df))) {
      return(df)
    }
    
    # Apply linking logic from deps_to_destination
    linked_df <- df
    
    # Check for PersonalID + UniqueID combination
    if (all(c("PersonalID", "UniqueID") %in% names(df))) {
      linked_df <- clarity.looker::make_linked_df(df, UniqueID)
      cli::cli_alert_info("Applied UniqueID linking to dataset")
    }
    # Check for PersonalID + EnrollmentID combination
    else if (all(c("PersonalID", "EnrollmentID") %in% names(df))) {
      linked_df <- clarity.looker::make_linked_df(df, EnrollmentID)
      cli::cli_alert_info("Applied EnrollmentID linking to dataset")
    }
    
    return(linked_df)
  })
  
  return(linked_data)
}

# Initialize global data store
cli::cli_alert_info("Starting one-time data download from S3...")
.download_time <- system.time({
  
  # Pre-download all S3 data files
  s3_data <- list()
  
  # Get list of available S3 files
  tryCatch({
    s3_objects <- aws.s3::get_bucket(bucket = "shiny-data-cohhio", prefix = "RME", region = "us-east-2")
    s3_files <- purrr::map_chr(s3_objects, ~.x$Key) |>
      basename()
    s3_files <- s3_files[s3_files != "" & tools::file_ext(s3_files) %in% c("rds", "parquet")]
    
    cli::cli_alert_info("Found {length(s3_files)} data files in S3")
    
    # Download each file once and store in memory
    s3_data <- purrr::map(
      rlang::set_names(s3_files, tools::file_path_sans_ext(s3_files)),
      function(file_name) {
        cli::cli_alert_info("Loading {file_name}...")
        load_s3_file(file_name)
      }
    )
    
    # Remove any NULL entries (failed downloads)
    s3_data <- purrr::compact(s3_data)
    
  }, error = function(e) {
    cli::cli_alert_danger("Error accessing S3: {e$message}")
    s3_data <- list()
  })
  
  # Load local/computed data
  local_data <- list()
  tryCatch({
    local_data$Regions <- HMISdata::Regions
    local_data$rm_dates <- HMISprep::load_dates()
    local_data$co_clients_served <- HMISdata::load_hmis_parquet("co_clients_served.parquet")
    local_data$program_lookup <- HMISdata::load_hmis_parquet("program_lookup.parquet")
  }, error = function(e) {
    cli::cli_alert_warning("Some local data failed to load: {e$message}")
  })
  
  # Combine all data
  all_data <- c(s3_data, local_data)
  
  # Apply data linking to relevant datasets
  APP_DATA <- apply_data_linking(all_data)
})

# Get refresh timestamp
DATA_REFRESH_TIME <- get_s3_refresh_date()

cli::cli_alert_success("Data loading completed in {(.download_time['elapsed'])} seconds")
cli::cli_alert_info("Loaded {length(APP_DATA)} datasets: {paste(names(APP_DATA), collapse = ', ')}")
cli::cli_alert_info("Data refresh time: {DATA_REFRESH_TIME}")

# Create simple accessor functions that just return the pre-loaded data
create_data_accessors <- function(data_list) {
  accessor_functions <- purrr::map(data_list, function(dataset) {
    function() dataset  # Return the already-loaded dataset
  })
  
  # Assign to global environment so they're available throughout the app
  purrr::iwalk(accessor_functions, function(func, name) {
    assign(name, func, envir = .GlobalEnv)
  })
  
  cli::cli_alert_success("Created {length(accessor_functions)} data accessor functions")
  invisible(accessor_functions)
}

# Create the accessor functions
if (length(APP_DATA) > 0) {
  create_data_accessors(APP_DATA)
}

# Optional: Create a function to check data freshness
check_data_freshness <- function() {
  current_s3_time <- get_s3_refresh_date()
  if (current_s3_time > DATA_REFRESH_TIME) {
    cli::cli_alert_warning("S3 data has been updated since app startup. Consider restarting the app.")
    return(FALSE)
  }
  return(TRUE)
}

# Clean up temporary variables
rm(.download_time)

cli::cli_alert_success("Global environment setup complete!")

if (exists("validation")) {
  programs <- validation() |>
    dplyr::distinct(ProjectID, ProjectName) |>
    dplyr::arrange(ProjectName) |> 
      {\(x) {rlang::set_names(x$ProjectID, x$ProjectName)}}()
  
    
  
}
  
if (exists("Regions")) {
  regions <- Regions() |> 
    dplyr::distinct(Region, RegionName) |> 
    {\(x) {rlang::set_names(x$Region, x$RegionName)}}()
  counties <- sort(Regions()$County)
  qpr_tab_choices <- regions |>
    {\(x) {list(
      community_need_ph = list(
        choices = x
      ),
      community_need_lh = list(
        choices = x
      ),
      length_of_stay = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(0, 1, 2, 8, 13)])
      ),
      permanent_housing = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(0:4, 8:9, 12:13)])
      ),
      temp_permanent_housing = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(4)])
      ),
      noncash_benefits = list(
        choices = unique(qpr_benefits()$ProjectName)
      ),
      health_insurance = list(
        choices = unique(qpr_benefits()$ProjectName)
      ),
      income_growth = list(
        choices = unique(qpr_income()$ProjectName)
      ),
      rrh_placement = list(
        choices = unique(sort(
          qpr_rrh_enterers()$ProjectName
        ))
      ),
      reentries = list(
        choices = unique(sort(
          qpr_reentries()$ExitingHP
        ))
      ),
      rrh_spending = list(
        choices = unique(sort(
          qpr_spending()$OrganizationName
        ))
      )
    )}}()
  
}



