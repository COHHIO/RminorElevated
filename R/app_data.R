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
    
    s3_folder <- get_golem_config("data_env")
    
    s3_objects <- aws.s3::get_bucket(
      bucket = "shiny-data-cohhio",
      prefix = s3_folder,
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