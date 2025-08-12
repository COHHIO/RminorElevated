#' Clean null/corrupted RDS files
#' @param files Character vector of file paths
#' @noRd
clean_null <- function(files) {
  .rds <- stringr::str_subset(files, "rds$")
  .sizes <- file.size(.rds)
  file.remove(.rds[.sizes == 44])
  files[!files %in% .rds[.sizes == 44]]
}

#' Assignment function for golem
#' @param funs List of functions to assign
#' @param ns Package namespace
#' @noRd
do_assignment <- function(funs, ns = pkgload::pkg_name()) {
  if (UU::is_legit(try(ns, silent = TRUE))) {
    namespace <- rlang::ns_env(ns)
    
    purrr::iwalk(funs, ~{
      # Unlock the specific binding if it exists
      if (exists(.y, envir = namespace, inherits = FALSE))
        rlang::env_binding_unlock(namespace, .y)
      
      # Assign to namespace
      assign(.y, .x, envir = namespace)
      assignInNamespace(.y, .x, ns, envir = namespace)
      
      # Lock the binding again
      rlang::env_binding_lock(namespace, .y)
    })
  } else
    funs
}

#' Load Specific File from S3 (supports parquet and rds)
#'
#' @param file_name \code{(character)} Name of the file to load (without path)
#' @param bucket \code{(character)} Name of the S3 bucket
#' @param folder \code{(character)} Folder within the bucket where files are stored
#' @param region \code{(character)} AWS region
#' @return \code{(data.frame)} Data frame containing the loaded data
#' @noRd
load_s3_file <- function(
    file_name,
    bucket = "shiny-data-cohhio",
    folder = "RME",
    region = "us-east-2"
) {
  tryCatch({
    # Construct the full S3 key (path)
    s3_key <- fs::path(folder, file_name)
    
    # Generate local temporary filename
    local_file <- fs::path(
      fs::path_temp(),
      paste0("temp_", file_name)
    )
    
    cli::cli_alert_info("Downloading {file_name} from S3...")
    
    # Download file
    aws.s3::save_object(
      object = s3_key,
      bucket = bucket,
      file = local_file,
      region = region
    )
    
    # Load the file based on extension
    cli::cli_alert_info("Loading data...")
    file_ext <- tools::file_ext(file_name)
    
    data <- switch(tolower(file_ext),
                   "parquet" = arrow::read_parquet(local_file),
                   "rds" = readRDS(local_file),
                   stop("Unsupported file type: ", file_ext, ". Only parquet and rds files are supported.")
    )
    
    # Delete the temporary file
    fs::file_delete(local_file)
    
    # Return the loaded data
    return(data)
    
  }, error = function(e) {
    cli::cli_alert_danger("Error loading {file_name}: {e$message}")
    NULL
  })
}

#' Get S3 file last modified dates
#'
#' @param bucket \code{(character)} S3 bucket name
#' @param folder \code{(character)} S3 folder/prefix  
#' @param region \code{(character)} AWS region
#' @return \code{(POSIXct)} Vector of last modified dates
#' @noRd
get_s3_refresh_date <- function(bucket = "shiny-data-cohhio", 
                                folder = "RME", 
                                region = "us-east-2") {
  tryCatch({
    # Get all objects in the S3 bucket/folder
    s3_objects <- aws.s3::get_bucket(bucket = bucket, prefix = folder, region = region)
    
    # Extract last modified dates, excluding folder entries
    last_modified <- purrr::map_chr(s3_objects, ~.x$LastModified) |>
      as.POSIXct(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    
    # Filter out any NA dates (from folder entries)
    last_modified <- last_modified[!is.na(last_modified)]
    
    if (length(last_modified) == 0) {
      cli::cli_alert_warning("No valid timestamps found in S3")
      return(Sys.time())  # Fallback to current time
    }
    
    # Return the most recent modification time, converted to Eastern Time
    max_time <- max(last_modified)
    # Convert to Eastern Time (handles EDT/EST automatically)
    attr(max_time, "tzone") <- "America/New_York"
    max_time
    
  }, error = function(e) {
    cli::cli_alert_warning("Could not get S3 refresh date: {e$message}")
    # Fallback: try local files if they exist
    if (dir.exists("data")) {
      local_files <- list.files(path = "data", full.names = TRUE)
      if (length(local_files) > 0) {
        purrr::map(local_files, ~file.info(.x)$mtime) |> 
          {\(x) {do.call(c, x)}}() |> 
          max()
      } else {
        Sys.time()
      }
    } else {
      Sys.time()
    }
  })
}

#' Create S3 accessor functions 
#'
#' @param s3_files \code{(character)} Vector of S3 file names  
#' @param local_data \code{(named list)} Named list where names are accessor function names and values are expressions to evaluate
#' @param bucket \code{(character)} S3 bucket name
#' @param folder \code{(character)} S3 folder/prefix
#' @param region \code{(character)} AWS region
#' @noRd
create_accessors_s3 <- function(s3_files = NULL, 
                                local_data = NULL,
                                bucket = "shiny-data-cohhio", 
                                folder = "RME", 
                                region = "us-east-2") {
  
  # If no files specified, list them from S3
  if (is.null(s3_files)) {
    tryCatch({
      s3_objects <- aws.s3::get_bucket(bucket = bucket, prefix = folder, region = region)
      # Extract the Key from each object and get basename
      s3_files <- purrr::map_chr(s3_objects, ~.x$Key) |>
        basename()
      s3_files <- s3_files[s3_files != ""] # Remove folder entries
      cli::cli_alert_info("Found {length(s3_files)} files in S3: {paste(s3_files, collapse = ', ')}")
    }, error = function(e) {
      cli::cli_alert_warning("Could not list S3 files: {e$message}")
      s3_files <- character(0)
    })
  }
  
  # Create S3 accessor functions
  s3_accessor_funs <- list()
  if (length(s3_files) > 0) {
    s3_accessor_funs <- purrr::map(
      rlang::set_names(s3_files, tools::file_path_sans_ext(s3_files)), 
      function(file_name) {
        rlang::new_function(
          args = rlang::pairlist2(
            bucket = bucket,
            folder = folder,
            region = region,
            ... = 
          ),
          body = rlang::expr({
            load_s3_file(!!file_name, bucket, folder, region)
          })
        )
      }
    )
  }
  
  # Create local data accessor functions
  local_accessor_funs <- list()
  if (!is.null(local_data) && length(local_data) > 0) {
    local_accessor_funs <- purrr::map(local_data, function(expr) {
      rlang::new_function(
        args = rlang::pairlist2(... = ),
        body = expr
      )
    })
  }
  
  # Combine all accessor functions
  all_accessor_funs <- c(s3_accessor_funs, local_accessor_funs)
  
  if (length(all_accessor_funs) == 0) {
    cli::cli_alert_warning("No accessor functions created")
    return(invisible(NULL))
  }
  
  # Use the existing do_assignment function
  do_assignment(all_accessor_funs)
  
  cli::cli_alert_success("Created {length(all_accessor_funs)} accessor function{?s}: {paste(names(all_accessor_funs), collapse = ', ')}")
  
  invisible(all_accessor_funs)
}