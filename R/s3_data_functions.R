# # Configuration
# S3_BUCKET <- "shiny-data-cohhio"
# S3_FOLDER <- "RME"
# 
# # List of all your data objects (just the base names without .parquet)
# DATA_OBJECTS <- c(
#   "dq_overlaps", "dq_main", "dq_APs", "dq_eligibility_details", 
#   "dq_for_pe", "dq_aps_no_referrals", "dq_past_year", 
#   "dq_providers_df", "current_tay_hohs", "enrollment_small", 
#   "mental_health_unsheltered", "path_referrals", "prioritization", 
#   "project_small", "qpr_benefits", "qpr_income", 
#   "qpr_leavers", "qpr_reentries", "qpr_rrh_enterers", 
#   "qpr_spending", "utilization_bed", "utilization_beds", 
#   "utilization_unit", "utilization", "validation", 
#   # "veteran_active_list", "vets_current", "vets_housed"
# )
# 
# # Generic wrapper function that uses your existing HMISdata function
# load_hmis_data <- function(object_name, use_cache = TRUE) {
#   filename <- paste0(object_name, ".parquet")
#   cache_key <- paste0(".cache_", object_name)
#   
#   # Check cache first if enabled
#   if (use_cache && exists(cache_key, envir = .GlobalEnv)) {
#     return(get(cache_key, envir = .GlobalEnv))
#   }
#   
#   # Load using your existing function
#   tryCatch({
#     data <- HMISdata::load_hmis_parquet(filename, bucket = S3_BUCKET, folder = S3_FOLDER)
#     
#     # Cache the data if requested
#     if (use_cache) {
#       assign(cache_key, data, envir = .GlobalEnv)
#     }
#     
#     return(data)
#   }, error = function(e) {
#     stop("Failed to load ", object_name, " from S3: ", e$message)
#   })
# }
# 
# # Dynamically create all your data functions
# for (obj_name in DATA_OBJECTS) {
#   assign(obj_name, local({
#     name <- obj_name  # Capture the current object name
#     function(use_cache = TRUE) {
#       load_hmis_data(name, use_cache)
#     }
#   }), envir = .GlobalEnv)
# }
# 
# # Utility functions for cache management
# clear_cache <- function(object_name = NULL) {
#   if (is.null(object_name)) {
#     # Clear all cache
#     cache_objects <- ls(envir = .GlobalEnv, pattern = "^\\.cache_")
#     if (length(cache_objects) > 0) {
#       rm(list = cache_objects, envir = .GlobalEnv)
#       cat("Cleared", length(cache_objects), "cached objects\n")
#     } else {
#       cat("No cached objects found\n")
#     }
#   } else {
#     cache_key <- paste0(".cache_", object_name)
#     if (exists(cache_key, envir = .GlobalEnv)) {
#       rm(list = cache_key, envir = .GlobalEnv)
#       cat("Cleared cache for", object_name, "\n")
#     } else {
#       cat("No cache found for", object_name, "\n")
#     }
#   }
# }
# 
# preload_data <- function(objects = NULL) {
#   if (is.null(objects)) {
#     objects <- DATA_OBJECTS
#   }
#   
#   cat("Preloading data from S3...\n")
#   success_count <- 0
#   
#   for (obj in objects) {
#     cat("  Loading", obj, "...")
#     tryCatch({
#       load_hmis_data(obj, use_cache = TRUE)
#       cat(" ✓\n")
#       success_count <- success_count + 1
#     }, error = function(e) {
#       cat(" ✗ (", e$message, ")\n")
#     })
#   }
#   
#   cat("Preloading complete! Successfully loaded", success_count, "of", length(objects), "objects.\n")
# }
# 
# get_cache_status <- function() {
#   cache_objects <- ls(envir = .GlobalEnv, pattern = "^\\.cache_")
#   cached_names <- gsub("^\\.cache_", "", cache_objects)
#   
#   status <- data.frame(
#     object = DATA_OBJECTS,
#     currently_cached = DATA_OBJECTS %in% cached_names,
#     cache_size_mb = sapply(DATA_OBJECTS, function(obj) {
#       cache_key <- paste0(".cache_", obj)
#       if (exists(cache_key, envir = .GlobalEnv)) {
#         round(object.size(get(cache_key, envir = .GlobalEnv)) / (1024^2), 2)
#       } else {
#         NA
#       }
#     }),
#     stringsAsFactors = FALSE
#   )
#   
#   return(status)
# }
# 
# # Optional: Preload critical data on startup
# preload_on_startup <- function(critical_objects = c("dq_main", "dq_overlaps")) {
#   if (interactive()) {
#     cat("Preloading critical data for app startup...\n")
#     preload_data(critical_objects)
#   }
# }