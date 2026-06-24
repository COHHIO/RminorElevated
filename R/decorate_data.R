# Function to apply data linking (extracted from deps_to_destination logic)
add_clarity_links <- function(data_list) {
  cli::cli_alert_info("Applying data linking to datasets...")
  
  linked_data <- purrr::map(data_list, function(df) {
    if (!is.data.frame(df) || !UU::is_legit(names(df))) {
      return(df)
    }
    
    # Apply linking logic from deps_to_destination
    linked_df <- df
    
    # Check for PersonalID + UniqueID combination
    if (all(c("PersonalID", "UniqueID") %in% names(df))) {
      linked_df <- clarity.looker::make_linked_df(linked_df, UniqueID)
    }
    # Check for PersonalID + EnrollmentID combination
    if (all(c("PersonalID", "EnrollmentID") %in% names(df))) {
      linked_df <- clarity.looker::make_linked_df(linked_df, EnrollmentID)
    }
    
    return(linked_df)
  })
  
  return(linked_data)
}