#' Apply clarity-link decoration to a single dataset
#'
#' Adds linked-ID columns via [clarity.looker::make_linked_df()] when the
#' dataset has the columns that identify a linkable ID (PersonalID +
#' UniqueID, or PersonalID + EnrollmentID). Non-data-frame or malformed
#' inputs are returned unchanged. Extracted from the list-mapping body of
#' [add_clarity_links()] so it can be called per-dataset, on demand, from
#' [get_app_data()] rather than eagerly over the whole dataset list at boot.
#'
#' @param df A single dataset (typically a data frame).
#' @return `df`, decorated with linked-ID columns if applicable; otherwise
#'   `df` unchanged.
#' @noRd
add_clarity_links_df <- function(df) {
  if (!is.data.frame(df) || !UU::is_legit(names(df))) {
    return(df)
  }

  linked_df <- df

  # Check for PersonalID + UniqueID combination
  if (all(c("PersonalID", "UniqueID") %in% names(df))) {
    linked_df <- clarity.looker::make_linked_df(linked_df, UniqueID)
  }
  # Check for PersonalID + EnrollmentID combination
  if (all(c("PersonalID", "EnrollmentID") %in% names(df))) {
    linked_df <- clarity.looker::make_linked_df(linked_df, EnrollmentID)
  }

  linked_df
}