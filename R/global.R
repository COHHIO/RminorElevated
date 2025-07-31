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

# Create the accessor functions
cli::cli_alert_info("Initializing S3 accessor functions...")
.time <- system.time({
  create_accessors_s3(local_data = list(
    Regions = rlang::expr(HMISdata::Regions),
    rm_dates = rlang::expr(HMISprep::load_dates()),
    co_clients_served = HMISdata::load_hmis_parquet("co_clients_served.parquet"),
    program_lookup = HMISdata::load_hmis_parquet("program_lookup.parquet"))
  )
})
  
cli::cli_alert_success("S3 accessors created in {(.time['elapsed'])} seconds")

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



