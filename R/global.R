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

# Get app data
APP_DATA <- load_app_data()

# Get refresh timestamp
APP_META <- list(
  refresh_time = get_s3_refresh_date(),
  loaded_at = Sys.time()
)

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

#' @title Living Situation Reference Number Translation `r lifecycle::badge("deprecated")`
#' @description Deprecated in favor of `hud_translations`. Return a human-readable living situation character vector provided with an integer reference number
#' @param ReferenceNo \code{(integer)} Reference number for living situation type
#' @return \code{(character)} Human-readable living situation type
#' @export
living_situation <- function(ReferenceNo) {
  dplyr::case_when(
    # Homeless Situations (100-199)
    ReferenceNo == 116 ~ "Place not meant for habitation",
    ReferenceNo == 101 ~ "Emergency shelter, including hotel or motel paid for with emergency shelter voucher, Host Home shelter",
    ReferenceNo == 118 ~ "Safe Haven",
    # Institutional Situations (200-299)
    ReferenceNo == 215 ~ "Foster care home or foster care group home",
    ReferenceNo == 206 ~ "Hospital or other residential non-psychiatric medical facility",
    ReferenceNo == 207 ~ "Jail, prison, or juvenile detention facility",
    ReferenceNo == 225 ~ "Long-term care facility or nursing home",
    ReferenceNo == 204 ~ "Psychiatric hospital or other psychiatric facility",
    ReferenceNo == 205 ~ "Substance abuse treatment facility or detox center",
    # Temporary Housing Situations (300-399)
    ReferenceNo == 302 ~ "Transitional housing for homeless persons (including homeless youth)",
    ReferenceNo == 329 ~ "Residential project or halfway house with no homeless criteria",
    ReferenceNo == 314 ~ "Hotel or motel paid for without emergency shelter voucher",
    ReferenceNo == 332 ~ "Host Home (non-crisis)",
    ReferenceNo == 312 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 313 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 327 ~ "Moved from one HOPWA funded project to HOPWA TH",
    ReferenceNo == 336 ~ "Staying or living in a friend's room, apartment, or house",
    ReferenceNo == 335 ~ "Staying or living in a family member's room, apartment, or house",
    # Permanent Housing Situations (400-499)
    ReferenceNo == 422 ~ "Staying or living with family, permanent tenure",
    ReferenceNo == 423 ~ "Staying or living with friends, permanent tenure",
    ReferenceNo == 426 ~ "Moved from one HOPWA funded project to HOPWA PH",
    ReferenceNo == 410 ~ "Rental by client, no ongoing housing subsidy",
    ReferenceNo == 435 ~ "Rental by client, with ongoing housing subsidy",
    ReferenceNo == 421 ~ "Owned by client, with ongoing housing subsidy",
    ReferenceNo == 411 ~ "Owned by client, no ongoing housing subsidy",
    # Other (1-99)
    ReferenceNo == 30 ~ "No exit interview completed",
    ReferenceNo == 17 ~ "Other",
    ReferenceNo == 24 ~ "Deceased",
    ReferenceNo == 37 ~ "Worker unable to determine",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client prefers not to answer",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

