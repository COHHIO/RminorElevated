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
#' @include accessor_fn_utils.R utils_helpers.R
Sys.setenv(TZ = "America/New_York")


db_auth()
# Create accessor functions
do_assignment(create_accessors("data"))


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
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(1, 2, 8, 13)])
      ),
      permanent_housing = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(1:4, 8:9, 12:13)])
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
      rrh_spending = list(
        choices = unique(sort(
          qpr_spending()$OrganizationName
        ))
      )
    )}}()
  
}



