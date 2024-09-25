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

.time <- system.time({
  maleta::create_accessors("data")
})
# Create accessor functions



rm_dates()

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
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") & ProjectType %in% c(0, 1, 2, 8, 13)) |> 
          dplyr::pull(ProjectName) |> unique()
      ),
      length_of_stay_youth = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |> 
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") & ProjectType %in% c(0, 1, 2, 8, 13)) |> 
          dplyr::pull(ProjectName) |> unique()
      ),
      permanent_housing = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") &
                          ProjectType %in% c(0:4, 8:9, 12:13)) |> 
          dplyr::pull(ProjectName) |> unique()
      ),
      permanent_housing_youth = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") &
                          ProjectType %in% c(0:4, 8:9, 12:13)) |> 
          dplyr::pull(ProjectName) |> unique()
      ),
      temp_permanent_housing = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") &
                          ProjectType == 4) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      temp_permanent_housing_youth = list(
        choices = qpr_leavers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp") &
                          ProjectType == 4) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      noncash_benefits = list(
        choices = qpr_benefits() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      noncash_benefits_youth = list(
        choices = qpr_benefits() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      health_insurance = list(
        choices = qpr_benefits() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      health_insurance_youth = list(
        choices = qpr_benefits() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      income_growth = list(
        choices = qpr_income() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      income_growth_youth = list(
        choices = qpr_income() |> 
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      rrh_placement = list(
        choices = qpr_rrh_enterers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(!stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      rrh_placement_youth = list(
        choices = qpr_rrh_enterers() |>
          dplyr::filter(ProgramCoC == "OH-507") |>
          dplyr::filter(stringr::str_detect(tolower(ProjectName), "odh|youth|yhdp")) |>
          dplyr::pull(ProjectName) |> unique()
      ),
      reentries = list(
        choices = unique(sort(
          qpr_reentries() |>
            dplyr::filter(ProgramCoC == "OH-507") |>
            dplyr::filter(!stringr::str_detect(tolower(ExitingHP), "odh|youth|yhdp")) |> 
            dplyr::pull(ExitingHP) |> unique()
        ))
      ),
      rrh_spending = list(
        choices = unique(sort(
          qpr_spending() |>
            dplyr::filter(ProgramCoC == "OH-507") |>
            dplyr::filter(!stringr::str_detect(tolower(OrganizationName), "odh|youth|yhdp")) |> 
            dplyr::pull(OrganizationName) |> unique()
        ))
      )
    )}}()
  
}



