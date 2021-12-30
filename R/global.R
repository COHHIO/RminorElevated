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

# library(tidyverse)
# library(shinydashboard)
# library(shiny)
# library(shinyWidgets)
# library(lubridate)
# library(scales)
# library(plotly)
# library(zoo)
# library(DT)
# library(writexl)
# library(viridis)
# library(HMIS)
# library(feather)
db_auth()
# Create accessor functions
do_assignment(create_accessors("data"))


if (exists("validation")) {
  projects <- validation() |>
    dplyr::distinct(ProjectID, ProjectName) |>
    dplyr::arrange(ProjectName) |> 
      {\(x) {rlang::set_names(x$ProjectID, x$ProjectName)}}()
  
  desk_time <- validation() |>
    HMIS::entered_between(Sys.Date() - lubridate::years(1),
                          Sys.Date()) |>
    dplyr::filter(ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13)) |>
    dplyr::select(ProjectName, UniqueID, HouseholdID, EntryDate, DateCreated) |>
    dplyr::mutate(
      DateCreated = lubridate::as_date(DateCreated, tz = NULL),
      DeskTime = as.integer(
        difftime(DateCreated,
                 EntryDate,
                 units = "days")
      ),
      GoalMet = dplyr::if_else(DeskTime > 5 |
                                 DeskTime < 0,
                               "orangered",
                               "forestgreen")
    ) |>
    dplyr::select(HouseholdID,
                  UniqueID,
                  ProjectName,
                  EntryDate,
                  DateCreated,
                  DeskTime,
                  GoalMet) 
  desk_time_providers <- desk_time$ProjectName |> unique()
  
  desk_time_medians <- desk_time |>
    dplyr::group_by(ProjectName) |>
    dplyr::summarise(MedianDeskTime = stats::median(DeskTime),
              TotalEntered = dplyr::n(), .groups = "drop")  
  
}
  
if (exists("Regions")) {
  regions <- Regions() |> 
    dplyr::distinct(Region, RegionName) |> 
    {\(x) {rlang::set_names(x$Region, x$RegionName)}}()
  counties <- sort(Regions()$County)
}


qpr_tab_choices <- unique(Regions()$RegionName) |>
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

