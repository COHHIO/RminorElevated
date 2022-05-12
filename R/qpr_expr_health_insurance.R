qpr_expr$health_insurance <- list()
qpr_expr$health_insurance$expr <- rlang::expr({
  qpr_benefits() |>
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
  # input <- list(region = "Richland - Harmony House Homeless Services - HCRP RRH",
  #               date_range = c(lubridate::ymd("2020-01-01"), Sys.Date()))
})

qpr_expr$health_insurance$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |> 
      dplyr::filter(InsuranceFromAnySource == 1) |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(InsuranceAtExit = dplyr::n(), .groups = "drop_last"),
    by = c("ProjectName")
  ) |> 
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
    dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
  
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  title = "Total Households Exiting With Health Insurance",
                  color = "gray-dark",
                  icon = "medkit",
                  value = scales::percent(.data$Percent),
                  subtitle = paste(.data$InsuranceAtExit, 
                                   "out of",
                                   .data$TotalHHs,
                                   "households")
    )
  } else {
    .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})

qpr_expr$health_insurance$datatable <- rlang::expr({
  data_env() |>
    dplyr::mutate(
      InsuranceFromAnySource = HMIS::hud_translations$`1.8 No_Yes_Reasons for Missing Data`(InsuranceFromAnySource)) |>
    dplyr::select(
      UniqueID,
      EntryDate,
      ExitDate,
      "Health Insurance from Any Source (at Exit)" = InsuranceFromAnySource
    ) |> 
    datatable_default(escape = FALSE)
})

