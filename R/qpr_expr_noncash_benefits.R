qpr_expr$noncash_benefits <- list()
qpr_expr$noncash_benefits$expr <- rlang::expr({
  req(input$date_range, input$region)
  qpr_benefits() |>
    HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
})

qpr_expr$noncash_benefits$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |> 
      dplyr::filter(BenefitsFromAnySource == 1) |> 
      dplyr::group_by(ProjectName) |>
      dplyr::summarise(BenefitsAtExit = dplyr::n(), .groups = "drop_last"),
    by = "ProjectName"
  ) |> 
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
    dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                icon = "shopping-cart",
                color = "fuchsia",
                value = scales::percent(.data$Percent),
                title = "Households Exiting With Non Cash Benefits",
                subtitle = paste(.data$BenefitsAtExit, 
                                 "out of",
                                 .data$TotalHHs, 
                                 "households")
    )
  } else {
    .args <- list(title = "No Leavers in the Date Range", .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})

qpr_expr$noncash_benefits$datatable <- rlang::expr({
  data_env() |>
    dplyr::mutate(
      BenefitsFromAnySource = HMIS::hud_translations$`1.8 NoYesReasons for Missing Data`(BenefitsFromAnySource)
    ) |>
    dplyr::select(
      UniqueID,
      EntryDate,
      ExitDate,
      "Benefits from Any Source (at Exit)" = BenefitsFromAnySource
    ) |> 
    datatable_default(escape = FALSE)
})

