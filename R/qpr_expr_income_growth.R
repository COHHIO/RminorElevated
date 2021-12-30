

qpr_expr$income_growth <- list()

qpr_expr$income_growth$expr <- rlang::expr({
  qpr_income() |>
    HMIS::stayed_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(ProjectName == input$region)
})

qpr_expr$income_growth$infobox <- rlang::expr({
  .data <- dplyr::left_join(
    # all_hhs
    data_env() |> 
      dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
      dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
    # meeting_objective
    data_env() |> 
      dplyr::filter(Difference > 0) |> 
      dplyr::group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) |>
      dplyr::summarise(Increased = dplyr::n(), .groups = "drop_last"),
    by = c("ProjectName", "ProjectType", "ProjectCounty", "ProjectRegion")
  ) |> 
    {\(x) {tidyr::replace_na(x, setNames(as.list(rep(0, length(x))), nm = names(x)))}}() |> 
    dplyr::mutate(Percent = Increased / TotalHHs)
  if (nrow(.data) > 0) {
    .args <- list(.data = .data,
                  title = "Households Increasing Their Income",
                  color = "green",
                  icon = "hand-holding-usd",
                  value = scales::percent(.data$Percent),
                  subtitle = paste(.data$Increased, 
                                   "out of",
                                   .data$TotalHHs, 
                                   "households served")
    )
  } else {
    .args <- list(title = HTML("Something's wrong- email us at <a href='mailto:hmis@cohhio.org' target='_blank'>hmis@cohhio.org</a>!"), .replace = TRUE)
  }
  
  do.call(qpr_infobox, .args)
})

qpr_expr$income_growth$datatable <- rlang::expr({
  data_env() |>
    dplyr::mutate(EntryIncome = scales::dollar(EntryIncome, accuracy = .01),
                  RecentIncome = scales::dollar(RecentIncome, accuracy = .01),
                  Difference = scales::dollar(Difference, accuracy = .01)) |>
    dplyr::select(
      UniqueID,
      EntryDate,
      ExitDate,
      "Income at Entry" = EntryIncome,
      "Most Recent Income" = RecentIncome,
      "Increase Amount" = Difference
    ) |> 
    datatable_default(escape = FALSE)
})

