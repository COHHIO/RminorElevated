qpr_expr$rrh_placement <- list()
qpr_expr$rrh_placement$expr <- rlang::expr({
  qpr_rrh_enterers() |>
    HMIS::entered_between(input$date_range[1], input$date_range[2]) |> 
    dplyr::filter(!is.na(MoveInDateAdjust) & ProjectName %in% input$region) 
})

qpr_expr$rrh_placement$infobox <- rlang::expr({
  data_env() |>
    dplyr::mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) |>
    dplyr::summarise(AvgDaysToHouse = round(mean(DaysToHouse), 0), .groups = "drop_last") |> 
    qpr_infobox(
      title = "Average Days to House",
      color = "indigo",
      icon = "hourglass-half",
      value = .data$AvgDaysToHouse,
    )
})

qpr_expr$rrh_placement$datatable <- rlang::expr({
  data_env() |>
    dplyr::arrange(dplyr::desc(DaysToHouse)) |>
    dplyr::select(
      UniqueID,
      EntryDate,
      "Move In Date" = MoveInDate,
      "Days to House" = DaysToHouse
    ) |> 
    datatable_default(escape = FALSE)
})

