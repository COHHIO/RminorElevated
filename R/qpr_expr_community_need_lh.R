qpr_expr$community_need_lh <- list()
qpr_expr$community_need_lh$expr <- rlang::expr({
  req(input$date_range, input$region)
  # counting all households who were scored AND SERVED between the report dates
  qpr_spdats_county() |>
    HMIS::served_between(input$date_range[1],
                   input$date_range[2]) |>
    dplyr::left_join(Regions(), by = c("CountyServed" = "County")) |>
    dplyr::filter(RegionName == input$region)
  
})

qpr_expr$community_need_lh$infobox <- rlang::expr({
  data_env() |>
    dplyr::group_by(RegionName) |>
    dplyr::summarise(AvgScore = round(mean(Score), 0), .groups = "drop_last") |> 
    qpr_infobox(icon = "shoe-prints",
                subtitle = "Literally Homeless Households in the Selected Region"
    )
})

qpr_expr$community_need_lh$datatable <- rlang::expr({
  data_env() |> 
    dplyr::select(
      Project = ProjectName,
      UniqueID,
      EntryDate,
      ExitDate,
      "County Served" = CountyServed,
      Score
    ) |>
    dplyr::arrange(dplyr::desc(Score)) |> 
    datatable_default(escape = FALSE)
})

