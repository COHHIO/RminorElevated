qpr_expr$community_need_ph <- list()
qpr_expr$community_need_ph$expr <- rlang::expr({
  req(input$date_range, input$region)
  # used in both summary and detail, filter by inputs
  qpr_spdats_project() |>
    HMIS::entered_between(input$date_range[1],
                          input$date_range[2]) |>
    dplyr::filter(Region %in% input$region)
  
  
})

qpr_expr$community_need_ph$infobox <- rlang::expr({
  req(data_env())
  data_env() |> 
    dplyr::group_by(Region) |>
    dplyr::summarise(AvgScore = round(mean(ScoreAdjusted), 0), .groups = "drop_last") |> 
    qpr_infobox(icon = "parachute-box",
                subtitle = "Households who were Housed in RRH or PSH in the Selected Region")
})

qpr_expr$community_need_ph$datatable <- rlang::expr({
  req(data_env())
  data_env() |> 
    dplyr::arrange(dplyr::desc(ScoreAdjusted)) |>
    dplyr::select(
      Project = ProjectName,
      UniqueID,
      EntryDate,
      "County Served" = CountyServed,
      "Score Date" = ScoreDate,
      Score,
      "Score Adjusted" = ScoreAdjusted
    ) |> 
    datatable_default(escape = FALSE)
})

qpr_expr$community_need_ph$details <- rlang::expr({
  tibble::tibble(
    ProjectType = c(""),
    Goal = c(""),
    HowCalculated = c("")
  ) |>
    DT::datatable(escape = FALSE)
})
