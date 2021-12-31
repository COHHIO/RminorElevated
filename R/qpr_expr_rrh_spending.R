qpr_expr$rrh_spending <- list()
spending_filter <- function(x, date_range, region, ProjectType) {
  x |> 
    HMIS::entered_between(date_range[1], date_range[2]) |> 
  dplyr::filter(
    OrganizationName == region &
      ProjectType == {{ProjectType}}
  )
}
qpr_expr$rrh_spending$expr <- rlang::expr({
  .data <- qpr_spending()
  list(
    rrh = spending_dt(spending_filter(.data, input$date_range, input$region, 13)),
    hp = spending_dt(spending_filter(.data, input$date_range, input$region, 12))
    )
})

qpr_expr$rrh_spending$infobox <- list(
  rlang::expr({
    qpr_infobox(
      data_env(),
      title = "RRH Spending",
      color = "success",
      value = scales::dollar(sum(.data$rrh$Amount)) 
    )
  }),
  rlang::expr({
    qpr_infobox(
      data_env(),
      title = "HP Spending",
      color = "success",
      value = scales::dollar(sum(.data$hp$Amount))
    )
  })
)

spending_dt <- function(x) {
  x |> 
    dplyr::mutate(ProjectName = as.factor(ProjectName)) |>
    dplyr::group_by(UniqueID, ProjectName, ServiceStartDate, ServiceItemName) |> 
    dplyr::summarize(Amount = sum(ServiceAmount)) |> 
    dplyr::select(
      UniqueID,
      "Project Name" = ProjectName,
      "Service Date" = ServiceStartDate,
      Description = ServiceItemName,
      Amount
    )
    
}

qpr_expr$rrh_spending$datatable <- list(
  `Rapid Rehousing Spending` = rlang::expr({
    datatable_default(data_env()$rrh, escape = FALSE)
  }),
  `Homeless Prevention Spending` = rlang::expr({
    datatable_default(data_env()$hp, escape = FALSE)
  })
)