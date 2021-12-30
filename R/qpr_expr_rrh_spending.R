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
  browser()
  list(
    rrh = spending_filter(.data, input$date_range, input$region, 13),
    hp = spending_filter(.data, input$date_range, input$region, 12)
    )
})

spending_dt <- function(x) {
  x |> 
    dplyr::mutate(ProjectName = as.factor(ProjectName)) |>
    dplyr::select(
      UniqueID,
      "Project Name" = ProjectName,
      "Service Date" = ServiceStartDate,
      Description = ServiceItemName,
      Amount = ServiceAmount
    ) |> 
    datatable_default(escape = FALSE)
}

qpr_expr$rrh_spending$datatable <- list(
  `Rapid Rehousing Spending` = rlang::expr({
    spending_dt(data_env()$rrh)
  }),
  `Homeless Prevention Spending` = rlang::expr({
    spending_dt(data_env()$hp)
  })
)