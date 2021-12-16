#' body_dq_region_level UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_body_dq_region_level_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row(
      ui_picker_project(
        inputId = ns("region"),
        choices = regions,
        width = "70%"
      ),
      ui_date_range(
        start = rm_dates()$hc$check_dq_back_to,
        width = "25%"
      )
    ),
    ui_solid_box(
      title = "Summary",
      DT::dataTableOutput(ns("summary")),
      status = "info"
    )
  )
}

#' body_dq_region_level Server Functions
#'
#' @noRd
mod_body_dq_region_level_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$header <- renderUI({
      server_header(
        "Regional Data Quality",
        region = input$region,
        date_range = input$date_range
      )
    })
    
    region <-
      shiny::eventReactive(input$region, {
        as.numeric(input$region)
      }) |> shiny::debounce(1000)
    
    output$summary <- DT::renderDataTable({
      req(region())
      
      dq_main() |>
        dq_filter_between(date_range = input$date_range,
                          ProjectRegion == region()) |>
        dq_select_cols(ProjectName) |>
        dplyr::group_by(ProjectName, Type, Issue) |>
        dplyr::summarise(Clients = dplyr::n()) |>
        dplyr::select("Program Name" = ProjectName, Type, Issue, Clients) |>
        dplyr::arrange(Type, dplyr::desc(Clients)) |>
        datatable_default()
      
    })
  })
}

## To be copied in the UI
# mod_body_dq_region_level_ui("body_dq_region_level_1")

## To be copied in the server
# mod_body_dq_region_level_server("body_dq_region_level_1")
