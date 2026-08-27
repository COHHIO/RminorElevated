#' body_program_lookup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_body_program_lookup_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row(
      tags$ul(
        tags$li(tags$span(style = "color:#FFB2B6", "Red"), " Program or Agency Name indicates it's inactive.")
      ),
      mod_dt_download_ui(ns("dl_detail")),
      DT::dataTableOutput(ns("detail"))
    )
  )
}

#' body_program_lookup Server Functions
#'
#' @noRd
mod_body_program_lookup_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$header <- renderUI(server_header("Agency & Program Lookup"))

    lookup_filtered <- reactive({
      get_app_data("program_lookup") |>
        dplyr::arrange(ProgramName)
    })

    output$detail <- DT::renderDT(server = TRUE, {
      lookup_filtered() |>
        datatable_default(escape = FALSE, export_buttons = FALSE) |>
        DT::formatStyle(
          c("AgencyName", "ProgramName"),
          c("AgencyActive", "ProgramActive"),
          target = "cell",
          backgroundColor = DT::styleEqual(FALSE, "#FFB2B6")
        ) |>
        datatable_options_update(hide_cols = c("AgencyActive", "ProgramActive",
                                               "StartDate", "EndDate",
                                               "LastUpdatedDate"))
    })

    mod_dt_download_server(
      "dl_detail",
      data = reactive(
        lookup_filtered() |>
          dplyr::select(-dplyr::any_of(c("AgencyActive", "ProgramActive")))
      ),
      filename_prefix = "program_lookup"
    )
  })
}

## To be copied in the UI
# mod_body_program_lookup_ui("body_program_lookup_1")
## To be copied in the server
# mod_body_program_lookup_server("body_program_lookup_1")
