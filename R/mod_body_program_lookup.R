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
     DT::dataTableOutput(ns("detail"))
   )
  )
}
    
#' body_program_lookup Server Functions
#'
#' @noRd 
mod_body_program_lookup_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  
  program_lookup() |> 
    dplyr::arrange("ProgramName") |> 
    datatable_default(escape = FALSE) |> 
    DT::formatStyle(
      c("AgencyName", "ProgramName"),
      c("AgencyActive", "ProgramActive"),
      target = "cell",
      backgroundColor = DT::styleEqual(FALSE, "#FFB2B6")
    ) -> df
    df |>  datatable_options_update(hide_cols = c("AgencyActive", "ProgramActive"))
    output$header <- renderUI(server_header("Agency & Program Lookup"))
    output$detail <- DT::renderDT(server = FALSE, {
      program_lookup() |> 
        dplyr::arrange("ProgramName") |> 
        datatable_default(escape = FALSE) |> 
        DT::formatStyle(
          c("AgencyName", "ProgramName"),
          c("AgencyActive", "ProgramActive"),
          target = "cell",
          backgroundColor = DT::styleEqual(FALSE, "#FFB2B6")
        ) |> 
        datatable_options_update(hide_cols = c("AgencyActive", "ProgramActive"))
      })
  })
}
    
## To be copied in the UI
# mod_body_program_lookup_ui("body_program_lookup_1")
    
## To be copied in the server
# mod_body_program_lookup_server("body_program_lookup_1")
