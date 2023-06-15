#' body_mahoning_performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_mahoning_performance_ui <- function(id){
  ns <- NS(id)
  Dates = if (!isFALSE(date_choices)) list(
    inputId = ns("date_range"),
    start = lubridate::floor_date(lubridate::as_date(.qbegin - lubridate::dmonths(4)), "quarter"),
    end = .qbegin
  )
}
    
#' body_mahoning_performance Server Functions
#'
#' @noRd 
mod_body_mahoning_performance_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    qpr_leavers() |> 
      HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
      dplyr::filter(((
        !is.na(MoveInDateAdjust) & ProjectType == 13
      ) |
        (
          !is.na(ExitDate) & ProjectType %in% c(1, 2, 8)
        ))
      )
  })
}
    
## To be copied in the UI
# mod_body_mahoning_performance_ui("body_mahoning_performance_1")
    
## To be copied in the server
# mod_body_mahoning_performance_server("body_mahoning_performance_1")
