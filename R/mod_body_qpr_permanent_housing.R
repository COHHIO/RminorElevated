#' body_permanent_housing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_permanent_housing_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_permanent_housing Server Functions
#'
#' @noRd 
mod_body_qpr_permanent_housing_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "Successful Placement Detail"))
}
    
## To be copied in the UI
# mod_body_permanent_housing_ui("body_permanent_housing_1")
    
## To be copied in the server
# mod_body_permanent_housing_server("body_permanent_housing_1")
