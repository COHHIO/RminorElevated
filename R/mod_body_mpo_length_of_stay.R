#' body_length_of_stay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_mpo_length_of_stay_ui <- function(id){
  ns <- NS(id)
  mod_mpo_ui(id)
}

#' body_length_of_stay Server Functions
#'
#' @noRd 
mod_body_mpo_length_of_stay_server <- function(id){
  moduleServer( id, mod_mpo_server(id, "Length of Stay"))
}

## To be copied in the UI
# mod_body_length_of_stay_ui("body_length_of_stay_1")

## To be copied in the server
# mod_body_length_of_stay_server("body_length_of_stay_1")
