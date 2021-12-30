#' body_community_need_ph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_community_need_ph_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_community_need_ph Server Functions
#'
#' @noRd 
mod_body_qpr_community_need_ph_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "Community Need, Entered Permanent Housing"))
}
    
## To be copied in the UI
# mod_body_community_need_ph_ui("body_community_need_ph_1")
    
## To be copied in the server
# mod_body_community_need_ph_server("body_community_need_ph_1")
