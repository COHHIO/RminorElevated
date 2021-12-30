#' body_community_need_lh UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_qpr_community_need_lh_ui <- function(id){
  ns <- NS(id)
  mod_qpr_ui(id)
}
    
#' body_community_need_lh Server Functions
#'
#' @noRd 
mod_body_qpr_community_need_lh_server <- function(id){
  moduleServer( id, mod_qpr_server(id, "Community Need, Literally Homeless in the County"))
}
    
## To be copied in the UI
# mod_body_community_need_lh_ui("body_community_need_lh_1")
    
## To be copied in the server
# mod_body_community_need_lh_server("body_community_need_lh_1")
