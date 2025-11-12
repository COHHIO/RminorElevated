#' news UI Function
#'
#' @description A shiny Module for displaying NEWS.md
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_news_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::box(
        width = 12,
        title = "News & Updates",
        status = "info",
        solidHeader = TRUE,
        includeMarkdown("NEWS.md")
      )
    )
  )
}

#' news Server Functions
#'
#' @noRd 
mod_body_news_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # No server logic needed for static markdown display
  })
}