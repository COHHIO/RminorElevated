#' body_dq_system_wide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_dq_system_wide_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' body_dq_system_wide Server Functions
#'
#' @noRd 
mod_body_dq_system_wide_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$desk_time_medians <- renderPlot({
      ggplot(
        head(desk_time_medians, 10L),
        aes(
          x = reorder(ProjectName, MedianDeskTime),
          y = MedianDeskTime,
          fill = MedianDeskTime
        )
      ) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = "",
             y = "Median Days") +
        scale_fill_viridis_c(direction = -1) +
        theme_minimal(base_size = 18)
    })
  })
}
    
## To be copied in the UI
# mod_body_dq_system_wide_ui("body_dq_system_wide_1")
    
## To be copied in the server
# mod_body_dq_system_wide_server("body_dq_system_wide_1")
