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
    ui_header_row(),
    ui_row(title = "Access Points", dq_APs() |> 
                 {\(x) {bs4Dash::bs4MultiProgressBar(value = x$percent, min = 0, max = 1, status = c("danger", "success"), striped = c(TRUE, FALSE), animated = rep(FALSE, length(x$percent)), label = paste0(x$category, ": ", x$count), style= "height: 3rem;font-size: 1.5rem;")}}()
               )
    
  )
}
    
#' body_dq_system_wide Server Functions
#'
#' @noRd 
mod_body_dq_system_wide_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- output$header <- renderUI({
      server_header(title = "Data Quality",
                    shiny::h3("System-Wide"),
                    date_range = c(rm_dates()$hc$check_dq_back_to, Sys.Date()))
    })
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
