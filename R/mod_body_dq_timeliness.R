#' body_dq_timeliness UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_dq_timeliness_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    ui_header_row(),
    ui_picker_project(choices = desk_time_providers),
    ui_row_box(plotly::plotlyOutput(ns("detail")),
                               title = "Detail"),
    ui_row_box(title = "Guidance", shiny::HTML("
      <h4>HUD and Data Quality</h4>
        <p>HUD defines <em>Data Quality</em> as having three elements:</p>
        <ol>
        <li>Accuracy</li>
        <li>Completeness</li>
        <li>Timeliness</li>
        </ol>
        <p>Data Entry Delay (aka <em>Desk Time</em>) refers to how long it is taking to enter a client into HMIS from the day they enter your project.</p>
        <h4>Ohio Balance of State CoC Data Standards</h4>
        <p>According to the Data Quality Standards for the Ohio Balance of State CoC, all clients should be entered within 5 days of their entry into your project.</p>
        <h4>How Do We Fix This?</h4>
        <p><strong>There is nothing a user can do to 'correct' a client entered into the system outside the window, we can only resolve to enter clients within the 5-day range going forward.</strong> As you catch up on data entry, you may see the median delay gets longer at first, but this data looks back exactly one year, so any clients with an Entry Date over a year ago will fall off of this plot and your median will change accordingly.</p>
        <h4>Interpretation</h4>
        <p><span style='color:forestgreen;'>Green</span> dots here represent clients entered within the range and <span style='color:orangered;'>orange</span> dots represent clients entered outside the range. The darker the dot, the more clients entered your project on that day. (Likely a household.)</p>
        <p>The metric COHHIO looks at here is the Median, so if you have orange dots but your Median is within the 5 day range, that is great!</p>
        <p>If you have orange dots BELOW the 0 mark, that means you entered Entry Dates into the future, which means there is potentially a mis-keyed date or the user needs technical assistance about how to know what date to enter for the Entry Date. If this is the case, please <a href='mailto:hmis@cohhio.org' target='_blank'>email the HMIS team at hmis@cohhio.org</a></p>.
      <h4>Is it possible there's a mistake?</h4>
<p>It's rare that this occurs, but if an Entry Exit has been created, deleted, and then recreated, the Entry Exit's <em>Date Created</em> date is reset, thus inflating the number of days between the Date Created and the Entry Date. If you need us to check if this was the case for a particular dot on the plot, please <a href='mailto:hmis@cohhio.org' target='_blank'>email us</a> and indicate the project and number of days it is displaying that you think may be incorrect so we can verify if this is the issue.</p>"
    ))
  )
}

#' body_dq_timeliness Server Functions
#'
#' @noRd 
mod_body_dq_timeliness_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ReportStart <- Sys.Date() - lubridate::years(1)
    ReportEnd <- Sys.Date()
    output$header <- shiny::renderUI(RminorElevated::server_header("Data Entry Timeliness",
                                                                   x = shiny::h3(paste0("Fixed Date Range: ", ReportStart, " to ", ReportEnd))))
    
    
    project <- shiny::eventReactive(input$project, {input$project}) |> shiny::debounce(1000)
    
    
    
    output$detail <- shiny::renderPlot({
      req(project())
      dt_median <- desk_time_medians |> 
        dplyr::filter(ProjectName == project())
      desk_time <- desk_time |> 
        dplyr::filter(ProjectName == project())
      
      ggplot2::ggplot(
        desk_time,
        ggplot2::aes(y = DeskTime)
      ) +
        ggplot2::geom_linerange(data = desk_time |> dplyr::filter(GoalMet == "orangered"), ggplot2::aes(color = GoalMet, size = 8, alpha = .2, xmin = EntryDate, xmax = DateCreated), show.legend = FALSE) +
        ggplot2::geom_point(data = desk_time |> dplyr::filter(GoalMet == "forestgreen"), ggplot2::aes(color = GoalMet, size = 8, alpha = .2, x = DateCreated), show.legend = FALSE) + 
        ggplot2::scale_color_identity() +
        ggplot2::geom_hline(yintercept = 5, color = "forestgreen") +
        ggplot2::geom_hline(yintercept = dt_median$MedianDeskTime, color = "black") +
        ggplot2::scale_x_date(date_breaks = "1 month", limits = c(ReportStart, ReportEnd), date_labels = "%b %y") + 
        ggplot2::geom_label(x = ReportEnd - lubridate::days(180),
                            y = dt_median$MedianDeskTime,
                            label = paste("Median:", 
                                          dt_median$MedianDeskTime,
                                          "days | Total Clients:",
                                          dt_median$TotalEntered)) +
        ggplot2::geom_label(x = ReportEnd - lubridate::days(300),
                            y = 5,
                            label = "DQ Standards (5d or less)") +
        ggplot2::labs(x = "Entry Date",
                      y = "Data Entry Delay (in days)") +
        ggplot2::theme_minimal(base_size = 18) + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) 
    })
    
  })
}

## To be copied in the UI
# mod_body_dq_timeliness_ui("body_dq_timeliness_1")

## To be copied in the server
# mod_body_dq_timeliness_server("body_dq_timeliness_1")