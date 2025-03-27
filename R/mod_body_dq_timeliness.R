
dt_time <- validation() |>
  dplyr::filter(ProjectType %in% c(0, 1, 2, 3, 4, 6, 8, 9, 12, 13, 14)) |>
  dplyr::mutate(
    DateCreated = lubridate::as_date(DateCreated, tz = NULL),
    DeskTime = as.integer(
      difftime(DateCreated,
               EntryDate,
               units = "days")
    ),
    GoalMet = dplyr::if_else(DeskTime > 5 |
                               DeskTime < 0,
                             "orangered",
                             "forestgreen")
  ) |>
  dplyr::select(HouseholdID,
                UniqueID,
                ProjectName,
                ProjectID,
                EntryDate,
                ExitDate,
                DateCreated,
                DeskTime,
                GoalMet)

desk_time_providers <- rlang::set_names(dt_time$ProjectID |> unique(), dt_time$ProjectName |> unique())
# DEBUG
# input <- list(date_range = c(lubridate::ymd("2020-01-01"), lubridate::ymd("2022-01-21")),
#               program = "558")
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
    ui_picker_program(choices = desk_time_providers,
                      multiple = FALSE,
                      width = "100%"),
    ui_date_range(start = rm_dates()$hc$check_dq_back_to),
    ui_row(shiny::plotOutput(ns("detail")),
                               title = "Detail"),
    ui_row(title = "Guidance", shiny::HTML("
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
        <p><span style='color:forestgreen;'>Green</span> dots here represent clients entered within the range and <span style='color:orangered;'>orange</span> lines (spanning the Entry Date to the Date the record was created) represent clients entered outside the range. The darker the dot, the more clients entered your program on that day. (Likely a household.)</p>
        <p>The metric COHHIO looks at here is the Median, so if you have orange dots but your Median is within the 5 day range, that is great!</p>
        <p>If you have orange dots BELOW the 0 mark, that means you entered Entry Dates into the future, which means there is potentially a mis-keyed date or the user needs technical assistance about how to know what date to enter for the Entry Date. If this is the case, please <a href='mailto:hmis@cohhio.org' target='_blank'>email the HMIS team at hmis@cohhio.org</a>.</p>
      <h4>Is it possible there's a mistake?</h4>
<p>It's rare that this occurs, but if an Entry Exit has been created, deleted, and then recreated, the Entry Exit's <em>Date Created</em> date is reset, thus inflating the number of days between the Date Created and the Entry Date. If you need us to check if this was the case for a particular dot on the plot, please <a href='mailto:hmis@cohhio.org' target='_blank'>email us</a> and indicate the program and number of days it is displaying that you think may be incorrect so we can verify if this is the issue.</p>"
    ))
  )
}

#' body_dq_timeliness Server Functions
#'
#' @noRd 
mod_body_dq_timeliness_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$header <- shiny::renderUI(RminorElevated::server_header("Data Entry Timeliness"))
    
    
    
    
    desk_time <- eventReactive(c(input$date_range, input$program), {
      req(input$date_range, input$program)
      dq_filter_between(dt_time, date_range = input$date_range, program = input$program)
    })
    
    
    
    
    
    
    
    output$detail <- shiny::renderPlot({
      req(input$program, input$date_range, desk_time())
      
      dt_median <- desk_time() |>
        dplyr::group_by(ProjectName) |>
        dplyr::summarise(MedianDeskTime = stats::median(DeskTime, na.rm = TRUE),
                         TotalEntered = dplyr::n(), .groups = "drop")
      
      p <- ggplot2::ggplot(
        desk_time(),
        ggplot2::aes(y = DeskTime)
      )
      
      label_x <- as.numeric(difftime(input$date_range[2], input$date_range[1], units = "days"))
      label_.2 <- round(label_x * .75)
      label_.5 <- round(label_x * .50)
      if (any(desk_time()$GoalMet == "orangered", na.rm = TRUE))
        p <- p + ggplot2::geom_linerange(data = desk_time() |> dplyr::filter(GoalMet == "orangered"), ggplot2::aes(color = GoalMet, size = 8, alpha = .2, xmin = EntryDate, xmax = DateCreated), show.legend = FALSE)
      if (any(desk_time()$GoalMet == "forestgreen", na.rm = TRUE))
        p <- p + ggplot2::geom_point(data = desk_time() |> dplyr::filter(GoalMet == "forestgreen"), ggplot2::aes(color = GoalMet, size = 8, alpha = .2, x = DateCreated, y = DeskTime), show.legend = FALSE) 
      
        p <- p +
          ggplot2::labs(x = "Entry Date",
                        y = "Data Entry Delay (in days)") +
          ggplot2::theme_minimal(base_size = 18) + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) + 
          ggplot2::scale_color_identity() +
          ggplot2::scale_x_date(date_breaks = ifelse(label_x < 90, "1 week", "1 month"), limits = c(input$date_range[1], input$date_range[2]), date_labels = "%b %y")
        
        for (i in 1:nrow(dt_median)) {
          p <- p + ggplot2::geom_hline(yintercept = 5, color = "forestgreen") +
            ggplot2::geom_hline(yintercept = dt_median[i,]$MedianDeskTime, color = "black") + 
            ggplot2::geom_label(x = input$date_range[2] - label_.2,
                                y = dt_median[i,]$MedianDeskTime,
                                alpha = .5,
                                label = paste("Median:", 
                                              dt_median[i,]$MedianDeskTime,
                                              "days | Total Clients:",
                                              dt_median[i,]$TotalEntered),
                                ) +
            ggplot2::geom_label(x = input$date_range[2] - label_.5,
                                y = 5,
                                label = "DQ Standards (< 5d)")
        }
        p
        
    })
    
  })
}

## To be copied in the UI
# mod_body_dq_timeliness_ui("body_dq_timeliness_1")

## To be copied in the server
# mod_body_dq_timeliness_server("body_dq_timeliness_1")
