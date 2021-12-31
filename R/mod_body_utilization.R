

#' mod_body_utilization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
# TODO Need to finish here
mod_body_utilization_ui <- function(id) {
  ns <- shiny::NS(id)
  date_range <-
    names(utilization_clients()) |> stringr::str_subset("[a-zA-Z]{3}\\d{4}") |> lubridate::parse_date_time(orders = "bY") |> sort()
  choices <- utilization_clients() |> 
    dplyr::distinct(ProjectName, ProjectID) |> 
    dplyr::arrange(ProjectName) |> 
    {\(x) {rlang::set_names(dplyr::pull(x, ProjectID), dplyr::pull(x, ProjectName))}}()
  
  shiny::fluidPage(
    ui_header_row(),
    ui_row(
      ui_picker_project(choices = choices),
      shinyWidgets::airDatepickerInput(
        inputId = ns("date_range"),
        label = "Click to Choose a Month",
        maxDate = max(date_range) + lubridate::dmonths(1),
        minDate = min(date_range),
        dateFormat = "MM yyyy",
        view = "month",
        value =
          lubridate::floor_date(rm_dates()$meta_HUDCSV$Export_Date, unit = "month") - lubridate::days(1),
        minView = "months",
        addon = "none",
        autoClose = TRUE,
        width = '50%'
      ),
      width = 12
    ),
    ui_row(
      title = "Summary",
      fluidRow(
      bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
        "infobox_bn_served"
      ), width = '100%')),
      bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
        "infobox_pbn_available"
      ), width = '100%')),
      bs4Dash::column(4, bs4Dash::infoBoxOutput(ns(
        "infobox_utilization"
      ), width = '100%'))
      ),
      width = 12
    ),
    ui_row(
      title = "Detail", 
      DT::dataTableOutput(ns("detail")),
      width = 12
    )
  )
  
}

#' mod_body_utilization Server Functions
#'
#' @noRd 
mod_body_utilization_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$header <- shiny::renderUI(server_header("Bed and Unit Utilization", input$project, format(input$date_range, "%B %Y"), shiny::tags$p(shiny::icon("exclamation-triangle", style = "display:inline-block; color: #ffc107;"), "During this time, congregate facilities should be aiming to deconcentrate. If this causes fluctuations in Utilization, that is okay. Please continue to keep your clients safe.", status = "warning")))
    
    ReportStart <- eventReactive(input$date_range,{
      lubridate::floor_date(input$date_range,
                                   unit = "month")
      })
    ReportEnd <- reactive(ReportStart()  + lubridate::dmonths(1))
    col_nm <- reactive({
      ReportStart() |>
        format(format = "%b%Y")
    })
    
    uc_selected <- reactive({
      utilization_clients() |>
        HMIS::served_between(ReportStart(), ReportEnd()) |> 
        dplyr::filter(ProjectID %in% input$project) |>
        dplyr::mutate(BedStart = dplyr::if_else(ProjectType %in% c(3, 9, 13),
                                                MoveInDateAdjust, EntryDate)) |>
        dplyr::select(UniqueID, BedStart, ExitDate, dplyr::all_of(col_nm()))
    })
    bed_count <- reactive({
      Beds() |> 
        HMIS::beds_available_between(ReportStart(), ReportEnd()) |> 
        dplyr::filter(ProjectID %in% input$project) |>
        dplyr::group_by(ProjectID) |>
        dplyr::summarise(BedCount = sum(BedInventory), .groups = "drop") |>
        dplyr::pull(BedCount)
    })
    
    daysInMonth <- reactive(lubridate::days_in_month(input$date_range))
    
    output$detail <-
      DT::renderDT(server = FALSE, {
        uc_selected() |> 
          rlang::set_names(c("Unique ID", "Bed Start", "Exit Date", paste("Bed Nights in", format(ReportStart(), "%B %Y")))) |> 
          datatable_default(escape = FALSE)
      })
    
    output$infobox_bn_served <-
      bs4Dash::renderInfoBox({
        bs4Dash::infoBox(
          title = "Total Bed Nights Served",
          color = "purple",
          icon = shiny::icon("bed"),
          value = sum(uc_selected()[[col_nm()]]),
          subtitle = "See table below for detail."
        )
      })
    
    
    output$infobox_pbn_available <-
      bs4Dash::renderInfoBox({
        
        bs4Dash::infoBox(
          title = "Possible Bed Nights",
          color = "purple",
          icon = shiny::icon("bed"),
          value = bed_count() * daysInMonth(),
          subtitle = paste(
            "Bed Count:",
            bed_count(),
            "beds x",
            daysInMonth(),
            "days in",
            format(ReportStart(), "%B"),
            "=",
            bed_count() * daysInMonth()
          )
        )
      })
    
    output$infobox_utilization <-
      bs4Dash::renderInfoBox({
        bedUtilization <- scales::percent(sum(uc_selected()[[col_nm()]]) / (bed_count() * daysInMonth()))
        bs4Dash::infoBox(
          title = "Bed Utilization",
          color = "teal",
          icon = shiny::icon("bed"),
          value = bedUtilization,
          subtitle = paste(sum(uc_selected()[[col_nm()]]),
                           "/",
                           bed_count() * daysInMonth(),
                           "=",
                           bedUtilization)
        )
      })
  })
}

## To be copied in the UI
# mod_mod_body_utilization_ui("mod_body_utilization_1")

## To be copied in the server
# mod_mod_body_utilization_server("mod_body_utilization_1")
