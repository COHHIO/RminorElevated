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
.mod_body_utilization_ui <- function(id){
  ns <- NS(id)
  
    fluidPage(
      ui_header_row(),
      fluidRow(
        box(
          title = "NOTICE",
          status = "warning",
          solidHeader = TRUE,
          "During this time, congregate facilities should be aiming to deconcentrate. If this causes fluctuations in Utilization, that is okay. Please continue to keep your clients safe."
          ,
          width = 6
        )
      ),
      fluidRow(box(
        ui_picker_project(),
        airDatepickerInput(
          inputId = "utilizationDate",
          label = "Click to Choose a Month",
          max = ymd(floor_date(rm_dates()$meta_HUDCSV$Export_Date, unit = "month") - days(1)),
          min = ymd(floor_date(ymd(
            rm_dates()$meta_HUDCSV$Export_End
          ), "month") - years(2) + days(1)),
          dateFormat = "MM yyyy",
          view = "month",
          value =
            ymd(floor_date(rm_dates()$meta_HUDCSV$Export_Date, unit = "month") - days(1)),
          minView = "months",
          addon = "none",
          autoClose = TRUE,
          width = '50%'
        ),
        width = 12
      )),
      fluidRow(box(
        infoBoxOutput("utilizationSummary0", width = '100%'),
        infoBoxOutput("utilizationSummary1", width = '100%'),
        infoBoxOutput("utilizationSummary2", width = '100%'),
        width = 12
      )),
      fluidRow(box(
        DT::dataTableOutput("utilizationDetail"), width = 12
      ))
    )
    
}
    
#' mod_body_utilization Server Functions
#'
#' @noRd 
.mod_body_utilization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$headerUtilization <- renderUI({
      list(h2("Bed and Unit Utilization"),
           h4(input$providerListUtilization),
           h4(format(ymd(
             input$utilizationDate
           ), "%B %Y"))
      )
    })
    output$utilizationDetail <-
      DT::renderDataTable({
        ReportStart <-
          format(floor_date(ymd(input$utilizationDate),
                            unit = "month"), "%m-%d-%Y")
        ReportEnd <-
          format(floor_date(ymd(input$utilizationDate) + days(31),
                            unit = "month") - days(1),
                 "%m-%d-%Y")
        
        y <- paste0(substr(input$utilizationDate, 6, 7),
                    "01",
                    substr(input$utilizationDate, 1, 4))
        
        z <-
          paste("Bed Nights in", format(ymd(input$utilizationDate), "%B %Y"))
        # input <- list(providerListUtilization = sample(c(sort(utilization_bed()$ProjectName)), 1))
        a <- utilizers_clients() %>%
          filter(
            ProjectName == input$providerListUtilization,
            served_between(., ReportStart, ReportEnd)
          ) %>%
          mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                    MoveInDate, EntryDate),
                 PersonalID = as.character(PersonalID)) %>%
          select(PersonalID, BedStart, ExitDate, all_of(y))
        
        colnames(a) <- c("Client ID", "Bed Start", "Exit Date", z)
        
        datatable(a,
                  rownames = FALSE,
                  filter = 'top',
                  options = list(dom = 'ltpi'))
        
      })
    
    output$utilizationSummary0 <-
      renderInfoBox({
        ReportStart <-
          format(floor_date(ymd(input$utilizationDate),
                            unit = "month"), "%m-%d-%Y")
        ReportEnd <-
          format(floor_date(ymd(input$utilizationDate) + days(31),
                            unit = "month") - days(1),
                 "%m-%d-%Y")
        
        y <- paste0(substr(input$utilizationDate, 6, 7),
                    "01",
                    substr(input$utilizationDate, 1, 4))
        
        a <- utilizers_clients() %>%
          filter(
            ProjectName == input$providerListUtilization,
            served_between(., ReportStart, ReportEnd)
          ) %>%
          mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                    MoveInDate, EntryDate)) %>%
          select(PersonalID, BedStart, ExitDate, all_of(y))
        
        colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
        
        beds <- Beds() %>%
          filter(ProjectName == input$providerListUtilization &
                   beds_available_between(., ReportStart, ReportEnd)) %>%
          group_by(ProjectID) %>%
          summarise(BedCount = sum(BedInventory)) %>%
          ungroup() %>%
          pull(BedCount)
        
        daysInMonth <- days_in_month(ymd(input$utilizationDate))
        
        infoBox(
          title = "Total Bed Nights Served",
          color = "purple",
          icon = icon("bed"),
          value = sum(a$BNs),
          subtitle = "See table below for detail."
        )
      })
    
    output$utilizationSummary1 <-
      renderInfoBox({
        ReportStart <-
          format(floor_date(ymd(input$utilizationDate),
                            unit = "month"), "%m-%d-%Y")
        ReportEnd <-
          format(floor_date(ymd(input$utilizationDate) + days(31),
                            unit = "month") - days(1),
                 "%m-%d-%Y")
        
        y <- paste0(substr(input$utilizationDate, 6, 7),
                    "01",
                    substr(input$utilizationDate, 1, 4))
        
        a <- utilizers_clients() %>%
          filter(
            ProjectName == input$providerListUtilization,
            served_between(., ReportStart, ReportEnd)
          ) %>%
          mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                    MoveInDate, EntryDate)) %>%
          select(PersonalID, BedStart, ExitDate, all_of(y))
        
        colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
        
        beds <- Beds() %>%
          filter(ProjectName == input$providerListUtilization &
                   beds_available_between(., ReportStart, ReportEnd)) %>%
          group_by(ProjectID) %>%
          summarise(BedCount = sum(BedInventory)) %>%
          ungroup() %>%
          pull(BedCount)
        
        # units <- Utilization %>%
        #   filter(ProjectName == input$providerListUtilization) %>%
        #   select(UnitCount)
        
        daysInMonth <- days_in_month(ymd(input$utilizationDate))
        
        infoBox(
          title = "Possible Bed Nights",
          color = "purple",
          icon = icon("bed"),
          value = beds * daysInMonth,
          subtitle = paste(
            "Bed Count:",
            beds,
            "beds ×",
            daysInMonth,
            "days in",
            format(ymd(input$utilizationDate), "%B"),
            "=",
            beds * daysInMonth
          )
        )
      })
    
    output$utilizationSummary2 <-
      renderInfoBox({
        ReportStart <-
          format(floor_date(ymd(input$utilizationDate),
                            unit = "month"), "%m-%d-%Y")
        ReportEnd <-
          format(floor_date(ymd(input$utilizationDate) + days(31),
                            unit = "month") - days(1),
                 "%m-%d-%Y")
        
        y <- paste0(substr(input$utilizationDate, 6, 7),
                    "01",
                    substr(input$utilizationDate, 1, 4))
        
        a <- utilizers_clients() %>%
          filter(
            ProjectName == input$providerListUtilization,
            served_between(., ReportStart, ReportEnd)
          ) %>%
          mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                    MoveInDate, EntryDate)) %>%
          select(PersonalID, BedStart, ExitDate, all_of(y))
        
        colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
        
        beds <- Beds() %>%
          filter(ProjectName == input$providerListUtilization &
                   beds_available_between(., ReportStart, ReportEnd)) %>%
          group_by(ProjectID) %>%
          summarise(BedCount = sum(BedInventory)) %>%
          ungroup() %>%
          pull(BedCount)
        
        daysInMonth <-
          as.numeric(days_in_month(ymd(input$utilizationDate)))
        
        bedUtilization <- percent(sum(a$BNs) / (beds * daysInMonth))
        
        infoBox(
          title = "Bed Utilization",
          color = "teal",
          icon = icon("bed"),
          value = bedUtilization,
          subtitle = paste(sum(a$BNs),
                           "÷",
                           beds * daysInMonth,
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
