#' body_prioritization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
.mod_body_prioritization_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_picker_project(
      label = "Select County/-ies",
      inputId = "region",
      multiple = TRUE,
      choices = stringr::str_subset(dplyr::arrange(Regions())$County, "^Mahoning", negate = TRUE),
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        liveSearchStyle = 'contains',
        actionsBox = TRUE
      )
    ),
    ui_row_box(title = "Active List", 
               DT::dataTableOutput(ns("summary")),
               footer = "Dark gray cells mean the client has a Data Quality issue that may be causing incorrect information to show.")
  )
}
    
#' body_prioritization Server Functions
#'
#' @noRd 
.mod_body_prioritization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
   output$header <- renderUI(server_header("Prioritization Report",
                              paste0("Literally Homeless Clients as of ", rm_dates()$meta_HUDCSV$Export_End)))
   output$summary <- DT::renderDataTable({
     active <- active_list() %>%
       filter(CountyServed %in% c(input$region) |
                is.na(CountyServed)) %>%
       arrange(C19Priority) %>%
       select(
         "HoH Client ID" = PersonalID,
         "Project Name" = ProjectName,
         "Entry Date" = EntryDate,
         "County" = CountyServed,
         "Current Situation (Entry, Referral, Perm Housing Track)" = Situation,
         "COVID-19: Priority for Immediate Non-congregate Housing" = C19Priority,
         "Veteran" = VeteranStatus,
         "Fleeing DV" = CurrentlyFleeing,
         "Transition Aged Youth" = TAY,
         "Chronic Status" = ChronicStatus,
         "Eligible for PSH (Disability in Household)" = DisabilityInHH,
         "Household Size" = HouseholdSize,
         "Income" = IncomeFromAnySource,
         Score,
         HH_DQ_Issue,
         CountyGuessed
       )
     
     datatable_default(
       active,
       rownames = FALSE,
       options = list(dom = 'Bfrltip',
                      buttons = c('copy', 'excel', 'csvHtml5'),
                      responsive = TRUE,
                      columnDefs = list(list(
                        visible = FALSE, 
                        targets = c(14:15)
                      )), 
                      initComplete = JS(
                        "function(settings, json) {",
                        "$('th').css({'text-align': 'center'});",
                        "$('td').css({'text-align': 'center'});",
                        "}"))
     ) %>%
       DT::formatStyle(
         columns = 1, # HoH Client ID indices
         valueColumns = 15, # HH_DQ_issue indices
         color = DT::styleEqual(c(1),
                            c("white")),
         backgroundColor = DT::styleEqual(c(1),
                                      c("#7d7d8d"))
       ) %>%
       DT::formatStyle(
         columns = 4, # County
         valueColumns = 16, # CountyGuessed indices
         color = DT::styleEqual(c(1),
                            c("white")),
         backgroundColor = DT::styleEqual(c(1),
                                      c("#7d7d8d"))
       )
   })
  })
}
    
## To be copied in the UI
# mod_body_prioritization_ui("body_prioritization_1")
    
## To be copied in the server
# mod_body_prioritization_server("body_prioritization_1")
