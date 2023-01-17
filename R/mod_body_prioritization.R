#' body_prioritization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_prioritization_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_picker_program(
      inputId = ns("region"),
      label = "Select County/-ies",
      multiple = TRUE,
      choices = sort(Regions()$County),
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        liveSearchStyle = 'contains',
        actionsBox = TRUE
      )
    ),
    ui_row(title = "Prioritization Report", 
               DT::dataTableOutput(ns("summary")),
               footer = tags$div(class = "alert alert-warning", role = 'alert', "Warning colored rows mean the client has a Data Quality issue that may be causing incorrect information to show."))
  )
}
    
#' body_prioritization Server Functions
#'
#' @noRd 
mod_body_prioritization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
   output$header <- renderUI(server_header("Prioritization Report",
                              x = shiny::h3(paste0("Updated: ", rm_dates()$meta_HUDCSV$Export_End)),
                              shiny::p("Prioritization workgroups should use this report to prioritize clients. PSH, RRH, and TH programs are required to prioritize clients with the most severe needs and the longest homeless histories. Clients enrolled in domestic violence programs do not appear in the report and must be advocated for in prioritization meetings."),
                              shiny::p("This report is intended to identify households who may be chronically homeless. It should not serve as formal documentation of chronic homeless status. Please reference ", a("HUD Definition of Chronic Homelessness", href = "https://www.hudexchange.info/homelessness-assistance/coc-esg-virtual-binders/coc-esg-homeless-eligibility/definition-of-chronic-homelessness/"), "and", a("Recordkeeping Requirements for Chronic Status", href = "https://www.hudexchange.info/homelessness-assistance/coc-esg-virtual-binders/coc-esg-homeless-eligibility/definition-of-chronic-homelessness/recordkeeping-requirements/"), "for more information."),
                              shiny::p("Answers for Approximate Date Homelessness Started and Total number of months homeless on the streets, in ES, or Safe Haven in the past three years can be nuanced. If the answers to these questions contradict one other, if they have not been filled in correctly, or if they have not been appropriately updated, they may create data quality issues. Please check the R minor elevated Data Quality Report to ensure that answers to these questions are cohesive.")))
   
   region <- eventReactive(input$region, {input$region}) |> debounce(1500)
   pc <- prioritization_colors()
   output$summary <- DT::renderDT(server = FALSE, {
     req(region())

     prioritization() |>
       dplyr::filter(CountyServed %in% region() |
                is.na(CountyServed)) |>
       dplyr::arrange(dplyr::desc(HousingStatus)) |>
       dplyr::select(
         "HoH Unique ID" = UniqueID,
         "Project Name" = ProjectName,
         "Entry Date" = EntryDate,
         "County" = CountyServed,
         "Current Situation (Entry, Referral, Perm Housing Track)" = Situation,
         # "COVID-19: Priority for Immediate Non-congregate Housing" = C19Priority,
         "Expected Move-in" = ExpectedPHDate,
         "Veteran" = VeteranStatus,
         "Fleeing DV" = CurrentlyFleeing,
         "Transition Aged Youth" = TAY,
         "Chronic Status" = ChronicStatus,
         "Approximate Date Homelessness Started" = DateToStreetESSH,
         "Times Homeless Last 3 Years" = TimesHomelessPastThreeYears,
         "Months Homeless Last 3 Years" = MonthsHomelessPastThreeYears,
         "Eligible for PSH (Disability in Household)" = DisabilityInHH,
         "Household Size" = HouseholdSize,
         "Income" = IncomeFromAnySource,
         Score,
         HH_DQ_Issue,
         CountyGuessed,
         HousingStatus
       ) |> 
     datatable_default(
       rownames = FALSE,
       options = list(dom = 'Bfrltip',
                      buttons = c('copy', 'excel', 'csvHtml5'),
                      responsive = TRUE,
                      initComplete = DT::JS(
                        "function(settings, json) {",
                        "$('th').css({'text-align': 'center'});",
                        "$('td').css({'text-align': 'center'});",
                        "}"),
                      scrollX = TRUE),
       escape = FALSE
     ) |> 
       DT::formatStyle(
         columns = c("HoH Unique ID", "County"), 
         valueColumns = c("HH_DQ_Issue", "CountyGuessed"),
         target = "row",
         backgroundColor = DT::styleEqual(c(TRUE), c("#fff3cd"))
       ) |> 
       DT::formatStyle(
         "Current Situation (Entry, Referral, Perm Housing Track)",
         "HousingStatus",
         target = "cell",
         backgroundColor = DT::styleEqual(names(pc), pc)
       ) |> 
       datatable_options_update(options = list(columnDefs = list(list(
         visible = FALSE,
         targets = c(18:19)
       ))))
       
   })
  })
}
    
## To be copied in the UI
# mod_body_prioritization_ui("body_prioritization_1")
    
## To be copied in the server
# mod_body_prioritization_server("body_prioritization_1")
