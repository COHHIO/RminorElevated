#' body_dq_provider_level UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_dq_provider_level_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row_box(
      ui_picker_project(options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        liveSearchStyle = 'contains',
        actionsBox = TRUE
      ),
      multiple = TRUE,
      width = "100%",
      selected = "none"),
      ui_date_range(start = lubridate::ymd("2019-01-01")),
      width = 12,
      headerBorder = FALSE
    ),
    shiny::fluidRow(uiOutput(ns("dq_APsNoReferrals"))),
    shiny::fluidRow(
      uiOutput(ns("dq_HHIssues")),
      uiOutput(ns("dq_DuplicateEEs")),
      # DQIncorrectEEType SP only
      uiOutput(ns("dq_MissingLocation")),
      uiOutput(ns("dq_PATHMissingContact"))
    ),
    shiny::fluidRow(uiOutput(ns("dq_Ineligible"))),
    shiny::fluidRow(uiOutput(ns("dq_OverlappingEEs"))),
    ui_solid_box(
      DT::dataTableOutput(ns("dq_Errors")),
      title = "Data Quality Errors",
      width = 12,
      solidHeader = TRUE,
      status = "danger"
    ),
    ui_solid_box(
      DT::dataTableOutput(ns("dq_Warnings")),
      title = "Data Quality Warnings",
      width = 12,
      status = "warning"
    ),
    ui_solid_box(
      id = "dq_summary",
      DT::dataTableOutput(ns("dq_summary")),
      title = "Data Quality Guidance",
      status = "info"
    )
  )
}
    
#' body_dq_provider_level Server Functions
#'
#' @noRd 
mod_body_dq_provider_level_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI({
      server_header(title = "Data Quality", date_range = input$date_range)
    })
    
    dq_profiles <- dq_main()
    
    eligibility_detail <- eligibility_detail()
    
    
    
    # TODO Should be a descriptionBox, and go in a section with others.
    output$dq_APsNoReferrals <- renderUI({
      req(input$project)
      AP_not_doing_referrals <- aps_no_referrals()  |> 
        dplyr::filter(ReferringProjectID %in% input$project)
      
      if (nrow(AP_not_doing_referrals) > 0) {
        ui_row_box(
          id = "noreferrals",
          title = "Access Point Has No Outgoing Referrals",
          status = "danger",
          width = 12,
          shiny::tags$p(
            "Access Points should be creating Referrals in HMIS so that households can be connected to housing. Please 
<a href=\"http://hmis.cohhio.org/index.php?pg=kb.page&id=186\">click here</a> for more information."
          )
        )
      } else {
        
      }
    })
    
    output$dq_DuplicateEEs <- renderUI({
      req(input$project)
      DuplicateEEs <- dq_profiles  |> 
        dq_filter_between(date_range = input$date_range, project = input$project, Issue == "Duplicate Entry Exits")  |> 
        dq_select_cols(
          "Exit Date" = ExitDate
        ) 
      
      if (nrow(DuplicateEEs)) {
        ui_solid_box(
          id = "dup_ees",
          title = "Duplicate Entry Exits",
          status = "warning",
          shiny::tags$p(
            "Please correct this issue before moving on to your other errors.<br> Duplicate Entry Exits are created when the user clicks \"Add Entry Exit\" instead of clicking the Entry pencil to get back into an assessment. These must be deleted for each member of the household. Please take care to not delete Entry Exits with valid Interims attached."
          ),
          datatable_default(DuplicateEEs, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    
    
    
    output$dq_HHIssues <- renderUI({
      req(input$project)
      HHIssues <- dq_profiles |> 
        dq_filter_between(date_range = input$date_range, project = input$project, Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household"
        )) |> 
        dq_select_cols(
          `A UniqueID in the HH` = UniqueID,
          "MoveInDateAdjust"
        )
      
      if (nrow(HHIssues)) {
        ui_solid_box(
          id = "hhs",
          title = "Household Issues",
          status = "warning",
          shiny::tags$p(
            "Please correct your Household Issues before moving on to make other Data Quality corrections."
          ),
          datatable_default(HHIssues, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    output$dq_MissingLocation <- renderTable({
      req(input$project)
      HHIssues <- dq_profiles |> 
        dq_filter_between(date_range = input$date_range, project = input$project, Issue == "Missing Client Location") |> 
        dq_select_cols()
      
      
      if (nrow(HHIssues)) {
        ui_solid_box(
          id = "location",
          title = "Missing Client Location",
          status = "warning",
          shiny::tags$p(
            "Households with a missing Client Location (the data element just after the Relationship to Head of Household) will be completely excluded from ALL HUD reporting."
          ),
          datatable_default(HHIssues, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    output$dq_PATHMissingContact <- renderUI({
      req(input$project)
      MissingPathContact <- dq_profiles  |> 
        dq_filter_between(date_range = input$date_range, project = input$project, Issue == "Missing PATH Contact") |> 
        dq_select_cols()
      
      if (nrow(MissingPathContact)) {
        ui_solid_box(
          id = "location",
          title = "Missing Contact (PATH)",
          status = "warning",
          datatable_default(MissingPATHContact, escape = FALSE),
          tagList(MissingPathContact |> 
            dplyr::select(Guidance) |> 
            unique() |> 
              shiny::tags$p())
        )
      }
      else {
        
      }
    })
    
    
    output$dq_Ineligible <- renderUI({
      req(input$project)
    Ineligible <- eligibility_detail |> 
        dq_filter_between(date_range = input$date_range, project = input$project) |> 
        dplyr::mutate(
          PreviousStreetESSH = dplyr::if_else(PreviousStreetESSH == 1, "Yes", "No")
        )  |> 
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Residence Prior" = ResidencePrior,
          "Length of Stay" = LengthOfStay,
          "Literally Homeless Prior" = PreviousStreetESSH
        )
      
      if (nrow(Ineligible)) {
        ui_solid_box(
          id = "eligibility",
          title = "Check Eligibility",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          shiny::tags$p(
            "Your Residence Prior data suggests that this project is either serving ineligible households, the household was entered into the wrong project, or the Residence Prior data at Entry is incorrect. Please check the terms of your grant or speak with the CoC team at COHHIO if you are unsure of eligibility criteria for your project type."
          ),
          datatable_default(Ineligible, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    
    
    
    output$dq_OverlappingEEs <- renderUI({
      req(input$project)
      OverlappingEEs <- dq_overlaps() |>
        dq_filter_between(date_range = input$date_range, project = input$project, Issue == "Overlapping Project Stays") |>
        dq_select_cols(
          "Exit Date" = ExitDate,
          "Move In Date" = MoveInDateAdjust,
          "Overlaps With This Provider's Stay" = PreviousProject
        ) 
        
      if (nrow(OverlappingEEs)) {
        ui_solid_box(
          id = "overlappers",
          title = "Overlapping Entry Exits",
          status = "warning",
          width = 12,
          shiny::tags$p(
            "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor can they have a Move-In Date into a PSH or RRH project while they are still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's or any two PSH's simultaneously, housed or not.<br> Please look the client(s) up in HMIS and determine which project stay's Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the \"Previous Provider's\" mistake, but if you are seeing clients here, it means your project stay was entered last. <br> If the overlap is not your project's mistake, please work with the project that has the incorrect Entry/Move-In/or Exit Date to get this corrected  or send an email to hmis@cohhio.org if you cannot get it resolved. These clients will NOT show on their Data Quality app. <br> If YOUR dates are definitely correct, it is fine to continue with other data corrections as needed."),
          datatable_default(OverlappingEEs, escape = FALSE)
        )
      } else {
        
      }
    })
    
    output$dq_Errors <- DT::renderDataTable({
      req(input$project)
      dq_profiles |>
        dq_filter_between(date_range = input$date_range, project = input$project, 
          !Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household",
            "Overlapping Project Stays",
            "Duplicate Entry Exits",
            "Access Point with Entry Exits"
          )  &
            Type == "Error"
        ) |>
        dq_select_cols() |> 
        datatable_default(escape = FALSE)
    })
    
    output$dq_Warnings <- DT::renderDataTable({
      req(input$project)
      DQWarnings <- dq_profiles |>
        dq_filter_between(date_range = input$date_range, project = input$project, 
          !Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household",
            "Overlapping Project Stays",
            "Duplicate Entry Exits",
            "Check Eligibility"
          ) &
            Type == "Warning"
        ) |>
        dq_select_cols() |> 
        datatable_default(escape = FALSE)

    })
    
    output$dq_summary <- DT::renderDataTable({
      req(input$project)
      guidance <- dq_profiles |>
        dq_filter_between(date_range = input$date_range, project = input$project) |> 
        dplyr::group_by(Type, Issue, Guidance) |>
        dplyr::ungroup() |>
        dplyr::select(Type, Issue, Guidance) |>
        dplyr::mutate(Type = factor(Type, levels = c("High Priority",
                                              "Error",
                                              "Warning"))) |>
        dplyr::arrange(Type) |>
        unique() |> 
        datatable_default(escape = FALSE)
    })
    
  })
}
    
## To be copied in the UI
# mod_body_dq_provider_level_ui("body_dq_provider_level_1")
    
## To be copied in the server
# mod_body_dq_provider_level_server("body_dq_provider_level_1")
