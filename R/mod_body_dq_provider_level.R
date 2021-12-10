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
      ui_picker_project(
        choices = projects[names(projects) %in% dq_providers()],
        options = shinyWidgets::pickerOptions(
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
      # uiOutput(ns("dq_MissingLocation")), # Deprecated in Clarity
      uiOutput(ns("dq_PATHMissingContact"))
    ),
    shiny::fluidRow(uiOutput(ns("dq_Ineligible"))),
    shiny::fluidRow(uiOutput(ns("dq_OverlappingEEs"))),
    ui_solid_box(
      DT::dataTableOutput(ns("dq_Errors")),
      dq_see_guidance(),
      title = "Data Quality Errors",
      width = 12,
      solidHeader = TRUE,
      status = "danger"
    ),
    ui_solid_box(
      DT::dataTableOutput(ns("dq_Warnings")),
      dq_see_guidance(),
      title = "Data Quality Warnings",
      width = 12,
      status = "warning"
    ),
    ui_solid_box(
      id = "dq_summary",
      DT::dataTableOutput(ns("dq_summary")),
      dq_see_guidance(),
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
    guidance <- guidance()
    dq_profiles <- dq_main()
    
    eligibility_detail <- eligibility_detail()
    project <- reactive(input$project) |> shiny::debounce(1500)
    
    # TODO Should be a descriptionBox, and go in a section with others.
    output$dq_APsNoReferrals <- renderUI({
      req(project())
      AP_no_referrals <- aps_no_referrals()  |> 
        dplyr::filter(ProjectID %in% project())
      
      if (nrow(AP_no_referrals) > 0) {
        ui_solid_box(
          id = "noreferrals",
          title = "Access Point Has No Outgoing Referrals",
          status = "danger",
          width = 12,
          shiny::tagList(
            # TODO print names of APS
            shiny::tags$p(
              "The following Access Points ",
              do.call(shiny::tags$ol, purrr::map(AP_no_referrals$ProjectName, ~shiny::tags$li(.x))),
              " should be creating Referrals in HMIS so that households can be connected to housing. Please ", shiny::tags$a(href = "http://hmis.cohhio.org/index.php?pg=kb.page&id=186", target = "_blank", "click here"), "for more information.")
          )
          
        )
      } else {
        
      }
    })
    
    output$dq_HHIssues <- renderUI({
      req(project())
      HHIssues <- dq_profiles |> 
        dq_filter_between(date_range = input$date_range, project = project(), Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household"
        )) |> 
        dq_select_cols(
          `A UniqueID in the HH` = UniqueID,
          "MoveInDateAdjust",
          default = list(`Entry Date` = "EntryDate",
                         "Type",
                         "Issue")
        )
      
      if (nrow(HHIssues)) {
        ui_solid_box(
          id = "hhs",
          title = "Household Issues",
          status = "warning",
          shiny::tags$p(
            "Please correct Household Issues before moving on to make other Data Quality corrections.", dq_see_guidance()
          ),
          datatable_default(HHIssues, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    output$dq_DuplicateEEs <- renderUI({
      req(project())
      DuplicateEEs <- dq_profiles  |> 
        dq_filter_between(date_range = input$date_range, project = project(), Issue == "Duplicate Entry Exits")  |> 
        dq_select_cols(
          "Exit Date" = ExitDate
        ) 
      
      if (nrow(DuplicateEEs)) {
        ui_solid_box(
          id = "dup_ees",
          title = "Duplicate Entry Exits",
          status = "warning",
          shiny::tags$p(
            guidance$duplicate_ees
          ),
          datatable_default(DuplicateEEs, escape = FALSE)
        )
      }
      else {
        
      }
    })
    

    # Deprecated in Clarity
    # output$dq_MissingLocation <- renderUI({
    #   req(project())
    #   HHIssues <- dq_profiles |> 
    #     dq_filter_between(date_range = input$date_range, project = project(), Issue == "Missing Client Location") |> 
    #     dq_select_cols()
    #   
    #   
    #   if (nrow(HHIssues)) {
    #     ui_solid_box(
    #       id = "location",
    #       title = "Missing Client Location",
    #       status = "warning",
    #       shiny::tags$p(
    #         guidance$missing_client_loc
    #       ),
    #       datatable_default(HHIssues, escape = FALSE)
    #     )
    #   }
    #   else {
    #     
    #   }
    # })
    
    output$dq_PATHMissingContact <- renderUI({
      req(project())
      MissingPathContact <- dq_profiles  |> 
        dq_filter_between(date_range = input$date_range, project = project(), Issue == "Missing PATH Contact") |> 
        dq_select_cols()
      
      if (nrow(MissingPathContact)) {
        ui_solid_box(
          id = "location",
          title = "Missing Contact (PATH)",
          status = "warning",
          datatable_default(MissingPATHContact, escape = FALSE),
          tags$p(guidance$missing_path_contact)
        )
      }
      else {
        
      }
    })
    
    
    output$dq_Ineligible <- renderUI({
      req(project())
    Ineligible <- eligibility_detail |> 
        dq_filter_between(date_range = input$date_range, project = project()) |> 
        dplyr::mutate(
          PreviousStreetESSH = dplyr::if_else(PreviousStreetESSH == 1, "Yes", "No")
        )  |> 
        dplyr::select(
          "Unique ID" = UniqueID,
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
            guidance$check_eligibility
          ),
          datatable_default(Ineligible, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    
    
    
    output$dq_OverlappingEEs <- renderUI({
      req(project())
      OverlappingEEs <- dq_overlaps() |>
        dq_filter_between(date_range = input$date_range, project = project()) |>
        dq_select_cols(
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Move-In Date" = MoveInDateAdjust,
          "Overlaps",
          "Issue",
          "Guidance",
          default = NULL
        ) 
        
      if (nrow(OverlappingEEs)) {
        ui_solid_box(
          id = "overlappers",
          title = "Overlapping Project Stays",
          status = "warning",
          width = 12,
          datatable_default(OverlappingEEs, escape = FALSE),
          shiny::tags$p(guidance$project_stays)
        )
      } else {
        
      }
    })
    
    output$dq_Errors <- DT::renderDataTable({
      req(project())
      dq_profiles |>
        dq_filter_between(date_range = input$date_range, project = project(), 
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
        datatable_default(escape = FALSE),
    })
    
    output$dq_Warnings <- DT::renderDataTable({
      req(project())
      DQWarnings <- dq_profiles |>
        dq_filter_between(date_range = input$date_range, project = project(), 
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
      req(project())
      guidance <- dq_profiles |>
        dq_filter_between(date_range = input$date_range, project = project()) |> 
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
