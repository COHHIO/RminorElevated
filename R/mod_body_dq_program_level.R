#' body_dq_program_level UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_body_dq_program_level_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row(
      ui_picker_program(
        choices = dq_providers(),
        options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        liveSearchStyle = 'contains',
        actionsBox = TRUE
      ),
      multiple = TRUE,
      width = "100%",
      selected = "none"),
      ui_date_range(start = rm_dates()$hc$check_dq_back_to),
      width = 12,
      headerBorder = FALSE,
      p("If you have questions please email ", strong(a(href = "mailto:hmis@cohhio.org?subject=RminorElevated DQ Question", target = "_blank", "hmis@cohhio.org")), " and please include the selected Program name.")
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
      bs4Dash::accordion(
        id = "dq_frequency_info",
        bs4Dash::accordionItem(title = "Frequency Explanation (# / Total Clients)",
        tags$p("Frequency is calculate as the # of times this error occurs / the # of clients (not the number of clients with the error / the # of clients). The bargraph indicates how frequently this error occurs for th(is/ese) programs(s) compared to the average for the rest of the CoC. ",tags$span("Green", style = "color:rgb(40, 167, 69)") , " indicates that the error occurs less frequently than the CoC average, ",tags$span("Red", style = "color:rgb(220, 53, 69)"), " indicates the error occurs more frequently than the CoC average. The larger the bar, the greater the distance from the average, IE a large red bar indicates this error occurs much more frequently for these programs than the CoC average."))
      ),
      DT::dataTableOutput(ns("dq_summary")),
      title = "Data Quality Guidance",
      status = "info"
    )
  )
}

#' body_dq_program_level Server Functions
#'
#' @noRd 
mod_body_dq_program_level_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI({
      server_header(title = "Data Quality", date_range = input$date_range)
    })
    guidance <- guidance()
    
    
    eligibility_detail <- dq_eligibility_detail()
    server_debounce(input$program, input$date_range)
    
    dq_p <- dq_main()

    dq_main_time <- eventReactive(input$date_range, {
      req(input$date_range)
      dq_p |> 
        dq_filter_between(date_range = input$date_range)
        
    }) |> 
      debounce(1500)
    dq_main_time_proj <- eventReactive(c(input$date_range, input$program), {
      req(input$program, input$date_range, dq_main_time())
      dq_main_time() |> 
        dq_filter_between(program = input$program)
    }) |> 
      debounce(1500)
    co_clients <- co_clients_served()
    clients <- eventReactive(input$date_range, {
      req(input$date_range)
      co_clients |> 
        dq_filter_between(date_range = input$date_range)
      
    }) |> 
      debounce(1500)
    
    # TODO Should be a descriptionBox, and go in a section with others.
    output$dq_APsNoReferrals <- renderUI({
      req(input$program, program())
      AP_no_referrals <- dq_aps_no_referrals()  |> 
        dplyr::filter(ProjectID %in% program() & 
                        (Sys.Date() < OperatingEndDate | is.na(OperatingEndDate)))
      
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
      req(dq_main_time_proj())
      HHIssues <- dq_main_time_proj() |> 
        dq_filter_between(Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household",
          "Client remains active after Head of Household's exit or deletion"
        )) |> 
        dq_select_cols(
          `A UniqueID in the HH` = UniqueID,
          `Move-in Date` = "MoveInDateAdjust",
          !!purrr::when(length(program()) > 1, . ~ rlang::expr({ProgramName = "ProjectName"}), ~ NULL),
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
      req(dq_main_time_proj())
      DuplicateEEs <- dq_main_time_proj()  |> 
        dq_filter_between(Issue == "Duplicate Entry Exits")  |> 
        dq_select_cols(
          !!purrr::when(length(program()) > 1, . ~ rlang::expr({ProgramName = "ProjectName"}), ~ NULL),
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate
        ) 
      
      if (nrow(DuplicateEEs)) {
        ui_solid_box(
          id = "dup_ees",
          title = "Duplicate Entry Exits",
          status = "warning",
          shiny::HTML(
            guidance$duplicate_ees
          ),
          datatable_default(DuplicateEEs, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    
    output$dq_PATHMissingContact <- renderUI({
      req(dq_main_time_proj())
      MissingPathContact <- dq_main_time_proj()  |> 
        dq_filter_between(Issue == "Missing PATH Contact", date_range = date_range()) |> 
        dq_select_cols(!!purrr::when(length(program()) > 1, . ~ rlang::expr({ProgramName = "ProjectName"}), ~ NULL))
      
      if (nrow(MissingPathContact)) {
        ui_solid_box(
          id = "location",
          title = "Missing Contact (PATH)",
          status = "warning",
          datatable_default(MissingPathContact, escape = FALSE),
          HTML(guidance$missing_path_contact)
        )
      }
      else {
        
      }
    })
    
    
    output$dq_Ineligible <- renderUI({
      req(input$program, input$date_range, program(), date_range())
    Ineligible <- eligibility_detail |> 
        dq_filter_between(ProjectID %in% program(), date_range = date_range()) |> 
        dplyr::mutate(
          PreviousStreetESSH = dplyr::if_else(PreviousStreetESSH == 1, "Yes", "No")
        )  |> 
        dplyr::select(
          "Unique ID" = UniqueID,
          "Entry Date" = EntryDate,
          !!purrr::when(length(program()) > 1, . ~ rlang::expr({ProgramName = "ProjectName"}), ~ NULL),
          "Prior Living Situation" = LivingSituation,
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
          shiny::HTML(
            guidance$check_eligibility
          ),
          datatable_default(Ineligible, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    
    
    
    output$dq_OverlappingEEs <- renderUI({
      req(input$program, input$date_range, program(), date_range())
      OverlappingEEs <- dq_overlaps() |>
        dq_filter_between(ProjectID %in% program(), date_range = date_range()) |>
        dq_select_cols(
          "UniqueID",
          "Project Name" = ProjectName,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Move-In Date" = MoveInDateAdjust,
          # "Overlaps",
          "Issue",
          "Guidance",
          default = NULL
        )

      if (nrow(OverlappingEEs)) {
        ui_solid_box(
          id = "overlappers",
          title = "Overlapping Program Stays",
          status = "warning",
          width = 12,
          datatable_default(OverlappingEEs, escape = FALSE),
          shiny::HTML(guidance$project_stays)
        )
      } else {

      }
    })
    
    output$dq_Errors <- DT::renderDT(server = FALSE, {
      req(dq_main_time_proj())
      
      dq_main_time_proj() |>
        dq_filter_between(
          !Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household",
            "Overlapping Program Stays",
            "Duplicate Entry Exits",
            "Access Point with Entry Exits",
            "Client remains active after Head of Household's exit or deletion"
          )  &
            Type == "Error"
        ) |>
        dq_select_cols(!!purrr::when(length(program()) > 1, . ~ rlang::expr({ProgramName = "ProjectName"}), ~ NULL)) |> 
        datatable_default(escape = FALSE)
    })
    
    output$dq_Warnings <- DT::renderDT(server = FALSE, {
      req(dq_main_time_proj())
      DQWarnings <- dq_main_time_proj() |>
        dq_filter_between(
          !Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household",
            "Overlapping Program Stays",
            "Duplicate Entry Exits",
            "Check Eligibility",
            "Client remains active after Head of Household's exit or deletion"
          ) &
            Type == "Warning"
        ) |>
        dq_select_cols(!!purrr::when(length(program()) > 1, . ~ rlang::expr({ProgramName = "ProjectName"}), ~ NULL)) |> 
        datatable_default(escape = FALSE)

    })
    
    issues_by_program <- reactive({
      req(dq_main_time(), clients())
      dq_performance(dq_performance(dq_main_time(), groups = c("Issue", "ProjectID")), dq_performance(clients()), join = T) |> 
        dplyr::group_by(Issue) |> 
        dplyr::mutate(rank = round(rank, 5),
                      p = round(p, 5)) |> 
        dplyr::distinct(Issue, ProjectID, rank, p)
    })
      
    output$dq_summary <- DT::renderDT(server = FALSE, {
      req(input$program, issues_by_program(), dq_main_time_proj())
      
      out <- dq_main_time_proj() |>
        dplyr::group_by(Type, Issue, Guidance, ProjectName, ProjectID) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |> 
        dplyr::mutate(Type = factor(Type, levels = c("High Priority",
                                              "Error",
                                              "Warning"))) |>
        dplyr::arrange(Type) |>
        dplyr::left_join(issues_by_program(), by = c("ProjectID", "Issue")) |> 
        dplyr::mutate(p = round(p, 3),
                      ProjectID = NULL) |> 
        dplyr::rename(`# of Issues` = n,
                      Frequency = p) |> 
        unique()
        
        
        datatable_default(out, escape = FALSE) |> 
          DT::formatStyle(
            columns = "Frequency",
            valueColumns = "rank",
            background = styleDivergentBar(c(-.5,.5), color_pos = "#28a745", color_neg = "#dc3545")
          ) |> 
          datatable_options_update(options = list(columnDefs = list(list(
            visible = FALSE,
            targets = length(out)- 1
          ))))
    })
    
  })
}
    
## To be copied in the UI
# mod_body_dq_program_level_ui("body_dq_program_level_1")
    
## To be copied in the server
# mod_body_dq_program_level_server("body_dq_program_level_1")
