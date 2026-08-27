#' body_client_counts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_client_counts_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    ui_header_row(),
    ui_row(
      tags$p(tags$em("Note:"), " if a program is absent from this list, it did not serve any clients since ", get_app_data("rm_dates")$calc$data_goes_back_to),
      ui_picker_program(),
      ui_date_range(start = Sys.Date() - lubridate::days(90)),
      headerBorder = FALSE
    ),
    ui_row(
      title = "Summary",
      mod_dt_download_ui(ns("dl_counts")),
      DT::dataTableOutput(ns("summary")),
    ),
    ui_row(
      title = "Client Details",
      mod_dt_download_ui(ns("dl_details")),
      DT::dataTableOutput(ns("dt_output")),
      width = 12
    )
  )
}

#' body_client_counts Server Functions
#'
#' @noRd 
mod_body_client_counts_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$header <- shiny::renderUI({
      server_header(
        title = "Client Counts Report",
        date_range = input$date_range
      )
    })
    
    details_filtered <- reactive({
      get_app_data("validation")  |> 
        HMIS::served_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectID %in% input$program) |>
        dplyr::mutate(
          RelationshipToHoH = dplyr::case_when(
            RelationshipToHoH == 1 ~ "Head of Household",
            RelationshipToHoH == 2 ~ "Child",
            RelationshipToHoH == 3 ~ "Spouse or Partner",
            RelationshipToHoH == 4 ~ "Other relative",
            RelationshipToHoH == 5 ~ "Unrelated household member",
            RelationshipToHoH == 99 ~ "Data not collected (please correct)"
          ),
          Status = dplyr::case_when(
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ paste0("Currently Awaiting Housing (", 
                                       lubridate::today() - EntryDate,
                                       " days)"),
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ paste0("Currently Moved In (",
                                       lubridate::today() - MoveInDateAdjust,
                                       " days)"),
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited No Move-In",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited with Move-In",
            !ProjectType %in% c(3, 13) &
              is.na(ExitDate) ~ paste0("Currently in program (",
                                       lubridate::today() - EntryDate, 
                                       " days)"),
            !ProjectType %in% c(3, 13) &
              !is.na(ExitDate) ~ "Exited program",
          ),
          sort = lubridate::today() - EntryDate
        ) |>
        dplyr::arrange(dplyr::desc(sort), HouseholdID) |>
        dplyr::select(
          "Project Name" = ProjectName,
          "County" = CountyServed,
          "Unique ID" = UniqueID,
          "Relationship to Head of Household" = RelationshipToHoH,
          "Entry Date" = EntryDate,
          "Move In Date (RRH/PSH Only)" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          Status
        )
    })

    output$dt_output <- DT::renderDT(server = TRUE, {
      details_filtered() |>
        datatable_default(escape = FALSE, export_buttons = FALSE)
    })
    
    counts_filtered <- reactive({
      hhs <- get_app_data("validation") |> 
        HMIS::served_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectID %in% input$program) |>
        dplyr::select(HouseholdID,
                      ProjectType,
                      EntryDate,
                      MoveInDateAdjust,
                      ExitDate) |>
        unique() |>
        dplyr::mutate(
          # Entered = if_else(between(EntryDate, input$date_range[1], input$date_range[2]),
          #                   "Entered in date range", "Entered outside date range"),
          # Leaver = if_else(!is.na(ExitDate), "Leaver", "Stayer"),
          Status = dplyr::case_when(
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Awaiting Housing",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Moved In",
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited No Move-In",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited with Move-In",
            !ProjectType %in% c(3, 13) &
              is.na(ExitDate) ~ "Currently in program",
            !ProjectType %in% c(3, 13) &
              !is.na(ExitDate) ~ "Exited program",
          )
        ) |>
        dplyr::group_by(Status) |>
        dplyr::summarise(Households = dplyr::n())
      
      clients <- get_app_data("validation")  |> 
        HMIS::served_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectID %in% input$program) |>
        dplyr::select(UniqueID,
                      ProjectType,
                      EntryDate,
                      MoveInDateAdjust,
                      ExitDate) |>
        unique() |>
        dplyr::mutate(
          Status = dplyr::case_when(
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Awaiting Housing",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Moved In",
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited No Move-In",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited with Move-In",
            !ProjectType %in% c(3, 13) &
              is.na(ExitDate) ~ "Currently in program",
            !ProjectType %in% c(3, 13) &
              !is.na(ExitDate) ~ "Exited program",
          )
        ) |>
        dplyr::group_by(Status) |>
        dplyr::summarise(Clients = dplyr::n())
      
      dplyr::full_join(clients, hhs, by = "Status")
    })

    output$summary <- DT::renderDT(server = TRUE, {
      counts_filtered() |>
        datatable_default(export_buttons = FALSE)
    })

    mod_dt_download_server("dl_details", data = details_filtered, filename_prefix = "client_details")
    mod_dt_download_server("dl_counts",  data = counts_filtered,  filename_prefix = "client_counts_summary")

  })
}

## To be copied in the UI
# mod_body_client_counts_ui("body_client_counts_1")

## To be copied in the server
# mod_body_client_counts_server("body_client_counts_1")