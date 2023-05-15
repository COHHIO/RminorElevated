#' body_vet_active_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_body_vet_active_list_ui <- function(id) {
  ns <- NS(id)
  val <- veteran_active_list()
  # val2 <- vets_housed()
  chronic_status <- as.character(sort(unique(val$ChronicStatus)))
  tagList(
    ui_header_row(),
    ui_row(
      title = "Legend",
      fluidRow(
        bs4Dash::bs4Card(
          id = "va-legend",
          collapsible = FALSE,
          title = "Enrollments",
          ui_list(
            l_style = "list-style-type:none;margin-left: 0px;
    padding-left: 0px;",
            text = c(
              "Housing Program",
              "Literally Homeless Program",
              "Other Program"
            ),
            style = glue::glue_data("background-color: {color};", .x = list(
              color = c("lavenderblush", "lightgoldenrodyellow", "paleturquoise")
            ))
          ),
          width = 4
        ),
        bs4Dash::bs4Card(
          collapsible = FALSE,
          title = "Eligibility",
          ui_list(
            l_style = "list-style-type:none;margin-left: 0px;
    padding-left: 0px;",
            text = c(
              " = Veteran eligible for all VA",
              " = Veteran not eligible for VA",
              " = Veteran eligible for SSVF/GPD only",
              " = VA eligibility unknown"
            ),
            icon = icons$vet_active
          ),
          width = 4
        ),
        bs4Dash::bs4Card(
          collapsible = FALSE,
          title = "Housing Track & Notes",
          ui_list(
            l_style = "list-style-type:none;margin-left: 0px;
    padding-left: 0px;",
            text = paste0(
              "Expected PH Date ",
              c("on or after today", "in the past", "not recorded")
            ),
            style = glue::glue_data(list(
              color = c("seagreen", "tomato", "inherit")
            ), "color: {color};")
          ),
          width = 4
        ) 
      )
    ),
    ui_row(
      fluidRow(
        column(
          3,
          ui_picker_program(
            label = "Select County(ies)",
            inputId = ns("county"),
            choices = counties,
            add_options = list(actionsBox = TRUE)
          )
        ),
        column(
          3,
          ui_picker_program(
            label = "Select List Status",
            inputId = ns("vet_status"),
            choices = val$ListStatus |>
              unique() |>
              sort(),
            selected = c("Active - ES/TH",
                         "Active - Unsheltered",
                         "No Status Set"),
            add_options = list(actionsBox = TRUE)
          )
        ),
        column(
          3,
          ui_picker_program(
            label = "Select Chronic Status",
            inputId = ns("chronic_status"),
            choices = chronic_status,
            selected = chronic_status,
            add_options = list(actionsBox = TRUE)
          )
        ),
        column(3, downloadButton(ns("download"), "Download Active List"))
      )
    ),
    ui_solid_box(title = "Veteran Active List", status = "info",
                 DT::dataTableOutput(ns("detail"))
                 )
    # ui_solid_box(title = "Veterans Housed Last 90 Days", status = "info",
    #              DT::dataTableOutput(ns("housed")))
  )
}


make_factor_regex <- function(x, levels = c("fa-check", "fa-exclamation-triangle", "fa-question-circle", "fa-times"), ordered = is.ordered(x)) {
  factor(x, 
         levels = do.call(c, purrr::map(levels, ~unique(stringr::str_subset(x, .x)))),
         ordered = ordered)
  
}
#' body_vet_active_list Server Functions
#'
#' @noRd
mod_body_vet_active_list_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    server_debounce(input$county, input$vet_status, input$chronic_status)
    
    output$header <- shiny::renderUI({
      server_header(
        "Veteran Active List",
        paste(
          "Homeless Veterans as of",
          rm_dates()$meta_HUDCSV$Export_End
        )
      )
    })
    
    val <- veteran_active_list()
    # val2 <- vets_housed()

    output$detail <- DT::renderDT(server = FALSE, {
      req(input$county, input$vet_status, input$chronic_status, county(), vet_status(), chronic_status())
      
      
      
      vet_active_list <- val |>
        dplyr::filter(
          County %in% county() &
          ListStatus %in% vet_status() &
          ChronicStatus %in% chronic_status()
        ) |>
        dplyr::arrange(PersonalID) |>
        dplyr::mutate(
          HousingPlan = dplyr::case_when(
            ExpectedPHDate < lubridate::today() ~ paste0("<span style='color:tomato;'>", HousingPlan, "</span>"),
            ExpectedPHDate >= lubridate::today() ~ paste0(
              "<span style='color:seagreen;'>",
              HousingPlan,
              "</span>"
            ),
            TRUE ~ HousingPlan
          ),
          VAEligibilityIcon = paste(
            dplyr::case_when(
              stringr::str_detect(VAEligible, stringr::regex(ignore_case = TRUE, "all VA homeless services")) ~ as.character(icons$vet_active$pass),
              stringr::str_detect(VAEligible, stringr::regex(ignore_case = TRUE, "VA services")) ~ as.character(icons$vet_active$fail),
              stringr::str_detect(VAEligible, stringr::regex(ignore_case = TRUE, "SSVF/GPD only")) ~ as.character(icons$vet_active$alert),
              stringr::str_detect(VAEligible, stringr::regex("Unknown", ignore_case = TRUE)) |
                is.na(VAEligible) ~ as.character(icons$vet_active$unknown)
            ),
            dplyr::case_when(
              !is.na(SSVFIneligible) ~ paste("<br><br>", SSVFIneligible),
              TRUE ~ ""
            )
          )
        ) |>
        dplyr::select(
          "SSVF Responsible Provider" = SSVFServiceArea,
          "Unique ID" = UniqueID,
          "Active Date" =  ActiveDateDisplay,
          "Enrollments" = Enrollments,
          "Eligibility" = VAEligibilityIcon,
          "Most Recent Offer" = MostRecentOffer,
          "List Status" = ListStatus,
          "Chronic Status" = ChronicStatus,
          "Housing Track & Notes" = HousingPlan
        ) |> 
        datatable_default(escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))
    })
    
    # output$housed <- DT::renderDT(server = FALSE,
    #   val2 |> datatable_default(escape = FALSE,
    #                             options = list(
    #                               initComplete = DT::JS(
    #                                 "function(settings, json) {",
    #                                 "$('th').css({'text-align': 'center'});",
    #                                 "$('td').css({'text-align': 'center'});",
    #                                 "}"
    #                               ))
    #                             ))
    
    output$download <- shiny::downloadHandler(
      filename = "veteran_active_list.csv",
      content = function(file) {
        write.csv(
          val |>
            dplyr::filter(County %in% input$county |
                            is.na(County)) |>
            dplyr::mutate(DisablingCondition = HMIS::hud_translations$`1.8 No_Yes_Reasons for Missing Data`(DisablingCondition)) |>
            clarity.looker::make_linked_df(UniqueID, unlink = TRUE) |> 
            dplyr::select(
              SSVFServiceArea,
              County,
              PersonalID,
              UniqueID,
              DateVeteranIdentified,
              EntryDate,
              ListStatus,
              ChronicStatus,
              VAEligible,
              SSVFIneligible,
              DisablingCondition,
              # VAMCStation,
              PHTrack,
              ExpectedPHDate,
              # Destination,
              # OtherDestination,
              # ClientLocation,
              AgeAtEntry,
              ProjectName_LH,
              TimeInProject_LH,
              ProjectName_O,
              TimeInProject_O,
              ProjectName_PH,
              TimeInProject_PH,
              # VeteranStatus,
              # HouseholdSize,
              Notes,
              ActiveDate,
              # DaysActive,
              # Eligibility,
              MostRecentOffer,
              HousingPlan
            ) |> unique(),
          file = file
        )
      }
    )
  })
}

## To be copied in the UI
# mod_body_vet_active_list_ui("body_vet_active_list_1")

## To be copied in the server
# mod_body_vet_active_list_server("body_vet_active_list_1")
