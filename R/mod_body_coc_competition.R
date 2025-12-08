#' body_coc_competition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_coc_competition_ui <- function(id){
  ns <- NS(id)
  pe_sum_val <- pe_summary_validation()
  tagList(
    ui_header_row(),
    ui_picker_program(
          inputId = ns("pe_provider"),
          label = "Select your CoC-funded Provider",
          choices = sort(pe_sum_val$AltProjectName) |> unique(),
          selected = NULL
        ),
    ui_row(
      title = "Score Summary",
      DT::dataTableOutput(ns("pe_ProjectSummary")),
      status = "info",
      solidHeader = TRUE
      ),
    ui_row(
      title = "Client Detail"
    ),
    ui_row(
      title = "Exits to Permanent Housing",
            DT::dataTableOutput(ns("pe_ExitsToPH"))
    ),
    ui_row(
      title = "Returns to Homelessness",
      DT::dataTableOutput(ns("pe_ReturnToHomelessness"))
    ),
    ui_row(
      title = "Benefits & Health Insurance at Exit",
            DT::dataTableOutput(ns("pe_BenefitsAtExit"))
    ),
    ui_row(
      title = "Living Situation at Entry",
            DT::dataTableOutput(ns("pe_LivingSituationAtEntry"))
    ),
    ui_row(
      title = "No Income at Entry",
            DT::dataTableOutput(ns("pe_NoIncomeAtEntry"))
    ),
    ui_row(
      title = "Increased Income",
      DT::dataTableOutput(ns("pe_IncreaseIncome"))
    ),
    ui_row(
      title = "Increased Earned Income",
      DT::dataTableOutput(ns("pe_IncreaseEarnedIncome"))
    ),
    ui_row(
      title = "Median Homeless History Index",
            DT::dataTableOutput(ns("pe_MedianHHI"))
    ),
    ui_row(
      title = "VISPDAT/HARP Score Completion",
            DT::dataTableOutput(ns("pe_ScoredAtPHEntry"))
    )
  )
}
    
#' body_coc_competition Server Functions
#'
#' @noRd 
mod_body_coc_competition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI(server_header("2025 CoC Competition Renewal Project Evaluation",
                                            x = shiny::h3(paste0("Reporting Period: 1/1/24 - 12/31/24")),
                                            shiny::p("For more information visit the", a("Ohio BoSCoC CoC Program website", href = "https://cohhio.org/boscoc/coc-program/"))))
    
    pe_summary <- pe_summary_final_scoring() |> 
      dplyr::mutate(dplyr::across(tidyselect::ends_with("Math"),
                                  function(x) gsub("/", "÷", x))) |> 
      dplyr::mutate(IncreasedEarnedIncomePoints = ifelse(
        IncreasedEarnedIncomeMath == "All points granted because 0 adults moved into the project's housing",
        8,
        IncreasedEarnedIncomePoints
      ))
    pe_summary_final_filter <- eventReactive(input$pe_provider, {
        pe_summary |>
          dplyr::filter(AltProjectName %in% input$pe_provider)
    })
    

    output$pe_ProjectSummary <-
      DT::renderDataTable({
        ptc <- pe_summary_final_filter() |>
          dplyr::pull(ProjectType)

        estimated_score <- pe_summary_final_filter() |>
          dplyr::select(
            "Project Name" = AltProjectName,
            "Exits to Permanent Housing" = ExitsToPHPoints,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitPoints,
            "Returns to Homelessness" = ReturnToHomelessnessPoints,
            "Living Situation at Entry" = LHResPriorPoints,
            "No Income at Entry" = NoIncomeAtEntryPoints,
            "Increased Income" = IncreasedIncomePoints,
            "Increased Earned Income" = IncreasedEarnedIncomePoints,
            "Median Homeless History Index" = MedianHHIPoints,
            "VISPDAT/HARP Completion at Entry" = ScoredAtEntryPoints,
            "Data Quality" = DQPoints,
            "Total Points" = TotalScore

          ) |>
          tidyr::pivot_longer(cols = -c("Project Name"),
                       names_to = "Measure",
                       values_to = "Estimated Score") 
        
        dq <- pe_summary_final_filter() |>
          dplyr::select(
            "Project Name" = AltProjectName,
            "Exits to Permanent Housing" = ExitsToPHDQ,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitDQ,
            "Returns to Homelessness" = ReturnToHomelessnessDQ,
            "Living Situation at Entry" = LHResPriorDQ,
            "No Income at Entry" = NoIncomeAtEntryDQ,
            "Increased Income" = IncreasedIncomeDQ,
            "Increased Earned Income" = IncreasedEarnedIncomeDQ,
            "Median Homeless History Index" = MedianHHIDQ,
            "VISPDAT/HARP Completion at Entry" = ScoredAtEntryDQ
          ) |>
          tidyr::pivot_longer(cols = -c("Project Name"),
                       names_to = "Measure",
                       values_to = "DQflag")
        
        possible_score <- pe_summary_final_filter() |>
          dplyr::select(
            "Project Name" = AltProjectName,
            "Exits to Permanent Housing" = ExitsToPHPossible,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitPossible,
            "Returns to Homelessness" = ReturnToHomelessnessPossible,
            "Living Situation at Entry" = LHResPriorPossible,
            "No Income at Entry" = NoIncomeAtEntryPossible,
            "Increased Income" = IncreasedIncomePossible,
            "Increased Earned Income" = IncreasedEarnedIncomePossible,
            "Median Homeless History Index" = MedianHHIPossible,
            "VISPDAT/HARP Completion at Entry" = ScoredAtEntryPossible,
            "Data Quality" = DQPossible
          ) |>
          tidyr::pivot_longer(cols = -c("Project Name"),
                       names_to = "Measure",
                       values_to = "Possible Score")
        
        calculation <- pe_summary_final_filter() |>
          dplyr::select(
            "Project Name" = AltProjectName,
            "Exits to Permanent Housing" = ExitsToPHMath,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitMath,
            "Returns to Homelessness" = ReturnToHomelessnessMath,
            "Living Situation at Entry" = LHResPriorMath,
            "No Income at Entry" = NoIncomeAtEntryMath,
            "Increased Income" = IncreasedIncomeMath,
            "Increased Earned Income" = IncreasedEarnedIncomeMath,
            "Median Homeless History Index" = MedianHHIMath,
            "VISPDAT/HARP Completion at Entry" = ScoredAtEntryMath,
            "Data Quality" = DQMath,
          ) |>
          tidyr::pivot_longer(cols = -c("Project Name"),
                       names_to = "Measure",
                       values_to = "Calculation")
        
        estimated_score_dq <- estimated_score |> dplyr::left_join(dq, by = c("Measure","Project Name")) |>
          dplyr::ungroup() |>
          dplyr::left_join(possible_score, by = c("Measure","Project Name")) |>
          dplyr::left_join(calculation, by = c("Measure","Project Name")) |>
          dplyr::mutate(
            DQ = dplyr::case_when(
              DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
              DQflag == 0 ~ "Data Quality passes",
              DQflag == 2 ~ "", # "Documents not yet received",
              DQflag == 3 ~ "", # "Docs received, not yet scored",
              DQflag == 4 ~ "", # "CoC Error",
              DQflag == 5 ~ "" # "Docs received past the due date"
            )
          ) |> 
          dplyr::filter(!is.na(`Estimated Score`))
        
        

        datatable_default(
          estimated_score_dq |> 
            dplyr::select(1, Calculation, 2, "Estimated Score", "Possible Score" = 5, "Data Quality" = DQ),
          options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                         list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                         75, 100, 1000), lengthChange = TRUE, pageLength = 10)
        )
      })
    
    # Exits to Permanent Housing
    # Measure 1
    pe_exits <- pe_exits_to_ph() |>
      dplyr::mutate(MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No"),
                    Destination = living_situation(Destination))
    pe_exits_to_ph_filter <- eventReactive(input$pe_provider, {
      pe_exits |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_ExitsToPH <- DT::renderDataTable({
      pe_exits_to_ph_filter() |>
        dplyr::select("Client ID" = UniqueID,
                      "Entry Date" = EntryDate,
                      "Move In Date" = MoveInDateAdjust,
                      "Exit Date" = ExitDate,
                      Destination,
                      "Destination Group" = DestinationGroup,
                      "Meets Objective" = MeetsObjective
                      ) |>
        datatable_default(caption = "PSH: Heads of Household |
                          TH, RRH: Heads of Household Leavers",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))

    })
    
    # Measure 2 NEW
    # % heads of household who returned to homelessness at program exit
    pe_return <- pe_return_to_homelessness() |>
      dplyr::mutate(MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No"),
                    Destination = living_situation(Destination))
    pe_return_to_homelessness_filter <- eventReactive(input$pe_provider, {
      pe_return |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_ReturnToHomelessness <- DT::renderDataTable({
      pe_return_to_homelessness_filter() |>
        dplyr::select("Client ID" = UniqueID,
                      "Entry Date" = EntryDate,
                      "Move In Date" = MoveInDateAdjust,
                      "Exit Date" = ExitDate,
                      Destination,
                      "Destination Group" = DestinationGroup,
                      "Meets Objective" = MeetsObjective
        ) |>
        datatable_default(caption = "Return to Homelessness",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))
      
    })
    
    # Benefits at Exit
    # Measure 3
    pe_benefits <- pe_benefits_at_exit() |>
      dplyr::mutate(
        BenefitsFromAnySource = dplyr::case_when(
          BenefitsFromAnySource == 1 ~ "Yes",
          BenefitsFromAnySource == 0 ~ "No",
          is.na(BenefitsFromAnySource) ~ "Missing"),
        InsuranceFromAnySource = dplyr::case_when(
          InsuranceFromAnySource == 1 ~ "Yes",
          InsuranceFromAnySource == 0 ~ "No",
          is.na(InsuranceFromAnySource) ~ "Missing"),
        MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No"))
    pe_benefits_filter <- eventReactive(input$pe_provider, {
      pe_benefits |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_BenefitsAtExit <- DT::renderDataTable({
      pe_benefits_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Move-In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Non-Cash Benefits at Exit" = BenefitsFromAnySource,
          "Health Insurance at Exit" = InsuranceFromAnySource,
          "Meets Objective" = MeetsObjective,
          AltProjectName
        ) |> 
        datatable_default(caption = "ALL Project Types: Adult Leavers who moved into the
              project's housing",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))

    })
    
    # Meaure 6
    # % adult who entered project during the date range and came from streets/emergency shelter only
    pe_res <- pe_res_prior() |>
      dplyr::mutate(
        LivingSituation = HMIS::living_situation(LivingSituation),
        MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No")
      )
    pe_res_filter <- eventReactive(input$pe_provider, {
      pe_res |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_LivingSituationAtEntry <- DT::renderDataTable({
      pe_res_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Residence Prior" = LivingSituation,
          "Meets Objective" = MeetsObjective
        ) |>
        datatable_default(caption = "ALL Project Types: Adults who entered the project
              during the reporting period",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))

    })

    # Measure 7
    # %  adult who entered project during the date range with no income
    pe_entries <- pe_entries_no_income() |> 
      dplyr::mutate(
        MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No"),
        IncomeFromAnySource = dplyr::case_when(
          IncomeFromAnySource == 1 ~ "Yes",
          IncomeFromAnySource == 0 ~ "No",
          IncomeFromAnySource %in% c(8, 9) ~ "Don't Know/Refused",
          IncomeFromAnySource == 99 ~ "Missing")
    )
    pe_entries_filter <- eventReactive(input$pe_provider, {
      pe_entries |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_NoIncomeAtEntry <- DT::renderDataTable({
      pe_entries_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Income From Any Source" = IncomeFromAnySource,
          "Meets Objective" = MeetsObjective
        ) |> 
        datatable_default(caption = "ALL Project Types: Adults who entered the project
              during the reporting period",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))

    })

    # Measure 4
    # % adult participants who gained or increased their total income (from all sources) 
    # as of the end of the reporting period or at program exit
    pe_increase <- pe_increase_income() |> 
      dplyr::mutate(
        MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No")
      )
    
    pe_increase_filter <- eventReactive(input$pe_provider, {
      pe_increase |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_IncreaseIncome <- DT::renderDataTable({
      pe_increase_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Income at Entry" = IncomeAtEntry,
          "Most Recent Income" = IncomeMostRecent,
          "Meets Objective" = MeetsObjective
        ) |> 
        datatable_default(caption = "ALL Project Types: Adults who entered the project
              during the reporting period",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))
      
    })
    
    # Measure 5
    # % adult participants who increased earned income at program exit.
    pe_increase_earned <- pe_increase_earned_income() |> 
      dplyr::mutate(
        MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No")
      )
    
    pe_increase_earned_filter <- eventReactive(input$pe_provider, {
      pe_increase_earned |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_IncreaseEarnedIncome <- DT::renderDataTable({
      pe_increase_earned_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Earned Income at Entry" = EarnedIncomeAtEntry,
          "Most Recent Earned Income" = EarnedIncomeMostRecent,
          "Meets Objective" = MeetsObjective
        ) |> 
        datatable_default(caption = "ALL Project Types: Adults who entered the project
              during the reporting period",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))
      
    })
    
    # TimesHomelessLastThreeYears
    times <- tibble::tribble(
      ~ReferenceNo, ~Description,
      1,  "One time",
      2,  "Two times",
      3,  "Three times",
      4,  "Four or more times",
      8,  "Client doesn't know",
      9,  "Client prefers not to answer",
      99, "Data not collected"
    )

    # MonthsHomelessLastThreeYears
    months <- tibble::tribble(
      ~ReferenceNo, ~Description,
      8,   "Client doesn't know",
      9,   "Client prefers not to answer",
      99,  "Data not collected",
      101, "1",
      102, "2",
      103, "3",
      104, "4",
      105, "5",
      106, "6",
      107, "7",
      108, "8",
      109, "9",
      110, "10",
      111, "11",
      112, "12",
      113, "More than 12 months"
    )

    pe_homeless_history <- pe_homeless_history_index() |>
      dplyr::left_join(times, by = c("TimesHomelessPastThreeYears" = "ReferenceNo")) |>
      dplyr::mutate(TimesHomelessPastThreeYears = Description) |>
      dplyr::select(-Description) |>
      dplyr::left_join(months, by = c("MonthsHomelessPastThreeYears" = "ReferenceNo")) |>
      dplyr::mutate(MonthsHomelessPastThreeYears = Description) |>
      dplyr::select(-Description)
    pe_homeless_history_filter <- eventReactive(input$pe_provider, {
      pe_homeless_history |>
        dplyr::filter(AltProjectName == input$pe_provider) |> 
        dplyr::mutate(DaysHomelessAtEntry = DaysHomelessAtEntry / 86400)
    })
    output$pe_MedianHHI <- DT::renderDataTable({
      pe_homeless_history_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Approximate Date Homeless" = DateToStreetESSH,
          "Days Homeless at Entry" = DaysHomelessAtEntry,
          "Times Homeless Past 3 Years" = TimesHomelessPastThreeYears,
          "Months Homeless Past 3 Years" = MonthsHomelessPastThreeYears,
          "Homeless Hisory Index" = HHI
        ) |>
        datatable_default(caption = "ALL Project Types: Adults who entered the project
              during the reporting period",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))

    })
    
    # Measure 9
    # % heads of household who entered the project during the date range and had an assessment (VI-SPDAT or HARP) recorded in HMIS 
    # (excludes clients for whom a current episode of DV was reported or who reported as currently fleeing)
    pe_scored_at_ph <- pe_scored_at_ph_entry() |>
      dplyr::mutate(MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No"))
    pe_scored_at_ph_filter <- eventReactive(input$pe_provider, {
      pe_scored_at_ph |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_ScoredAtPHEntry <- DT::renderDataTable({
      pe_scored_at_ph_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Meets Objective" = MeetsObjective
        ) |>
        datatable_default(caption = "All Project Types: Heads of Household who entered the
              project during the reporting period",
                          escape = FALSE,
                          options = list(
                            initComplete = DT::JS(
                              "function(settings, json) {",
                              "$('th').css({'text-align': 'center'});",
                              "$('td').css({'text-align': 'center'});",
                              "}"
                            )
                          ))

    })

    
    
  })
}
    
## To be copied in the UI
# mod_body_coc_competition_ui("body_coc_competition_1")
    
## To be copied in the server
# mod_body_coc_competition_server("body_coc_competition_1")
