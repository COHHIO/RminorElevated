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
      title = "Length of Stay",
            DT::dataTableOutput(ns("pe_LengthOfStay"))
    ),
    ui_row(
      title = "Rapid Placement into Housing",
      DT::dataTableOutput(ns("pe_OwnHousing"))
    ),
    ui_row(
      title = "Median Homeless History Index",
            DT::dataTableOutput(ns("pe_MedianHHI"))
    ),
    ui_row(
      title = "Long Term Homeless",
            DT::dataTableOutput(ns("pe_LongTermHomeless"))
    ),
    ui_row(
      title = "VISPDAT Score Completion",
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
    output$header <- renderUI(server_header("2024 CoC Competition Renewal Project Evaluation",
                                            x = shiny::h3(paste0("Reporting Period: 1/1/23 - 12/31/23")),
                                            shiny::p("For more information visit the", a("Ohio BoSCoC CoC Program website", href = "https://cohhio.org/boscoc/coc-program/"))))
    
    pe_summary <- pe_summary_final_scoring() |> 
      dplyr::mutate(dplyr::across(tidyselect::ends_with("Math"),
                                  function(x) gsub("/", "÷", x)))
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
            "Average Length of Stay" = AverageLoSPoints,
            "Rapid Placement into Housing" = OwnHousingPoints,
            "Living Situation at Entry" = LHResPriorPoints,
            "No Income at Entry" = NoIncomeAtEntryPoints,
            "Increased Income" = IncreasedIncomePoints,
            "Median Homeless History Index" = MedianHHIPoints,
            "Long Term Homeless" = LongTermHomelessPoints,
            "VISPDAT Completion at Entry" = ScoredAtEntryPoints,
            "Data Quality" = DQPoints,
            "Prioritization Workgroup" = PrioritizationWorkgroupScore,
            # "Housing First" = HousingFirstScore,
            "Total Points" = TotalScore,
            "Prioritization of Chronic" = ChronicPrioritizationScore
          ) |>
          tidyr::pivot_longer(cols = -c("Project Name"),
                       names_to = "Measure",
                       values_to = "Estimated Score") 
        
        dq <- pe_summary_final_filter() |>
          dplyr::select(
            "Project Name" = AltProjectName,
            "Exits to Permanent Housing" = ExitsToPHDQ,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitDQ,
            "Average Length of Stay" = AverageLoSDQ,
            "Rapid Placement into Housing" = OwnHousingDQ,
            "Living Situation at Entry" = LHResPriorDQ,
            "No Income at Entry" = NoIncomeAtEntryDQ,
            "Increased Income" = IncreasedIncomeDQ,
            "Median Homeless History Index" = MedianHHIDQ,
            "Long Term Homeless" = LTHomelessDQ,
            "VISPDAT Completion at Entry" = ScoredAtEntryDQ,
            # "Housing First" = HousingFirstDQ,
            "Prioritization of Chronic" = ChronicPrioritizationDQ
          ) |>
          tidyr::pivot_longer(cols = -c("Project Name"),
                       names_to = "Measure",
                       values_to = "DQflag")
        
        possible_score <- pe_summary_final_filter() |>
          dplyr::select(
            "Project Name" = AltProjectName,
            "Exits to Permanent Housing" = ExitsToPHPossible,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitPossible,
            "Average Length of Stay" = AverageLoSPossible,
            "Rapid Placement into Housing" = OwnHousingPossible,
            "Living Situation at Entry" = LHResPriorPossible,
            "No Income at Entry" = NoIncomeAtEntryPossible,
            "Increased Income" = IncreasedIncomePossible,
            "Median Homeless History Index" = MedianHHIPossible,
            "Long Term Homeless" = LongTermHomelessPossible,
            "VISPDAT Completion at Entry" =
              ScoredAtEntryPossible,
            "Data Quality" = DQPossible,
            "Prioritization Workgroup" = PrioritizationWorkgroupPossible,
            # "Housing First" = HousingFirstPossible,
            "Prioritization of Chronic" = ChronicPrioritizationPossible
          ) |>
          tidyr::pivot_longer(cols = -c("Project Name"),
                       names_to = "Measure",
                       values_to = "Possible Score")
        
        calculation <- pe_summary_final_filter() |>
          dplyr::select(
            "Project Name" = AltProjectName,
            "Exits to Permanent Housing" = ExitsToPHMath,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitMath,
            "Average Length of Stay" = AverageLoSMath,
            "Rapid Placement into Housing" = OwnHousingMath,
            "Living Situation at Entry" = LHResPriorMath,
            "No Income at Entry" = NoIncomeAtEntryMath,
            "Increased Income" = IncreasedIncomeMath,
            "Median Homeless History Index" = MedianHHIMath,
            "Long Term Homeless" = LongTermHomelessMath,
            "VISPDAT Completion at Entry" =
              ScoredAtEntryMath,
            "Data Quality" = DQMath,
            # "Prioritization Workgroup" = PrioritizationWorkgroupMath,
            # "Housing First" = HousingFirstMath,
            # "Prioritization of Chronic" = ChronicPrioritizationMath
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
          dplyr::filter(!Measure %in% c("Prioritization of Chronic",
                                        "Prioritization Workgroup")) |>
          dplyr::filter(!is.na(`Estimated Score`))
        
        # psh <-  estimated_score_dq |>
        #   dplyr::filter(!Measure %in% c("Moved into Own Housing",
        #                          "Average Length of Stay",
        #                          "Prioritization of Chronic",
        #                          "Prioritization Workgroup")) |>
        #   dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
        # 
        # rrh <- estimated_score_dq |>
        #   dplyr::filter(!Measure %in%
        #            c("Long Term Homeless",
        #              "Prioritization of Chronic",
        #              "Prioritization Workgroup")) |>
        #   dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
        # 
        # th <- estimated_score_dq |>
        #   dplyr::filter(!Measure %in% c(
        #     "Long Term Homeless",
        #     "Prioritization of Chronic",
        #     "Prioritization Workgroup"
        #   )) |>
        #   dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
        
        

        datatable_default(
          estimated_score_dq |> 
            dplyr::select(1, Calculation, 2, "Estimated Score", "Possible Score" = 5, "Data Quality" = DQ),
          options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                         list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                         75, 100, 1000), lengthChange = TRUE, pageLength = 10)
          # purrr::when(ptc,
          #             . == 3 ~ psh,
          #             . == 13 ~ rrh,
          #             . == 2 ~ th
          # )
        )
      })
    
    pe_exits <- pe_exits_to_ph() |>
      dplyr::mutate(MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No"),
                    Destination = HMIS::living_situation(Destination))
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

    # Increased Income
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
    
    # Length of Stay
    pe_length_filter <- eventReactive(input$pe_provider, {
      pe_length_of_stay() |>
        dplyr::filter(AltProjectName == input$pe_provider &
                        ProjectType %in% c(2, 8, 13)) |>
        dplyr::mutate(DaysInProject = DaysInProject / 86400)
    })
    
    output$pe_LengthOfStay <- DT::renderDataTable({
      pe_length_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Move-In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Days in Project" = DaysInProject
        ) |> 
      datatable_default(caption = "RRH, TH: Client Leavers who moved into the project's housing",
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

    # Rapid Placement into Housing
    pe_own_filter <- eventReactive(input$pe_provider, {
      pe_own_housing() |>
        dplyr::filter(AltProjectName == input$pe_provider &
                        ProjectType %in% c(3, 13)) |> 
        dplyr::mutate(DaysInProject = DaysInProject / 86400)
    })
    
    output$pe_OwnHousing <- DT::renderDataTable({
      pe_own_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Move-In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Days in Project" = DaysInProject
        ) |> 
        datatable_default(caption = "RRH, TH: Client Leavers who moved into the project's housing",
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
    
    hud_specs <- HUD_specs()
    times <-  hud_specs |>
      dplyr::filter(DataElement == "TimesHomelessPastThreeYears") |>
      dplyr::select(ReferenceNo, Description)

    months <-  hud_specs |>
      dplyr::filter(DataElement == "MonthsHomelessPastThreeYears") |>
      dplyr::select(ReferenceNo, Description)

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
    
    pe_long_homeless <- pe_long_term_homeless() |>
      dplyr::filter(ProjectType == 3) |>
      dplyr::left_join(times, by = c("TimesHomelessPastThreeYears" = "ReferenceNo")) |>
      dplyr::mutate(TimesHomelessPastThreeYears = Description) |>
      dplyr::select(-Description) |>
      dplyr::left_join(months, by = c("MonthsHomelessPastThreeYears" = "ReferenceNo")) |>
      dplyr::mutate(MonthsHomelessPastThreeYears = Description,
                    MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No")) |>
      dplyr::select(-Description)
      
    pe_long_homeless_filter <- eventReactive(input$pe_provider, {
      pe_long_homeless |>
        dplyr::filter(AltProjectName == input$pe_provider)
    })
    output$pe_LongTermHomeless <- DT::renderDataTable({
      pe_long_homeless_filter() |>
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Approximate Date Homeless" = DateToStreetESSH,
          "Days Homeless at Entry" = CurrentHomelessDuration,
          "Times Homeless Past 3 Years" = TimesHomelessPastThreeYears,
          "Months Homeless Past 3 Years" = MonthsHomelessPastThreeYears,
          "Meets Objective" = MeetsObjective
        ) |> 
        datatable_default(caption = "PSH: Adults who entered the project during the
              reporting period",
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
