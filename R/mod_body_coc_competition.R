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
    ui_picker_program(
          inputId = ns("pe_provider"),
          label = "Select your CoC-funded Provider",
          choices = sort(pe_sum_val$AltProjectName) |> unique(),
          selected = pe_sum_val$AltProjectName[1]
        ),
    ui_row(
      title = "Score Summary",
      DT::dataTableOutput(ns("pe_ProjectSummary")),
      status = "info",
      solidHeader = TRUE
      ),
    ui_row(
          # title = "Client Detail",
            "Exits to Permanent Housing",
            DT::dataTableOutput(ns("pe_ExitsToPH")),

            "Benefits & Health Insurance at Exit",
            DT::dataTableOutput(ns("pe_BenefitsAtExit")),
            
            "Living Situation at Entry",
            DT::dataTableOutput(ns("pe_LivingSituationAtEntry")),
            
            "No Income at Entry",
            DT::dataTableOutput(ns("pe_NoIncomeAtEntry")),
            
            "Length of Stay",
            DT::dataTableOutput(ns("pe_LengthOfStay")),
            
            "Median Homeless History Index",
            DT::dataTableOutput(ns("pe_MedianHHI")),
            
            "Long Term Homeless",
            DT::dataTableOutput(ns("pe_LongTermHomeless")),
            
            "VISPDAT Score Completion",
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
    
    output$pe_ProjectSummary <-
      DT::renderDataTable({
        ptc <- pe_summary_final_scoring() |>
          dplyr::filter(AltProjectName == input$pe_provider) |>
          dplyr::pull(ProjectType)

        pe_summary_final_filter <- pe_summary_final_scoring()  |> 
          dplyr::mutate(dplyr::across(tidyselect::everything(),
                                      function(x) gsub("/", "÷", x))) |>
          dplyr::filter(AltProjectName == input$pe_provider)
        
        estimated_score <- pe_summary_final_filter |>
          dplyr::select(
            "Exits to Permanent Housing" = ExitsToPHPoints,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitPoints,
            "Average Length of Stay" = AverageLoSPoints,
            "Living Situation at Entry" = LHResPriorPoints,
            "No Income at Entry" = NoIncomeAtEntryPoints,
            "Median Homeless History Index" = MedianHHIPoints,
            "Long Term Homeless" = LongTermHomelessPoints,
            "VISPDAT Completion at Entry" = ScoredAtEntryPoints,
            "Data Quality" = DQPoints,
            "Prioritization Workgroup" = PrioritizationWorkgroupScore,
            "Housing First" = HousingFirstScore,
            "Prioritization of Chronic" = ChronicPrioritizationScore
          ) |>
          tidyr::pivot_longer(cols = everything(),
                       names_to = "Measure",
                       values_to = "Estimated Score") 
        
        dq <- pe_summary_final_filter |>
          dplyr::select(
            "Exits to Permanent Housing" = ExitsToPHDQ,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitDQ,
            "Average Length of Stay" = AverageLoSDQ,
            "Living Situation at Entry" = LHResPriorDQ,
            "No Income at Entry" = NoIncomeAtEntryDQ,
            "Median Homeless History Index" = MedianHHIDQ,
            "Long Term Homeless" = LTHomelessDQ,
            "VISPDAT Completion at Entry" = ScoredAtEntryDQ,
            "Housing First" = HousingFirstDQ,
            "Prioritization of Chronic" = ChronicPrioritizationDQ
          ) |>
          tidyr::pivot_longer(cols = tidyselect::everything(),
                       names_to = "Measure",
                       values_to = "DQflag")
        
        possible_score <- pe_summary_final_filter |>
          dplyr::select(
            "Exits to Permanent Housing" = ExitsToPHPossible,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitPossible,
            "Average Length of Stay" = AverageLoSPossible,
            "Living Situation at Entry" = LHResPriorPossible,
            "No Income at Entry" = NoIncomeAtEntryPossible,
            "Median Homeless History Index" = MedianHHIPossible,
            "Long Term Homeless" = LongTermHomelessPossible,
            "VISPDAT Completion at Entry" =
              ScoredAtEntryPossible,
            "Data Quality" = DQPossible,
            "Prioritization Workgroup" = PrioritizationWorkgroupPossible,
            "Housing First" = HousingFirstPossible,
            "Prioritization of Chronic" = ChronicPrioritizationPossible
          ) |>
          tidyr::pivot_longer(cols = tidyselect::everything(),
                       names_to = "Measure",
                       values_to = "Possible Score")
        
        calculation <- pe_summary_final_filter |>
          dplyr::select(
            "Exits to Permanent Housing" = ExitsToPHMath,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitMath,
            "Average Length of Stay" = AverageLoSMath,
            "Living Situation at Entry" = LHResPriorMath,
            "No Income at Entry" = NoIncomeAtEntryMath,
            "Median Homeless History Index" = MedianHHIMath,
            "Long Term Homeless" = LongTermHomelessMath,
            "VISPDAT Completion at Entry" =
              ScoredAtEntryMath,
            "Data Quality" = DQMath,
            # "Prioritization Workgroup" = PrioritizationWorkgroupMath,
            # "Housing First" = HousingFirstMath,
            # "Prioritization of Chronic" = ChronicPrioritizationMath
          ) |>
          tidyr::pivot_longer(cols = tidyselect::everything(),
                       names_to = "Measure",
                       values_to = "Calculation")
        
        estimated_score_dq <- estimated_score |> dplyr::left_join(dq, by = "Measure") |>
          dplyr::ungroup() |>
          dplyr::left_join(possible_score, by = "Measure") |>
          dplyr::left_join(calculation, by = "Measure") |>
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
          )
        
        psh <-  estimated_score_dq |>
          dplyr::filter(!Measure %in% c("Moved into Own Housing",
                                 "Average Length of Stay")) |>
          dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
        
        rrh <- estimated_score_dq |>
          dplyr::filter(!Measure %in%
                   c("Long Term Homeless",
                     "Prioritization of Chronic",
                     "Prioritization Workgroup")) |>
          dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
        
        th <- estimated_score_dq |>
          dplyr::filter(!Measure %in% c(
            "Long Term Homeless",
            "Prioritization of Chronic",
            "Prioritization Workgroup"
          )) |>
          dplyr::select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)

        datatable_default(
          if (ptc == 3) {
            psh
          } else if (ptc == 13) {
            rrh
          } else if(ptc == 2) {
            th
          }
        )
      })
    
    output$pe_ExitsToPH <- DT::renderDataTable({
      a <- pe_exits_to_ph() |>
        dplyr::filter(AltProjectName == input$pe_provider) |>
        dplyr::mutate(MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No"),
               Destination = living_situation(Destination)) |>
        dplyr::select("Client ID" = PersonalID,
               "Entry Date" = EntryDate,
               "Move In Date" = MoveInDateAdjust,
               "Exit Date" = ExitDate,
               Destination,
               "Destination Group" = DestinationGroup,
               "Meets Objective" = MeetsObjective)    
      
      datatable_default(a,
                caption = "PSH: Heads of Household | 
              TH, RRH: Heads of Household Leavers")
      
    })
    
    output$pe_BenefitsAtExit <- DT::renderDataTable({
      a <- pe_benefits_at_exit() |>
        dplyr::filter(AltProjectName == input$pe_provider) |>
        dplyr::mutate(
          BenefitsFromAnySource = dplyr::case_when(
            BenefitsFromAnySource == 1 ~ "Yes", 
            BenefitsFromAnySource == 0 ~ "No",
            is.na(BenefitsFromAnySource) ~ "Missing"),
          InsuranceFromAnySource = dplyr::case_when(
            InsuranceFromAnySource == 1 ~ "Yes",
            InsuranceFromAnySource == 0 ~ "No",
            is.na(InsuranceFromAnySource) ~ "Missing"
          ),
          MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")
        ) |>
        dplyr::select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Move-In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Non-Cash Benefits at Exit" = BenefitsFromAnySource,
          "Health Insurance at Exit" = InsuranceFromAnySource,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable_default(a,
                caption = "ALL Project Types: Adult Leavers who moved into the
              project's housing")
      
    })
    
    output$pe_LivingSituationAtEntry <- DT::renderDataTable({
      a <- pe_res_prior() |>
        dplyr::filter(AltProjectName == input$pe_provider) |>
        dplyr::mutate(
          LivingSituation = living_situation(LivingSituation),
          MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No")
        ) |>
        dplyr::select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Residence Prior" = LivingSituation,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable_default(a,
                caption = "ALL Project Types: Adults who entered the project
              during the reporting period")
      
    })
    
    output$pe_NoIncomeAtEntry <- DT::renderDataTable({
      a <- pe_entries_no_income() |>
        dplyr::filter(AltProjectName == input$pe_provider) |>
        dplyr::mutate(
          MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No"),
          IncomeFromAnySource = dplyr::case_when(
            IncomeFromAnySource == 1 ~ "Yes", 
            IncomeFromAnySource == 0 ~ "No",
            IncomeFromAnySource %in% c(8, 9) ~ "Don't Know/Refused",
            IncomeFromAnySource == 99 ~ "Missing")
        ) |>
        dplyr::select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Income From Any Source" = IncomeFromAnySource,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable_default(a,
                caption = "ALL Project Types: Adults who entered the project
              during the reporting period")
      
    })
    
    output$pe_LengthOfStay <- DT::renderDataTable({
      a <- pe_length_of_stay() |>
        dplyr::filter(AltProjectName == input$pe_provider &
                 ProjectType %in% c(2, 8, 13)) |>
        dplyr::select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Move-In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Days in Project" = DaysInProject
        )    
      
      datatable_default(a,
                caption = "RRH, TH: Client Leavers who moved into the project's 
              housing")
      
    })
    
    output$pe_MedianHHI <- DT::renderDataTable({
      
      times <- HUD_specs() |>
        dplyr::filter(DataElement == "TimesHomelessPastThreeYears") |>
        dplyr::select(ReferenceNo, Description)
      
      months <- HUD_specs() |>
        dplyr::filter(DataElement == "MonthsHomelessPastThreeYears") |>
        dplyr::select(ReferenceNo, Description)
      
      a <- pe_homeless_history_index() |>
        dplyr::left_join(times, by = c("TimesHomelessPastThreeYears" = "ReferenceNo")) |>
        dplyr::mutate(TimesHomelessPastThreeYears = Description) |>
        dplyr::select(-Description) |> 
        dplyr::left_join(months, by = c("MonthsHomelessPastThreeYears" = "ReferenceNo")) |>
        dplyr::mutate(MonthsHomelessPastThreeYears = Description) |>
        dplyr::select(-Description) |> 
        dplyr::filter(AltProjectName == input$pe_provider) |>
        dplyr::select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Approximate Date Homeless" = DateToStreetESSH,
          "Days Homeless at Entry" = DaysHomelessAtEntry,
          "Times Homeless Past 3 Years" = TimesHomelessPastThreeYears,
          "Months Homeless Past 3 Years" = MonthsHomelessPastThreeYears,
          "Homeless Hisory Index" = HHI
        )    
      
      datatable_default(a,
                caption = "ALL Project Types: Adults who entered the project 
              during the reporting period")
      
    })
    
    output$pe_LongTermHomeless <- DT::renderDataTable({
      
      times <- HUD_specs() |>
        dplyr::filter(DataElement == "TimesHomelessPastThreeYears") |>
        dplyr::select(ReferenceNo, Description)
      
      months <- HUD_specs() |>
        dplyr::filter(DataElement == "MonthsHomelessPastThreeYears") |>
        dplyr::select(ReferenceNo, Description)
      
      a <- pe_long_term_homeless() |>
        dplyr::filter(ProjectType == 3) |>
        dplyr::left_join(times, by = c("TimesHomelessPastThreeYears" = "ReferenceNo")) |>
        dplyr::mutate(TimesHomelessPastThreeYears = Description) |>
        dplyr::select(-Description) |> 
        dplyr::left_join(months, by = c("MonthsHomelessPastThreeYears" = "ReferenceNo")) |>
        dplyr::mutate(MonthsHomelessPastThreeYears = Description) |>
        dplyr::select(-Description) |> 
        dplyr::filter(AltProjectName == input$pe_provider) |>
        dplyr::mutate(MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No")) |>
        dplyr::select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Approximate Date Homeless" = DateToStreetESSH,
          "Days Homeless at Entry" = CurrentHomelessDuration,
          "Times Homeless Past 3 Years" = TimesHomelessPastThreeYears,
          "Months Homeless Past 3 Years" = MonthsHomelessPastThreeYears,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable_default(a,
                caption = "PSH: Adults who entered the project during the 
              reporting period")
      
    })
    
    output$pe_ScoredAtPHEntry <- DT::renderDataTable({
      
      a <- pe_scored_at_ph_entry() |>
        dplyr::filter(AltProjectName == input$pe_provider) |>
        dplyr::mutate(MeetsObjective = dplyr::if_else(MeetsObjective == 1, "Yes", "No")) |>
        dplyr::select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable_default(a,
                caption = "All Project Types: Heads of Household who entered the 
              project during the reporting period")
      
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_body_coc_competition_ui("body_coc_competition_1")
    
## To be copied in the server
# mod_body_coc_competition_server("body_coc_competition_1")
