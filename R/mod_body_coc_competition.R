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
  tagList(
    tabItem(
      tabName = "cocCompetitionTab",
      fluidRow(box(
        htmlOutput("headerCoCCompetitionProjectLevel"),
        width = 12
      )),
      fluidRow(box(
        pickerInput(
          inputId = "pe_provider",
          label = "Select your CoC-funded Provider",
          choices = sort(pe_validation_summary()$AltProjectName) %>%
            unique(),
          selected = pe_validation_summary()$AltProjectName[1],
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = 'contains'),
          width = "100%"
        ),
        width = 12
      )),
      fluidRow(
        box(
          DT::dataTableOutput("pe_ProjectSummary"),
          width = 12,
          title = "Score Summary",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE
        )
      ),
      fluidRow(
        tabBox(
          id = "tabs",
          # title = "Client Detail",
          tabPanel(
            "Exits to Permanent Housing",
            DT::dataTableOutput("pe_ExitsToPH")
          ),
          # tabPanel("Moved into Own Housing",
          #          DT::dataTableOutput("pe_OwnHousing")),
          # tabPanel(
          #   "Increased Income",
          #   DT::dataTableOutput("pe_IncreasedIncome")
          # ),
          tabPanel(
            "Benefits & Health Insurance at Exit",
            DT::dataTableOutput("pe_BenefitsAtExit")
          ),
          tabPanel(
            "Living Situation at Entry",
            DT::dataTableOutput("pe_LivingSituationAtEntry")
          ),
          tabPanel(
            "No Income at Entry",
            DT::dataTableOutput("pe_NoIncomeAtEntry")
          ),
          tabPanel("Length of Stay",
                   DT::dataTableOutput("pe_LengthOfStay")),
          tabPanel(
            "Median Homeless History Index",
            DT::dataTableOutput("pe_MedianHHI")
          ),
          tabPanel(
            "Long Term Homeless",
            DT::dataTableOutput("pe_LongTermHomeless")
          ),
          tabPanel(
            "VISPDAT Score Completion",
            DT::dataTableOutput("pe_ScoredAtPHEntry")
          ),
          width = 12
        )
      )
    )
  )
}
    
#' body_coc_competition Server Functions
#'
#' @noRd 
mod_body_coc_competition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$headerCoCCompetitionProjectLevel <- renderUI({
      # Remove these dates
      next_thing_due <- tribble(
        ~ DueDate, ~ Event,
        ymd(rm_dates()$hc$project_eval_docs_due), "Projects submit program documents to evidence 
      best practices and CE Prioritization compliance",
      ymd("20210506"), "All HMIS data corrections must be complete by 11:59pm",
      ymd("20210507"), "Project Evaluation data is saved as final data for scoring",
      ymd("20210527"), "CoC staff post online preliminary renewal project ranking",
      ymd("20210604"), "Recipients submit appeals of project evaluation results and 
      ranking to ohioboscoc@cohhio.org.",
      ymd("20210625"), "Final CoC project ranking released"
      ) %>%
        mutate(
          ShowStart = lag(ymd(DueDate), n = 1L, order_by = DueDate),
          ShowStart = if_else(is.na(ShowStart), today(), ShowStart + days(1)),
          ShowEnd = ymd(DueDate),
          DateRange = interval(ShowStart, ShowEnd)
        ) %>%
        filter(today() %within% DateRange) %>%
        select(Event, DueDate)
      
      list(
        h2("2021 BoS CoC Competition: Project Evaluation"), 
        h4(paste("Fixed Date Range:", 
                 format.Date(rm_dates()$hc$project_eval_start, "%B %d, %Y"), 
                 "to",
                 format.Date(rm_dates()$hc$project_eval_end, "%B %d, %Y"))),
        # h4(strong("THE DATA ON THIS TAB DOES NOT SHOW CHANGES MADE ON OR AFTER
        #           5-7-2021.")),
        h4(input$pe_provider),
        hr(),
        h5(strong("Next Due Date:"),
           format(ymd(next_thing_due$DueDate), "%A %b %e, %Y"),
           "| ",
           next_thing_due$Event),
        p("Please consult the", 
          a("CoC Program",
            href = "https://cohhio.org/boscoc/coc-program/", target="_blank"), 
          "page for complete specifications and timeline.")
      )
    })
    
    output$pe_ProjectSummary <-
      DT::renderDataTable({
        ptc <- pe_summary_final_scoring() %>%
          filter(AltProjectName == input$pe_provider) %>%
          pull(ProjectType)
        
        pe_summary_final_scoring <- pe_summary_final_scoring() %>%
          mutate(
            ExitsToPHMath = str_replace(ExitsToPHMath, "/", "÷"),
            # OwnHousingMath = str_replace(OwnHousingMath, "/", "÷"),
            # IncreasedIncomeMath = str_replace(IncreasedIncomeMath, "/", "÷"),
            BenefitsAtExitMath = str_replace(BenefitsAtExitMath, "/", "÷"),
            AverageLoSMath = str_replace(AverageLoSMath, "/", "÷"),
            LHResPriorMath = str_replace(LHResPriorMath, "/", "÷"),
            NoIncomeAtEntryMath = str_replace(NoIncomeAtEntryMath, "/", "÷"),
            MedianHHIMath = str_replace(MedianHHIMath, "/", "÷"),
            LongTermHomelessMath = str_replace(LongTermHomelessMath, "/", "÷"),
            ScoredAtEntryMath = str_replace(ScoredAtEntryMath, "/", "÷"),
            DQMath = str_replace(DQMath, "/", "÷"),
            PrioritizationWorkgroupMath = str_replace(PrioritizationWorkgroupMath, "/", "÷"),
            HousingFirstMath = str_replace(HousingFirstMath, "/", "÷"),
            ChronicPrioritizationMath = str_replace(ChronicPrioritizationMath, "/", "÷")
          )
        
        a <- pe_summary_final_scoring %>%
          filter(AltProjectName == input$pe_provider) %>%
          select(
            "Exits to Permanent Housing" = ExitsToPHPoints,
            # "Moved into Own Housing" = OwnHousingPoints,
            # "Increased Income" = IncreasedIncomePoints,
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
          ) %>%
          pivot_longer(cols = everything(),
                       names_to = "Measure",
                       values_to = "Estimated Score")
        
        b <- pe_summary_final_scoring %>%
          filter(AltProjectName == input$pe_provider) %>%
          select(
            "Exits to Permanent Housing" = ExitsToPHDQ,
            # "Moved into Own Housing" = OwnHousingDQ,
            # "Increased Income" = IncreasedIncomeDQ,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitDQ,
            "Average Length of Stay" = AverageLoSDQ,
            "Living Situation at Entry" = LHResPriorDQ,
            "No Income at Entry" = NoIncomeAtEntryDQ,
            "Median Homeless History Index" = MedianHHIDQ,
            "Long Term Homeless" = LTHomelessDQ,
            "VISPDAT Completion at Entry" = ScoredAtEntryDQ,
            "Housing First" = HousingFirstDQ,
            "Prioritization of Chronic" = ChronicPrioritizationDQ
          ) %>%
          pivot_longer(cols = everything(),
                       names_to = "Measure",
                       values_to = "DQflag")
        
        c <- pe_summary_final_scoring %>%
          filter(AltProjectName == input$pe_provider) %>%
          select(
            "Exits to Permanent Housing" = ExitsToPHPossible,
            # "Moved into Own Housing" = OwnHousingPossible,
            # "Increased Income" = IncreasedIncomePossible,
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
          ) %>%
          pivot_longer(cols = everything(),
                       names_to = "Measure",
                       values_to = "Possible Score")
        
        d <- pe_summary_final_scoring %>%
          filter(AltProjectName == input$pe_provider) %>%
          select(
            "Exits to Permanent Housing" = ExitsToPHMath,
            # "Moved into Own Housing" = OwnHousingMath,
            # "Increased Income" = IncreasedIncomeMath,
            "Benefits & Health Insurance at Exit" = BenefitsAtExitMath,
            "Average Length of Stay" = AverageLoSMath,
            "Living Situation at Entry" = LHResPriorMath,
            "No Income at Entry" = NoIncomeAtEntryMath,
            "Median Homeless History Index" = MedianHHIMath,
            "Long Term Homeless" = LongTermHomelessMath,
            "VISPDAT Completion at Entry" =
              ScoredAtEntryMath,
            "Data Quality" = DQMath,
            "Prioritization Workgroup" = PrioritizationWorkgroupMath,
            "Housing First" = HousingFirstMath,
            "Prioritization of Chronic" = ChronicPrioritizationMath
          ) %>%
          pivot_longer(cols = everything(),
                       names_to = "Measure",
                       values_to = "Calculation")
        
        psh <- a %>% left_join(b, by = "Measure") %>%
          ungroup() %>%
          left_join(c, by = "Measure") %>%
          left_join(d, by = "Measure") %>%
          mutate(
            DQ = case_when(
              DQflag == 0 ~ "Data Quality passes",
              DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
            )
          ) %>%
          filter(!Measure %in% c("Moved into Own Housing",
                                 "Average Length of Stay")) %>%
          select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
        
        rrh <- a %>% left_join(b, by = "Measure") %>%
          ungroup() %>%
          left_join(c, by = "Measure") %>%
          left_join(d, by = "Measure") %>%
          mutate(
            DQ = case_when(
              DQflag == 0 ~ "Data Quality passes",
              DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
            )
          ) %>%
          filter(!Measure %in%
                   c("Long Term Homeless",
                     "Prioritization of Chronic",
                     "Prioritization Workgroup")) %>%
          select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
        
        th <- a %>% left_join(b, by = "Measure") %>%
          ungroup() %>%
          left_join(c, by = "Measure") %>%
          left_join(d, by = "Measure") %>%
          mutate(
            DQ = case_when(
              DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
            )
          ) %>%
          filter(!Measure %in% c(
            "Long Term Homeless",
            "Prioritization of Chronic",
            "Prioritization Workgroup"
          )) %>%
          select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
        
        datatable(
          if (ptc == 3) {
            psh
          } else if (ptc == 13) {
            rrh
          } else if(ptc == 2) {
            th
          },
          rownames = FALSE,
          options = list(dom = 't',
                         pageLength = 100)
        )
      })
    
    output$pe_ExitsToPH <- DT::renderDataTable({
      a <- pe_exits_to_ph() %>%
        filter(AltProjectName == input$pe_provider) %>%
        mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No"),
               Destination = living_situation(Destination)) %>%
        select("Client ID" = PersonalID,
               "Entry Date" = EntryDate,
               "Move In Date" = MoveInDateAdjust,
               "Exit Date" = ExitDate,
               Destination,
               "Destination Group" = DestinationGroup,
               "Meets Objective" = MeetsObjective)    
      
      datatable(a,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'),
                caption = "PSH: Heads of Household | 
              TH, RRH: Heads of Household Leavers")
      
    })
    
    # output$pe_OwnHousing <- DT::renderDataTable({
    #   
    #   a <- pe_own_housing() %>%
    #     filter(AltProjectName == input$pe_provider) %>%
    #     mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No"),
    #            Destination = living_situation(Destination)) %>%
    #     select(
    #       "Client ID" = PersonalID,
    #       "Entry Date" = EntryDate,
    #       "Exit Date" = ExitDate,
    #       Destination,
    #       "Destination Group" = DestinationGroup,
    #       "Meets Objective" = MeetsObjective
    #     )    
    #   
    #   datatable(a,
    #             rownames = FALSE,
    #             filter = 'top',
    #             options = list(dom = 'ltpi'),
    #             caption = "RRH, TH, SH: Heads of Household Leavers who moved into 
    #             the project's housing")
    #   
    # })
    
    output$pe_BenefitsAtExit <- DT::renderDataTable({
      a <- pe_benefits_at_exit() %>%
        filter(AltProjectName == input$pe_provider) %>%
        mutate(
          BenefitsFromAnySource = case_when(
            BenefitsFromAnySource == 1 ~ "Yes", 
            BenefitsFromAnySource == 0 ~ "No",
            is.na(BenefitsFromAnySource) ~ "Missing"),
          InsuranceFromAnySource = case_when(
            InsuranceFromAnySource == 1 ~ "Yes",
            InsuranceFromAnySource == 0 ~ "No",
            is.na(InsuranceFromAnySource) ~ "Missing"
          ),
          MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")
        ) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Move-In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Non-Cash Benefits at Exit" = BenefitsFromAnySource,
          "Health Insurance at Exit" = InsuranceFromAnySource,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable(a,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'),
                caption = "ALL Project Types: Adult Leavers who moved into the
              project's housing")
      
    })
    
    # output$pe_IncreasedIncome <- DT::renderDataTable({
    #   a <- pe_increase_income() %>%
    #     filter(AltProjectName == input$pe_provider) %>%
    #     mutate(
    #       IncomeDifference = IncomeMostRecent - IncomeAtEntry,
    #       MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")
    #     ) %>%
    #     select(
    #       "Client ID" = PersonalID,
    #       "Entry Date" = EntryDate,
    #       "Move-In Date" = MoveInDateAdjust,
    #       "Exit Date" = ExitDate,
    #       "Income at Entry" = IncomeAtEntry,
    #       "Most Recent Income" = IncomeMostRecent,
    #       "Increase/Decrease" = IncomeDifference,
    #       "Meets Objective" = MeetsObjective
    #     )    
    #   
    #   datatable(a,
    #             rownames = FALSE,
    #             filter = 'top',
    #             options = list(dom = 'ltpi'),
    #             caption = "ALL Project Types: Adults who moved into the project's
    #             housing")
    #   
    # })
    
    output$pe_LivingSituationAtEntry <- DT::renderDataTable({
      a <- pe_res_prior() %>%
        filter(AltProjectName == input$pe_provider) %>%
        mutate(
          LivingSituation = living_situation(LivingSituation),
          MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")
        ) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Residence Prior" = LivingSituation,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable(a,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'),
                caption = "ALL Project Types: Adults who entered the project
              during the reporting period")
      
    })
    
    output$pe_NoIncomeAtEntry <- DT::renderDataTable({
      a <- pe_entries_no_income() %>%
        filter(AltProjectName == input$pe_provider) %>%
        mutate(
          MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No"),
          IncomeFromAnySource = case_when(
            IncomeFromAnySource == 1 ~ "Yes", 
            IncomeFromAnySource == 0 ~ "No",
            IncomeFromAnySource %in% c(8, 9) ~ "Don't Know/Refused",
            IncomeFromAnySource == 99 ~ "Missing")
        ) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Income From Any Source" = IncomeFromAnySource,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable(a,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'),
                caption = "ALL Project Types: Adults who entered the project
              during the reporting period")
      
    })
    
    output$pe_LengthOfStay <- DT::renderDataTable({
      a <- pe_length_of_stay() %>%
        filter(AltProjectName == input$pe_provider &
                 ProjectType %in% c(2, 8, 13)) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Move-In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Days in Project" = DaysInProject
        )    
      
      datatable(a,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'),
                caption = "RRH, TH: Client Leavers who moved into the project's 
              housing")
      
    })
    
    output$pe_MedianHHI <- DT::renderDataTable({
      
      times <- HUD_specs() %>%
        filter(DataElement == "TimesHomelessPastThreeYears") %>%
        select(ReferenceNo, Description)
      
      months <- HUD_specs() %>%
        filter(DataElement == "MonthsHomelessPastThreeYears") %>%
        select(ReferenceNo, Description)
      
      a <- pe_homeless_history_index() %>%
        left_join(times, by = c("TimesHomelessPastThreeYears" = "ReferenceNo")) %>%
        mutate(TimesHomelessPastThreeYears = Description) %>%
        select(-Description)
      
      b <- a %>%
        left_join(months, by = c("MonthsHomelessPastThreeYears" = "ReferenceNo")) %>%
        mutate(MonthsHomelessPastThreeYears = Description) %>%
        select(-Description)
      
      c <- b %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Approximate Date Homeless" = DateToStreetESSH,
          "Days Homeless at Entry" = DaysHomelessAtEntry,
          "Times Homeless Past 3 Years" = TimesHomelessPastThreeYears,
          "Months Homeless Past 3 Years" = MonthsHomelessPastThreeYears,
          "Homeless Hisory Index" = HHI
        )    
      
      datatable(c,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'),
                caption = "ALL Project Types: Adults who entered the project 
              during the reporting period")
      
    })
    
    output$pe_LongTermHomeless <- DT::renderDataTable({
      
      times <- HUD_specs() %>%
        filter(DataElement == "TimesHomelessPastThreeYears") %>%
        select(ReferenceNo, Description)
      
      months <- HUD_specs() %>%
        filter(DataElement == "MonthsHomelessPastThreeYears") %>%
        select(ReferenceNo, Description)
      
      a <- pe_long_term_homeless() %>%
        filter(ProjectType == 3) %>%
        left_join(times, by = c("TimesHomelessPastThreeYears" = "ReferenceNo")) %>%
        mutate(TimesHomelessPastThreeYears = Description) %>%
        select(-Description)
      
      b <- a %>%
        left_join(months, by = c("MonthsHomelessPastThreeYears" = "ReferenceNo")) %>%
        mutate(MonthsHomelessPastThreeYears = Description) %>%
        select(-Description)
      
      c <- b %>%
        filter(AltProjectName == input$pe_provider) %>%
        mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Approximate Date Homeless" = DateToStreetESSH,
          "Days Homeless at Entry" = CurrentHomelessDuration,
          "Times Homeless Past 3 Years" = TimesHomelessPastThreeYears,
          "Months Homeless Past 3 Years" = MonthsHomelessPastThreeYears,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable(c,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'),
                caption = "PSH: Adults who entered the project during the 
              reporting period")
      
    })
    
    output$pe_ScoredAtPHEntry <- DT::renderDataTable({
      
      a <- pe_scored_at_ph_entry() %>%
        filter(AltProjectName == input$pe_provider) %>%
        mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "Meets Objective" = MeetsObjective
        )    
      
      datatable(a,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'),
                caption = "All Project Types: Heads of Household who entered the 
              project during the reporting period")
      
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_body_coc_competition_ui("body_coc_competition_1")
    
## To be copied in the server
# mod_body_coc_competition_server("body_coc_competition_1")
