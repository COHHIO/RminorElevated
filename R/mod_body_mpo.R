goals <-  arrow::read_feather("data/mahoning_goals.feather") |>
  tidyr::pivot_longer(- tidyselect::all_of(c("Measure", "Operator")),  names_to = "ProjectType",
                      values_to = "Goal") |>
  dplyr::mutate(ProjectType = as.numeric(ProjectType)) |>
  dplyr::filter(!is.na(Goal))

#' body_coc_competition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_mpo_ui <- function(id){
  ns <- NS(id)
  report_end <- lubridate::floor_date(Sys.Date(), "month") - lubridate::days(1)
  tagList(
    ui_header_row(),
    ui_picker_program(
      inputId = ns("mpo_type"),
      label = "Select your Project Type",
      choices = c("PH – Permanent Supportive Housing",
                  "Emergency Shelter – Entry Exit",
                  "PH – Rapid Re-Housing",
                  "Transitional Housing"),
      selected = "Emergency Shelter – Entry Exit",
      multiple = FALSE
    ),
    ui_date_range(start = lubridate::floor_date(report_end, "month"),
                  end = report_end),
    ui_row(
      title = "Length of Stay",
      DT::dataTableOutput(ns("mpo_LengthOfStay"))
    ),
    ui_row(
      title = "Health Insurance",
      DT::dataTableOutput(ns("mpo_HealthInsurance"))
    ),
    ui_row(
      title = "Noncash Benefits",
      DT::dataTableOutput(ns("mpo_NoncashBenefits"))
    ),
    ui_row(
      title = "Income Growth",
      DT::dataTableOutput(ns("mpo_IncomeGrowth"))
    ),
    ui_row(
      title = "Rapid Placement for RRH",
      DT::dataTableOutput(ns("mpo_Placement"))
    ),
    ui_row(
      title = "Exits to Permanent Housing",
      DT::dataTableOutput(ns("mpo_PermanentHousing"))
    )
  )
}

#' body_coc_competition Server Functions
#'
#' @noRd 
mod_body_mpo_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI(server_header("Mahoning Performance & Outcomes"))
    
    server_debounce(input$mpo_type, input$date_range)
    
    measure <- eventReactive(input$mpo_type, {
      req(input$mpo_type)
      goals |>dplyr::mutate(
        ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)
      ) |> 
      dplyr::filter(ProjectType == input$mpo_type)
    })

    #### Length of Stay
    mpo_leavers <- eventReactive(input$date_range, {
      req(input$date_range)
      qpr_leavers() |>
        dplyr::filter(ProgramCoC == "OH-504") |> 
        dplyr::filter(!(ProjectName %in% c("Mahoning - Family and Community Services - Veteran's Haven - GPD - TH",
                                           "Mahoning - Alliance for Children & Families, Inc. - TANF RRH"))) |> 
      HMIS::exited_between(input$date_range[1], input$date_range[2]) |> 
      dplyr::filter(((
        !is.na(MoveInDateAdjust) & ProjectType == 13
      ) |
        (
          !is.na(ExitDate) & ProjectType %in% c(0, 1, 2, 8)
        ))
      ) |> 
      dplyr::mutate(
        ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)
      )
    })
    
    mpo_length_of_stay <- eventReactive(input$mpo_type, {
      req(input$mpo_type)
      mpo_leavers() |>
        dplyr::filter(ProjectType == input$mpo_type) |> 
        dplyr::group_by(ProjectName) |>
        dplyr::summarise(Average = round(mean(DaysinProject), 1),
                         Median = median(DaysinProject), .groups = "drop_last")
    })
    
    
    output$mpo_LengthOfStay <-
      DT::renderDataTable({
        length_of_stay <- mpo_length_of_stay()
        length_of_stay_measure <- measure()
        
        goal <- length_of_stay_measure |> 
          dplyr::filter(Measure == "Average Length of Stay") |> 
          dplyr::pull(Goal)
        if (length(goal) > 0) {
          datatable_default(
            length_of_stay,
            options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                           list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                  list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                           75, 100, 1000), lengthChange = TRUE, pageLength = 10)
          ) |> 
            DT::formatStyle(
              'Average',
              backgroundColor = DT::styleInterval(goal, c('green', 'red'))
            )
        }
      }) 
    
    #### Health Insurance
    mpo_benefits <- eventReactive(input$date_range, {
      qpr_benefits() |>
      HMIS::exited_between(input$date_range[1], input$date_range[2])
    })
    
    mpo_health <- eventReactive(input$mpo_type, {
      mpo_benefits_m <- mpo_benefits() |>
        dplyr::filter(ProgramCoC == "OH-504") |> 
        dplyr::filter(!(ProjectName %in% c("Mahoning - Family and Community Services - Veteran's Haven - GPD - TH",
                                           "Mahoning - Alliance for Children & Families, Inc. - TANF RRH"))) |>
        dplyr::filter(ProjectType == input$mpo_type)
      
      data <- dplyr::left_join(
        # all_hhs
        mpo_benefits_m |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
        # meeting_objective
        mpo_benefits_m |> 
          dplyr::filter(InsuranceFromAnySource == 1) |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(InsuranceAtExit = dplyr::n(), .groups = "drop_last"),
        by = c("ProjectName")
      ) |> 
        dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
        dplyr::mutate(Percent = InsuranceAtExit / TotalHHs)
      
      data
      
    })
    
    output$mpo_HealthInsurance <-
      DT::renderDataTable({
        health <- mpo_health()
        health_measure <- measure()
        
        goal <- health_measure |> 
          dplyr::filter(Measure == "Health Insurance at Exit") |> 
          dplyr::pull(Goal)
        
        if (length(goal) > 0) {
          datatable_default(
            health,
            options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                           list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                  list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                           75, 100, 1000), lengthChange = TRUE, pageLength = 10)
          ) |>
            DT::formatStyle(
              'Percent',
              backgroundColor = DT::styleInterval(goal, c('red', 'green'))
            ) |> 
            DT::formatPercentage(c("Percent"), 1)
        }
      })
    
    #### Non-cash benefits at Exit
    
    mpo_noncash <- eventReactive(input$mpo_type, {
      mpo_benefits_m <- mpo_benefits() |>
        dplyr::filter(ProgramCoC == "OH-504") |> 
        dplyr::filter(!(ProjectName %in% c("Mahoning - Family and Community Services - Veteran's Haven - GPD - TH",
                                           "Mahoning - Alliance for Children & Families, Inc. - TANF RRH"))) |>
        dplyr::filter(ProjectType == input$mpo_type)
      
      data <- dplyr::left_join(
        # all_hhs
        mpo_benefits_m |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
        # meeting_objective
        mpo_benefits_m |> 
          dplyr::filter(BenefitsFromAnySource == 1) |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(BenefitsAtExit = dplyr::n(), .groups = "drop_last"),
        by = c("ProjectName")
      ) |> 
        dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
        dplyr::mutate(Percent = BenefitsAtExit / TotalHHs)
      
      data
      
    })
    
    output$mpo_NoncashBenefits <-
      DT::renderDataTable({
        noncash <- mpo_noncash()
        noncash_measure <- measure()
        
        goal <- noncash_measure |> 
          dplyr::filter(Measure == "Non-cash Benefits") |> 
          dplyr::pull(Goal)
        if (length(goal) > 0) {
          datatable_default(
            noncash,
            options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                           list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                  list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                           75, 100, 1000), lengthChange = TRUE, pageLength = 10)
          ) |> 
            DT::formatStyle(
              'Percent',
              backgroundColor = DT::styleInterval(goal, c('red', 'green'))
            ) |> 
            DT::formatPercentage(c("Percent"), 1)
        }
      })
    
    #### Income Growth
    mpo_income <- eventReactive(input$date_range, {
      qpr_income() |>
      HMIS::served_between(input$date_range[1], input$date_range[2])
    })
    
    mpo_income_growth <- eventReactive(input$mpo_type, {
      mpo_income_m <- mpo_income() |>
        dplyr::filter(ProgramCoC == "OH-504") |> 
        dplyr::filter(!(ProjectName %in% c("Mahoning - Family and Community Services - Veteran's Haven - GPD - TH",
                                           "Mahoning - Alliance for Children & Families, Inc. - TANF RRH"))) |>
        dplyr::filter(ProjectType == input$mpo_type)
      
      data <- dplyr::left_join(
        # all_hhs
        mpo_income_m |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(TotalHHs = dplyr::n(), .groups = "drop_last"),
        # meeting_objective
        mpo_income_m |> 
          dplyr::filter(Difference > 0) |> 
          dplyr::group_by(ProjectName) |>
          dplyr::summarise(Increased = dplyr::n(), .groups = "drop_last"),
        by = c("ProjectName")
      ) |> 
        dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) |> 
        dplyr::mutate(Percent = Increased / TotalHHs)
      
      data
      
    })
    
    output$mpo_IncomeGrowth <-
      DT::renderDataTable({
        income_growth <- mpo_income_growth()
        income_measure <- measure()
        
        goal <- income_measure |> 
          dplyr::filter(Measure == "Gain or Increase Income") |> 
          dplyr::pull(Goal)
        
        if (length(goal) > 0) {
          datatable_default(
            income_growth,
            options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                           list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                  list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                           75, 100, 1000), lengthChange = TRUE, pageLength = 10)
          ) |> 
            DT::formatStyle(
              'Percent',
              backgroundColor = DT::styleInterval(goal, c('red', 'green'))
            ) |> 
            DT::formatPercentage(c("Percent"), 1)
        }
      })
    
    #### Rapid Placement for RRH
    mpo_rrh_enterers <- eventReactive(input$date_range, {
      qpr_rrh_enterers() |>
      HMIS::exited_between(input$date_range[1], input$date_range[2])
    })
    
    mpo_placment <- eventReactive(input$mpo_type, {
      mpo_rrh_enterers_m <- mpo_rrh_enterers() |>
        dplyr::filter(ProgramCoC == "OH-504") |> 
        dplyr::filter(!(ProjectName %in% c("Mahoning - Family and Community Services - Veteran's Haven - GPD - TH",
                                           "Mahoning - Alliance for Children & Families, Inc. - TANF RRH"))) |>
        dplyr::mutate(
          ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)
        ) |> 
        dplyr::filter(ProjectType == input$mpo_type)
      
      data <- mpo_rrh_enterers_m |>
        dplyr::mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) |>
        dplyr::group_by(ProjectName) |>
        dplyr::summarise(AvgDaysToHouse = round(mean(DaysToHouse, na.rm = TRUE), 0), .groups = "drop_last")
      
      data
      
    })
    
    output$mpo_Placement <-
      DT::renderDataTable({
        placement <- mpo_placment()
        placement_measure <- measure()
        
        goal <- placement_measure |> 
          dplyr::filter(Measure == "Rapid Placement for RRH") |> 
          dplyr::pull(Goal)

        if (length(goal) > 0) {
          datatable_default(
            placement,
            options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                           list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                  list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                           75, 100, 1000), lengthChange = TRUE, pageLength = 10)
          ) |> 
            DT::formatStyle(
              'AvgDaysToHouse',
              backgroundColor = DT::styleInterval(goal, c('red', 'green'))
            )
        }
      })
    
    #### Exits to Permanent Housing
    
    SuccessfullyPlaced <- eventReactive(c(input$mpo_type,input$date_range), {
      mpo_leavers <- qpr_leavers() |>
        dplyr::filter(ProgramCoC == "OH-504") |> 
        dplyr::filter(!(ProjectName %in% c("Mahoning - Family and Community Services - Veteran's Haven - GPD - TH",
                                           "Mahoning - Alliance for Children & Families, Inc. - TANF RRH"))) |>
          dplyr::mutate(ProjectTypeLong = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |>
          dplyr::filter(ProjectTypeLong == input$mpo_type)

      exited <- mpo_leavers |>
        HMIS::exited_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      served <- mpo_leavers |>
        HMIS::served_between(input$date_range[1], input$date_range[2], lgl = TRUE)

      psh_hp <- mpo_leavers$ProjectType %in% c(3, 9, 12)
      es_th_sh_out_rrh <- mpo_leavers$ProjectType %in% c(0, 1, 2, 4, 8, 13)

      data <- dplyr::filter(mpo_leavers,
                                          ((ProjectType %in% c(3, 9, 13) &
                                              !is.na(MoveInDateAdjust)) |
                                             ProjectType %in% c(0, 1, 2, 4, 8, 12)
                                          ) &
                                            # excluding non-mover-inners
                                            (((DestinationGroup == "Permanent" |
                                                 #exited to ph or still in PSH/HP
                                                 is.na(ExitDate)) &
                                                psh_hp & # PSH & HP
                                                served
                                            ) |
                                              (
                                                DestinationGroup == "Permanent" & # exited to ph
                                                  es_th_sh_out_rrh &
                                                  exited
                                              )
                                            ))

      data

    })
    
    TotalHHsSuccessfulPlacement <- eventReactive(c(input$mpo_type,input$date_range), {
      mpo_leavers <- qpr_leavers() |>
        dplyr::filter(ProgramCoC == "OH-504") |> 
        dplyr::filter(!(ProjectName %in% c("Mahoning - Family and Community Services - Veteran's Haven - GPD - TH",
                                           "Mahoning - Alliance for Children & Families, Inc. - TANF RRH"))) |>
        dplyr::mutate(ProjectTypeLong = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |>
        dplyr::filter(ProjectTypeLong == input$mpo_type)

      exited <- mpo_leavers |>
        HMIS::exited_between(input$date_range[1], input$date_range[2], lgl = TRUE)
      served <- mpo_leavers |>
        HMIS::served_between(input$date_range[1], input$date_range[2], lgl = TRUE)

      psh_hp <- mpo_leavers$ProjectType %in% c(3, 9, 12)
      es_th_sh_out_rrh <- mpo_leavers$ProjectType %in% c(0, 1, 2, 4, 8, 13)

      # # calculating the total households to compare successful placements to
      data <- dplyr::filter(mpo_leavers, ProjectTypeLong == input$mpo_type) |>
        dplyr::filter((served & psh_hp) # PSH & HP
                      |
                        (exited & es_th_sh_out_rrh) # ES, TH, SH, OUT, RRH
        )

      data
    })


    
     

    # Table
    output$mpo_PermanentHousing <- DT::renderDataTable({
      successfully_placed <- SuccessfullyPlaced()
      Success <- successfully_placed |> dplyr::count(ProjectName)

      total_hhs_successful_placement <- TotalHHsSuccessfulPlacement()
      Total <- total_hhs_successful_placement |> dplyr::count(ProjectName)

      PHTable <- dplyr::left_join(Success, Total, by = "ProjectName") |>
        dplyr::rename("Successfully Placed" = n.x,
                      "Total Households" = n.y) |>
        dplyr::mutate(Percent = `Successfully Placed` / `Total Households`)

      permanent_housing_measure <- measure()

      goal <- permanent_housing_measure |>
        dplyr::filter(Measure == "Exits to Permanent Housing") |>
        dplyr::pull(Goal)

      if (length(goal) > 0) {
        datatable_default(
          PHTable,
          options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                         list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                         75, 100, 1000), lengthChange = TRUE, pageLength = 10)
        ) |>
          DT::formatStyle(
            'Percent',
            backgroundColor = DT::styleInterval(goal, c('red', 'green'))
          ) |>
          DT::formatPercentage(c("Percent"), 1)
      }

    })
    
    
    
  })
}

## To be copied in the UI
# mod_body_coc_competition_ui("body_coc_competition_1")

## To be copied in the server
# mod_body_coc_competition_server("body_coc_competition_1")
