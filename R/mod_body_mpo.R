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
  mpo <- qpr_income()
  tagList(
    ui_header_row(),
    ui_picker_program(
      inputId = ns("mpo_type"),
      label = "Select your Project Type",
      choices = sort(mpo$ProjectType) |> unique(),
      selected = NULL,
      multiple = FALSE
    ),
    ui_row(
      title = "Lenght of Stay",
      DT::dataTableOutput(ns("mpo_LengthOfStay")),
      status = "info",
      solidHeader = TRUE
    )
    # ui_row(
    #   title = "Exits to Permanent Housing",
    #   DT::dataTableOutput(ns("mpo_PermanentHousing"))
    # )
  )
}

#' body_coc_competition Server Functions
#'
#' @noRd 
mod_body_mpo_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI(server_header("Mahoning Performance & Outcomes"))
    
    #### Length of Stay
    mpo_leavers <- qpr_leavers() |>
      HMIS::exited_between(lubridate::ymd("2021-09-01"), lubridate::ymd("2022-08-31")) |> 
      dplyr::filter(((
        !is.na(MoveInDateAdjust) & ProjectType == 13
      ) |
        (
          !is.na(ExitDate) & ProjectType %in% c(1, 2, 8)
        )) &
        ProjectCounty == "Mahoning/Trumbull"
      ) |> 
      dplyr::mutate(
        ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)
      )
    
    mpo_length_of_stay <- eventReactive(input$mpo_type, {
      mpo_leavers |>
        dplyr::filter(ProjectType == input$mpo_type) |> 
        dplyr::group_by(ProjectName) |>
        dplyr::summarise(Average = round(mean(DaysinProject), 1),
                         Median = median(DaysinProject), .groups = "drop_last")
    })
    
    
    output$mpo_LengthOfStay <-
      DT::renderDataTable({
        length_of_stay <- mpo_length_of_stay()
        
        datatable_default(
          length_of_stay,
          options = list(dom = "Blfrtip", buttons = list("copy", "excel", "csvHtml5",
                                                         list(extend = "csvHtml5", text = "Full CSV", filename = "data_full", exportOptions =
                                                                list(modifier = list(page = "all")))), responsive = TRUE, lengthMenu = c(10, 25, 50,
                                                                                                                                         75, 100, 1000), lengthChange = TRUE, pageLength = 10)
        )
      })
    
    #### Exits to Permanent Housing
    # leavers <- qpr_leavers()
    # 
    # by_type <- eventReactive(input$mpo_type, {
    #   leavers |> 
    #     dplyr::filter(ProjectType == input$mpo_type & ProjectCounty == "Mahoning/Trumbull")
    # })
    # exited <- by_type |> 
    #   HMIS::exited_between(lubridate::ymd("2021-09-01"), lubridate::ymd("2022-08-31"), lgl = TRUE)
    # served <- by_type |> 
    #   HMIS::served_between(lubridate::ymd("2021-09-01"), lubridate::ymd("2022-08-31"), lgl = TRUE)
    # 
    # 
    # psh_hp <- by_type$ProjectType %in% c(3, 9, 12)
    # es_th_sh_out_rrh <- by_type$ProjectType %in% c(1, 2, 4, 8, 13)
    
    # SuccessfullyPlaced <- dplyr::filter(by_type,
    #                                     ((ProjectType %in% c(3, 9, 13) &
    #                                         !is.na(MoveInDateAdjust)) |
    #                                        ProjectType %in% c(1, 2, 4, 8, 12)
    #                                     ) &
    #                                       # excluding non-mover-inners
    #                                       (((DestinationGroup == "Permanent" |
    #                                            #exited to ph or still in PSH/HP
    #                                            is.na(ExitDate)) &
    #                                           psh_hp & # PSH & HP
    #                                           served
    #                                       ) |
    #                                         (
    #                                           DestinationGroup == "Permanent" & # exited to ph
    #                                             es_th_sh_out_rrh &
    #                                             exited
    #                                         )
    #                                       ))
    
    # calculating the total households to compare successful placements to
    # TotalHHsSuccessfulPlacement <- 
    #   dplyr::filter(by_type,
    #                 (served & psh_hp) # PSH & HP
    #                 |
    #                   (exited & es_th_sh_out_rrh) # ES, TH, SH, OUT, RRH
    #   )
    # Success <- SuccessfullyPlaced |> dplyr::count(ProjectName)
    # Total <- TotalHHsSuccessfulPlacement |> dplyr::count(ProjectName)
    
    # PHTable <- dplyr::left_join(Success, Total, by = "ProjectName")
    
    # Table
    # output$mpo_PermanentHousing <- DT::renderDataTable({
    #   Total |>
    #     datatable_default(caption = "PSH: Heads of Household |
    #                       TH, RRH: Heads of Household Leavers",
    #                       escape = FALSE,
    #                       options = list(
    #                         initComplete = DT::JS(
    #                           "function(settings, json) {",
    #                           "$('th').css({'text-align': 'center'});",
    #                           "$('td').css({'text-align': 'center'});",
    #                           "}"
    #                         )
    #                       ))
      
    # })
    
    
    
  })
}

## To be copied in the UI
# mod_body_coc_competition_ui("body_coc_competition_1")

## To be copied in the server
# mod_body_coc_competition_server("body_coc_competition_1")
