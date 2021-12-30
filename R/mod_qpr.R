.qbegin <- lubridate::floor_date(Sys.Date(), "quarter")
strip_id <- function(id) {
  stringr::str_remove(id, "^(?:body\\-)?body\\_qpr\\_")
}
#' @family QPR
#' @title QPR UI Function
#' @description A shiny Module to generate the QPR UI.
#' @param id Internal parameters for {shiny}.
#' @param choices \code{(named list)} of arguments to \link[shinyWidgets]{pickerInput}.
#'  `FALSE` to omit the choice drop-down selector. - choices must be provided 
#'  for the picker to show options. Defaults will be used for omitted arguments 
#'  unless arguments are explicitly set to `NULL`. Defaults are as follows:
#' `list(
#' inputId = ns("region"),
#' label = "Select Region(s)",
#' choices = choices$choices, # 
#' options = shinyWidgets::pickerOptions(liveSearch = TRUE),
#' selected = NULL,
#' width = "70%"
#' )`
#' Default value for choices is found in *global.R* for each tabItem namespace.
#QUESTION Should there be defaults for options?
#' @param date_choices \code{(logical)} whether to show a date range picker


mod_qpr_ui <- function(id, choices = NULL, date_choices = NULL, ns = rlang::caller_env()$ns) {
  # Create labeled Quarter List
  # .quarter_labels <- rev(unique(zoo::Sys.yearqtr() - 6 / 4:zoo::Sys.yearqtr() 
  #+ 1 / 4))
  # slider_choices <- rev(purrr::map(.qs, ~lubridate::yq(.x) - lubridate::days(1)))
  # names(slider_choices) <- .quarter_labels
  force(ns)
  .id <- strip_id(id)
  .defaults <- purrr::compact(list(
  Dates = if (!isFALSE(date_choices)) list(
    inputId = ns("date_range"),
    start = lubridate::floor_date(lubridate::as_date(.qbegin - lubridate::dmonths(4)), "quarter"),
    end = .qbegin
  ),
  Regions = if (!isFALSE(choices))
    list(
      inputId = ns("region"),
      choices = qpr_tab_choices[[.id]]$choices,
      multiple = FALSE
    )
  ))
  .user <- purrr::compact(list(
    Dates = date_choices,
    Regions = choices
  ))
  if (UU::is_legit(.user)) {
    # if there are 
    .defaults[names(.user)] <- purrr::map2(.defaults[names(.user)], .user, ~{
      # replace default params with those supplied by user on a param by param 
      # basis, retaining defaults.
      purrr::list_modify(.x, !!!.y)
    })
  }
  # tabItem Output ----
  
  shiny::tagList(
    ui_header_row(ns("header")),
    ui_row(
      if (shiny::isTruthy(.defaults$Dates))
        do.call(ui_date_range, .defaults$Dates)
      ,
      if (shiny::isTruthy(.defaults$Regions))
        do.call(ui_picker_project, .defaults$Regions)
    )
    ,
    ui_row(
      if (UU::is_legit(qpr_expr[[.id]]$infobox))
        bs4Dash::infoBoxOutput(ns("ib_summary"), width = 12)
      ,
      if (length(qpr_expr[[.id]]$datatable) > 1) {
        tagList(h4(names(qpr_expr[[.id]]$datatable[1])),
        DT::dataTableOutput(ns("dt_detail")),
        h4(names(qpr_expr[[.id]]$datatable[2])),
        DT::dataTableOutput(ns("dt_detail2")))
      } else {
        DT::dataTableOutput(ns("dt_detail"))
      }
        
    )
  )
  
    
}

#' @family QPR
#' @title QPR Server Functions
#' @description A shiny server Module to generate the header, slider, pickers 
#' and plot for each tabitem.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param header \code{(character)} The header text passed to the initial 
#' \link[shiny]{h2} tag in the header.
#' @param ... Additional \code{(list/shiny.tag.list/shiny.tag)}s  to be appended 
#' to the header after the \link[shiny]{h2} tag with `header`. Defaults to
#'  \code{list(h4(input$region), h4(paste(ReportStart, "to", ReportEnd)))} if 
#'  unspecified. 


mod_qpr_server <- function(id, header){
  .id <- strip_id(id)
  if (missing(header)) 
    rlang::abort("Must provide header for mod_QPR_server(",id,")")
  function(input, output, session){
    ns <- session$ns
    
    # Header
    output$header <- shiny::renderUI({
      req(input$date_range)
      server_header(header, date_range = input$date_range)
    })
    
    # Process Data
    data_env <- shiny::reactive(qpr_expr[[.id]]$expr, quoted = TRUE)
    output$ib_summary <- bs4Dash::renderbs4InfoBox(qpr_expr[[.id]]$infobox, 
                                                       quoted = TRUE)
    
    
    if (length(qpr_expr[[.id]]$datatable) > 1) {
      output$dt_detail <- DT::renderDT(qpr_expr[[.id]]$datatable[[1]], quoted = TRUE)
      output$dt_detail2 <- DT::renderDT(qpr_expr[[.id]]$datatable[[2]], quoted = TRUE)
    } else {
      output$dt_detail <- DT::renderDT(qpr_expr[[.id]]$datatable, quoted = TRUE)
    }
      
  }
}

