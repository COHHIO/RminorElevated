#' body_dq_system_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_dq_system_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    uiOutput(ns("ce")),
    uiOutput(ns("summary")),
    uiOutput(ns("overlaps"))
  )
}

c("High Priority Issues & Errors by Program", "Error Types", "Warnings by Program", "Warning Types", "Household Errors by Program", "Old Referrals by Program", "Eligibility Issues by Program", "Clients without SPDAT by Program", "Programs with Overlaps")

#' body_dq_system_summary Server Functions
#'
#' @noRd 
mod_body_dq_system_summary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- output$header <- renderUI({
      server_header(title = "Data Quality",
                    shiny::h3("System Summary"),
                    date_range = c(rm_dates()$hc$check_dq_back_to, Sys.Date()))
    })
    dq_aps_no_referrals <- dq_aps_no_referrals()
    dq_aps_referrals <- programs |> 
      {\(x) {tibble::tibble(ProjectID = x, ProjectName = names(x))}}() |> 
      dplyr::filter(stringr::str_detect(ProjectName, "^zz", negate = TRUE) & stringr::str_detect(ProjectName, "\\sAP\\s?") & !ProjectID %in% dq_aps_no_referrals$ProjectID) 
    
    output$ce <- renderUI({
      ui_row(title = "Coordinated Entry",
             dq_APs() |> 
               {\(x) {
                 values <- round(x$percent * 100, 0)  # Round to integers
                 cat("Integer values:", values, "\n")
                 cat("Sum of integers:", sum(values), "\n")
                 
                 bs4Dash::bs4MultiProgressBar(
                   value = values,
                   max = 100,
                   status = c("danger", "success"),
                   striped = c(TRUE, FALSE),
                   animated = rep(FALSE, length(values)),
                   label = paste0(x$category, ": ", x$count, " (", scales::percent(x$percent),")")
                 )
               }}(),
             fluidRow(
               bs4Dash::column(6,
                               datatable_default(dq_aps_no_referrals, add_options = list(pageLength = 20))
                               ),
               bs4Dash::column(6,
                               datatable_default(dq_aps_referrals, add_options = list(pageLength = 20)))
             )
             
      )
    })
    dq_summary <- dq_summary()
    
    dq_summary_args <- tibble::tribble(
      ~ id,
      ~ title,
      ~ status,
      "projects_errors",
      "High Priority Issues & Errors by Project",
      "danger",
      "projects_warnings",
      "Warnings by Project",
      "warning",
      "error_types",
      "Error Types",
      "danger",
      "warning_types",
      "Warning Types",
      "warning",
      "hh_issues",
      "Household Errors by Project",
      "danger",
      "outstanding_referrals",
      "Old Referrals by Project",
      "warning",
      "eligibility",
      "Eligibility Issues by Project",
      "warning",
      "clients_without_spdat",
      "Households without HARP or SPDAT by Project",
      "warning",
      "overlaps",
      "Overlapping Enrollments by Project",
      "danger",
      "long_stayer",
      "Long Stayers by Project",
      "warning",
      "psh_destination",
      "Incorrect PSH Destination by Project",
      "danger",
      "incorrect_destination",
      "Incorrect Destinations by Project",
      "warning"
    )
    
    dq_summary_args <- dq_summary_args |>
      dplyr::mutate(table = purrr::map(id, ~{
        out <- dq_summary[[.x]]
        if ("n_Issue" %in% names(out))
          out <- dplyr::rename(out, `# of Issues` = "n_Issue")
        out <- dplyr::select(out, -dplyr::any_of(c("Total Clients", "ProjectID"))) 
        
        datatable_default(out, add_options = list(pageLength = 20)) |>
          datatable_add_bars(divergent = TRUE) |>
          datatable_options_update(hide_cols = "from_mean", options = list(columnDefs = list(
            list(width = "20px", targets = which_cols(c(
              "# of Issues", "Frequency"
            ), out) - 1)
          )))
        
        }),
        solidHeader = TRUE,
        collapsed = TRUE)
    
    output$summary <- renderUI({
        rlang::exec(ui_row, title = "System-wide Summary",
               !!!make_columns(dq_summary_args, max_cols = 2, fn = bs4Dash::box),
               width = 12,
               box = TRUE)
    })
    
    
    output$desk_time_medians <- renderPlot({
      ggplot(
        head(desk_time_medians, 10L),
        aes(
          x = reorder(ProjectName, MedianDeskTime),
          y = MedianDeskTime,
          fill = MedianDeskTime
        )
      ) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = "",
             y = "Median Days") +
        scale_fill_viridis_c(direction = -1) +
        theme_minimal(base_size = 18)
    })
  })
  
}
    
## To be copied in the UI
# mod_body_dq_system_summary_ui("body_dq_system_summary_1")
    
## To be copied in the server
# mod_body_dq_system_summary_server("body_dq_system_summary_1")
