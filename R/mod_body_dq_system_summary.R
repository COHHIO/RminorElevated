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
    selectInput(ns("window_selector"), "Time Window",
                choices = c("Last Year"     = "last_year",
                            "Last 6 Months" = "last_6months",
                            "Last 3 Months" = "last_3months",
                            "Last Month"    = "last_month"),
                selected = "last_year"),
    uiOutput(ns("ce")),
    uiOutput(ns("summary")),
    uiOutput(ns("overlaps"))
  )
}

#' body_dq_system_summary Server Functions
#'
#' @noRd
mod_body_dq_system_summary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # HMISserve::data_quality_summary() now uploads a named list of per-window
    # summaries (last_year / last_6months / last_3months / last_month) rather
    # than a single flat summary. Fetch it once; index by the selected window.
    dq_summary_all <- get_app_data("dq_summary")

    current_summary <- reactive({
      req(input$window_selector)
      dq_summary_all[[input$window_selector]]
    })

    output$header <- renderUI({
      req(input$window_selector)
      # Match the offsets the backend actually filtered on (years/dmonths) so the
      # displayed range lines up with the data in the selected window.
      start_date <- switch(input$window_selector,
                           last_year    = Sys.Date() - lubridate::years(1),
                           last_6months = Sys.Date() - lubridate::dmonths(6),
                           last_3months = Sys.Date() - lubridate::dmonths(3),
                           last_month   = Sys.Date() - lubridate::dmonths(1),
                           get_app_data("rm_dates")$hc$check_dq_back_to  # fallback
      )

      server_header(title = "Data Quality",
                    shiny::h3("System Summary"),
                    date_range = c(start_date, Sys.Date()))
    })

    # Coordinated Entry / AP section. APs are now computed per-window in the
    # backend and live inside each window as $aps (progress-bar data) and
    # $aps_no_referrals (table) — they are no longer standalone datasets.
    output$ce <- renderUI({
      dq_summary <- current_summary()

      x                   <- dq_summary$aps
      dq_aps_no_referrals <- dq_summary$aps_no_referrals

      dq_aps_referrals <- programs |>
        {\(p) {tibble::tibble(ProjectID = p, ProjectName = names(p))}}() |>
        dplyr::filter(stringr::str_detect(ProjectName, "^zz", negate = TRUE) &
                        stringr::str_detect(ProjectName, "\\sAP\\s?") &
                        !ProjectID %in% dq_aps_no_referrals$ProjectID)

      ui_row(
        title = "Coordinated Entry",

        # Render multiple progress bars
        tagList({
          values <- as.integer(round(x$percent * 100, 0))  # Round to integers
          statuses <- c("danger", "success")  # Match this to the number of values

          lapply(seq_along(values), function(i) {
            bs4Dash::progressBar(
              value = values[i],
              status = statuses[i],
              label = paste0(
                x$category[i], ": ",
                x$count[i], " (",
                scales::percent(x$percent[i]), ")"
              )
            )
          })
        }),

        # Data tables
        fluidRow(
          bs4Dash::column(6,
                          datatable_default(current$aps_no_referrals, add_options = list(pageLength = 20))
          ),
          bs4Dash::column(6,
                          datatable_default(dq_aps_referrals, add_options = list(pageLength = 20))
          )
        )
      )
    })

    # Static spec for the summary boxes; the actual tables are rebuilt from the
    # selected window on each render.
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

    output$summary <- renderUI({
      dq_summary <- current_summary()

      args <- dq_summary_args |>
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

      rlang::exec(ui_row, title = "System-wide Summary",
             !!!make_columns(args, max_cols = 2, fn = bs4Dash::box),
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
