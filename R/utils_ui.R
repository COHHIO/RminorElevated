#' @title A pickerInput that provides active projects to select from
#'
#' @param inputId \code{(namespaced character)} A character vector wrapped in `ns` from the parent environment.
#' @inherit shinyWidgets::pickerInput params return
#' @inheritDotParams shinyWidgets::pickerInput

#' @export

ui_picker_project <- function(
  label = "Select Project",
  inputId = rlang::caller_env()$ns("project"),
  choices = projects, 
  options = shinyWidgets::pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains'),
  ...) {
  shinyWidgets::pickerInput(
    label = label,
    inputId = inputId,
    choices = choices,
    options = options,
    ...
  )
}

#' @title The UI Header output
#'
#' @param outputId \code{(namespaced character)} A character vector wrapped in `ns` from the parent environment.
#' @inheritParams bs4Dash::box
#' @return A fluidrow containing a minimizable box with the header
#' @export

ui_header_row <-
  function(outputId = rlang::caller_env()$ns("header"),
           width = 12,
           headerBorder = FALSE) {
    shiny::fluidRow(bs4Dash::box(
      shiny::htmlOutput(outputId),
      width = width,
      headerBorder = headerBorder
    ))
  }

#' @title A date range picker with sensible defaults
#'
#' @inherit shiny::dateRangeInput params return
#' @inheritDotParams shiny::dateRangeInput
#' @export

ui_date_range <- function(
  inputId = rlang::caller_env()$ns("date_range"),
  label = "Date Range",
  start = Sys.Date() - lubridate:::days(7),
  end = Sys.Date(),
  min = rm_dates()$meta_HUDCSV$Export_Start,
  width = 300,
  ...
  ) {
  shiny::dateRangeInput(
    inputId = inputId,
    label = label,
    start = start,
    end = end,
    min = min,
    width = width,
    ...
  )
}

#' @title A default full width row box.
#' @inheritParams bs4Dash::box
#' @return A full-width \link[bs4Dash]{box} nested in a row
#' @export
#'
#' @examples
#' ui_row_box(tags$p("Hi"))
ui_row_box <- function(...,
                       title = NULL,
                       footer = NULL,
                       status = NULL,
                       solidHeader = FALSE,
                       background = NULL,
                       width = 12,
                       height = NULL,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       closable = FALSE,
                       maximizable = FALSE,
                       icon = NULL,
                       gradient = FALSE,
                       boxToolSize = "sm",
                       elevation = NULL,
                       headerBorder = TRUE,
                       label = NULL,
                       dropdownMenu = NULL,
                       sidebar = NULL,
                       id = NULL) {
  
  .dots <- rlang::dots_list(...)
  if (UU::is_legit(.dots)) {
    out <- shiny::fluidRow(class = "ui_row_box", eval(
      rlang::call2(
        bs4Dash::box,
        title = title,
        footer = footer,
        status = status,
        solidHeader = solidHeader,
        background = background,
        width = width,
        height = height,
        collapsible = collapsible,
        collapsed = collapsed,
        closable = closable,
        maximizable = maximizable,
        icon = icon,
        gradient = gradient,
        boxToolSize = "sm",
        elevation = elevation,
        headerBorder = headerBorder,
        label = label,
        dropdownMenu = dropdownMenu,
        sidebar = sidebar,
        id = id,
        !!!.dots
      )
    ))
  } else {
    out <- NULL
  }
  out
}

#' @title A default full width row box.
#' @inheritParams bs4Dash::box
#' @return A \link[bs4Dash]{box} with solid header
#' @export
#'
#' @examples
#' ui_solid_box("Hi")
ui_solid_box <- function(...,
                       title = NULL,
                       footer = NULL,
                       status = NULL,
                       solidHeader = TRUE,
                       background = NULL,
                       width = 12,
                       height = NULL,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       closable = FALSE,
                       maximizable = FALSE,
                       icon = NULL,
                       gradient = FALSE,
                       boxToolSize = "sm",
                       elevation = NULL,
                       headerBorder = TRUE,
                       label = NULL,
                       dropdownMenu = NULL,
                       sidebar = NULL,
                       id = NULL) {
  if (!missing(id))
    id = purrr::when(id, 
                     stringr::str_detect(., "^dq\\_box\\_", negate = TRUE) ~ paste0("dq_box_", .),
                     ~ .)
  .dots <- rlang::dots_list(...)
  if (UU::is_legit(.dots)) {
    out <- shiny::fluidRow(class = "ui_row_box", eval(
      rlang::call2(
        bs4Dash::box,
        title = title,
        footer = footer,
        status = status,
        solidHeader = solidHeader,
        background = background,
        width = width,
        height = height,
        collapsible = collapsible,
        collapsed = collapsed,
        closable = closable,
        maximizable = maximizable,
        icon = icon,
        gradient = gradient,
        boxToolSize = "sm",
        elevation = elevation,
        headerBorder = headerBorder,
        label = label,
        dropdownMenu = dropdownMenu,
        sidebar = sidebar,
        id = id,
        !!!.dots
      )
    ))
  } else {
    out <- NULL
  }
  out
}


fun_arg_maker <- function(fn) {
  rlang::fn_fmls(fn) |> purrr::imap_chr(~paste0(.y, ifelse(!is.null(.x), paste0(" = ", .x), ""),",")) |> cat(sep = "\n")
}

fun_arg_pass <- function(fn) {
  rlang::fn_fmls(fn) |> purrr::imap_chr(~paste0(.y, " = ",.y,",")) |> cat(sep = "\n")
}
