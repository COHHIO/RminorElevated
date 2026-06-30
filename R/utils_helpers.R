#' @title qpr_datatable
#' @description Function to render datatable from default template for QPR tabitems
#' @param .data \code{(any)} Data to be passed in and used subsequent arguments
#' @param .replace \code{(logical)} whether to replace the default arguments with those supplied and eliminate the default arguments, or to replace existing defaults & and add additional args specified
#' @inheritParams DT::datatable
#' @inheritDotParams DT::datatable
#' @param ... \code{(named arguments)} passed on to \link[DT]{datatable}
#' @export

 
qpr_datatable <- function(.data,
                          .replace = FALSE,
                          caption = NULL,
                          rownames = FALSE,
                          filter = 'top',
                          options = list(dom = 'ltpi'),
                          ...
) {
  # Get default args
  .dt_opts <- rlang::fn_fmls()
  # Remove . args
  .dt_opts <- .dt_opts[!grepl("^\\.", names(.dt_opts))]
  # Get user supplied args
  .user <- rlang::call_args(match.call())
  .user <- append(.user[!grepl("^\\.", names(.user))], rlang::dots_list(...))
  
  if (.replace) {
    .dt_opts <- .user
  } else if (!identical(.dt_opts, .user) && !rlang::is_empty(.user)) {
    .dt_opts <- purrr::list_modify(.dt_opts, !!!.user)
  }
  # replace data call with actual data
  .dt_opts$data <- .data
  #evaluate each item so calls are not passed in.
  for (i in which(purrr::map_lgl(.dt_opts, is.call))) {
    .dt_opts[[i]] <- eval(.dt_opts[[i]])
  }
  rlang::exec(DT::datatable, !!!.dt_opts)
}


#' @title qpr_infobox
#' @description Function to render infobox from default template for QPR tabitems
#' @inheritParams qpr_datatable
#' @inheritParams bs4Dash::bs4InfoBox
#' @inheritDotParams bs4Dash::infoBox
#' @param .replace \code{(logical)} whether to replace the default arguments with those supplied and eliminate the default arguments, or to replace existing defaults & and add additional args specified
#' @param ... \code{(named arguments)} passed on to \link[bs4Dash]{infoBox}
#' @export

qpr_infobox <- function(.data,
                        .replace = FALSE,
                        title = "Average Score",
                        color = "purple",
                        value = .data$AvgScore,
                        icon = shiny::icon("shopping-cart"),
                        subtitle = "See table below for detail.",
                        ...
) {
  
  .ib_opts <- rlang::fn_fmls()
  .ib_opts <- .ib_opts[!grepl("^\\.", names(.ib_opts))]
  .user <- rlang::call_args(match.call())
  .user <- append(.user[!grepl("^\\.", names(.user))], rlang::dots_list(..., .named = TRUE))
  if (inherits(.user$icon, "character")) .user$icon <- shiny::icon(.user$icon)
  if (.replace) {
    .ib_opts <- .user
  } else if (!identical(.ib_opts, .user) && !rlang::is_empty(.user)) {
    .ib_opts <- purrr::list_modify(.ib_opts, !!!.user)
  }  
  
  # Evaluate all calls in this environment (purrr::map does not work)
  for (i in which(purrr::map_lgl(.ib_opts, is.call))) {
    .ib_opts[[i]] <- eval(.ib_opts[[i]])
  }
  
  rlang::exec(bs4Dash::bs4InfoBox, !!!.ib_opts)
}

#' @title safe_reactive_quoted
#' @description Function to safely return reactives if it fails
#' @export

safe_reactive_quoted <- function(expr, error_message = "Something went wrong loading this report.") {
  shiny::reactive({
    tryCatch(
      rlang::eval_bare(expr, env = parent.frame()),
      error = function(e) {
        cli::cli_alert_danger("safe_reactive caught: {e$message}")
        shinyalert::shinyalert(title = "Error", text = error_message, type = "error")
        NULL
      }
    )
  })
}

# For reactive()/eventReactive() pipelines
safe_reactive <- function(fn, error_message = "Something went wrong loading this report.") {
  reactive({
    tryCatch(
      fn(),
      error = function(e) {
        cli::cli_alert_danger("safe_reactive caught: {e$message}")
        shinyalert::shinyalert(title = "Error", text = error_message, type = "error")
        NULL
      }
    )
  })
}

# For renderUI()/renderDT() blocks
safe_render <- function(expr, error_message = "Something went wrong loading this report.") {
  tryCatch(
    expr,
    error = function(e) {
      cli::cli_alert_danger("safe_render caught: {e$message}")
      shinyalert::shinyalert(title = "Error", text = error_message, type = "error")
      NULL
    }
  )
}
