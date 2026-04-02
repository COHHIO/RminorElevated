#' Development Mode Banner
#'
#' Displays a visible banner when the app is running in non-production mode.
#'
#' @return Shiny UI element (div) or NULL
#' @noRd
dev_banner_ui <- function() {
  app_prod <- tryCatch(
    get_golem_config("app_prod"),
    error = function(e) NULL
  )
  
  if (!isTRUE(app_prod)) {
    shiny::tags$div(
      style = "
        background-color: #c0392b;
        color: white;
        padding: 8px;
        text-align: center;
        font-weight: bold;
        z-index: 9999;
      ",
      "⚠️ DEVELOPMENT MODE — USING TEST DATA ⚠️"
    )
  }
}