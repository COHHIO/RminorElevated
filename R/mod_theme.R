#' theme UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_theme_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' theme Server Functions
#'
#' @noRd 
mod_theme_server <- function(id, active){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(active$dark_mode, {
      # Switch to dark color Theme
      if (isTRUE(active$dark_mode)) {
        # Sass changes here, see bslib
        
      }
    })
  })
}
    
## To be copied in the UI
# mod_theme_ui("theme_1")
    
## To be copied in the server
# mod_theme_server("theme_1")

# Handle SCSS -----------------------------------------------------------------
# custom.scss is the source; custom.min.css is a committed build artifact that
# bundle_resources() serves at runtime. Regenerate it with build_css() after
# editing any .scss file, then commit the result. This used to run on every
# worker boot via a top-level do_sass() call, which was pure startup overhead.
build_css <- function() {
  css_dir <- file.path("inst", "app", "www", "css")  # run from project root
  bundle <- sass::sass_bundle(
    sass::sass_layer_file(file.path(css_dir, "custom.scss"))
  )
  sass::sass(
    bundle,
    options = sass::sass_options(output_style = "compressed"),
    output  = file.path(css_dir, "custom.min.css")
  )
  cli::cli_alert_success("Compiled {.path {file.path(css_dir, 'custom.min.css')}}")
  invisible(file.path(css_dir, "custom.min.css"))
}  
