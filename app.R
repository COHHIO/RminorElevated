# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

Sys.setenv(R_CONFIG_ACTIVE = "production")
message("Active config: ", Sys.getenv("R_CONFIG_ACTIVE"))

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
message("Package loaded successfully")

is_prod <- Sys.getenv("R_CONFIG_ACTIVE") == "production"
options("golem.app.prod" = is_prod)

tryCatch(
  RminorElevated::run_app(),
  error = function(e) {
    message("ERROR: ", e$message)
    rlang::last_trace()
  }
)

