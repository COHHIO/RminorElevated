# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Deploy script writes .config_active with the target's profile.
# An explicit env var (e.g. local dev runs) takes precedence.
if (Sys.getenv("GOLEM_CONFIG_ACTIVE") == "" && file.exists(".config_active")) {
  Sys.setenv(GOLEM_CONFIG_ACTIVE = readLines(".config_active", n = 1))
}
message("Active config: ", Sys.getenv("GOLEM_CONFIG_ACTIVE",
                                      unset = Sys.getenv("R_CONFIG_ACTIVE", unset = "default")))

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

