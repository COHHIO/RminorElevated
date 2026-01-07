#!/usr/bin/env Rscript
# Deploy script for Shiny app
# Disable renv for the deployment script since we installed packages globally
Sys.setenv(RENV_ACTIVATE = "FALSE")

# Verify library paths (should show global paths)
cat("R library paths:\n")
cat(paste(.libPaths(), collapse = "\n"))
cat("\n")

# Deploy script for Shiny app with S3 data
library(rsconnect)

# Configuration
SHINYAPPS_ACCOUNT <- Sys.getenv("SHINYAPPS_ACCOUNT")
SHINYAPPS_TOKEN <- Sys.getenv("SHINYAPPS_TOKEN")
SHINYAPPS_SECRET <- Sys.getenv("SHINYAPPS_SECRET")
APP_NAME <- "Rminor_elevated"

cat("Starting deployment process...\n")

# 1. Configure rsconnect authentication
cat("Configuring rsconnect authentication...\n")
tryCatch({
  setAccountInfo(
    name = SHINYAPPS_ACCOUNT,
    token = SHINYAPPS_TOKEN,
    secret = SHINYAPPS_SECRET
  )
  cat("Authentication configured\n")
}, error = function(e) {
  cat("Error configuring authentication:", e$message, "\n")
  quit(status = 1)
})

# Enable more verbose output and warnings
options(warn = 1)

# 2. Deploy the app
cat("Deploying app to shinyapps.io...\n")
tryCatch({
  deployApp(
    appDir = ".",
    appName = APP_NAME,
    account = SHINYAPPS_ACCOUNT,
    forceUpdate = TRUE,
    launch.browser = FALSE,
    logLevel = "verbose"
  )
  cat("App deployed successfully!\n")

  # Print the app URL instead of trying to open it
  app_url <- paste0("https://", SHINYAPPS_ACCOUNT, ".shinyapps.io/", APP_NAME, "/")
  cat("App is available at:", app_url, "\n")
  
}, error = function(e) {
  cat("Error deploying app:", e$message, "\n")
  cat("Warnings:\n")
  print(warnings())
  quit(status = 1)
})

cat("Deployment process completed successfully!\n")