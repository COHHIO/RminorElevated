#!/usr/bin/env Rscript

# Deploy script for Shiny app with S3 data
library(rsconnect)
library(aws.s3)
library(readr)

# Configuration
S3_BUCKET <- Sys.getenv("S3_BUCKET")
S3_DATA_KEY <- fs::path(folder, file_name)
SHINYAPPS_ACCOUNT <- Sys.getenv("SHINYAPPS_ACCOUNT")
SHINYAPPS_TOKEN <- Sys.getenv("SHINYAPPS_TOKEN")
SHINYAPPS_SECRET <- Sys.getenv("SHINYAPPS_SECRET")
APP_NAME <- Sys.getenv("APP_NAME")

cat("Starting deployment process...\n")

# 1. Download data from S3
cat("Downloading data from S3...\n")
tryCatch({
  # Create data directory
  dir.create("data", showWarnings = FALSE)
  
  # Download the data file
  save_object(
    object = S3_DATA_KEY,
    bucket = S3_BUCKET,
    file = "data/app_data.csv"  # Local filename for your app
  )
  
  cat("Data downloaded successfully\n")
}, error = function(e) {
  cat("Error downloading data:", e$message, "\n")
  quit(status = 1)
})

# 2. Configure rsconnect authentication
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

# 3. Deploy the app
cat("Deploying app to shinyapps.io...\n")
tryCatch({
  deployApp(
    appDir = "/app",
    appName = APP_NAME,
    account = SHINYAPPS_ACCOUNT,
    forceUpdate = TRUE,
    launch.browser = FALSE
  )
  cat("App deployed successfully!\n")
}, error = function(e) {
  cat("Error deploying app:", e$message, "\n")
  quit(status = 1)
})

cat("Deployment process completed successfully!\n")