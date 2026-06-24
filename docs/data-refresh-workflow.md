# RME Data Pipeline Overview

RME gets data from two sources:

- **HUD CSV export** — Clarity runs an automated nightly script that pushes 
  this to an S3 bucket
- **Custom Looks** — Clarity Looker exports, also updated daily

## Pipeline

HMISdata → HMISprep → HMISserve → S3 (`shiny-data-cohhio`) → RminorElevated

Each step depends on the previous:
- **HMISdata** pulls and stores the raw HUD CSV export
- **HMISprep** cleans and prepares data for calculations
- **HMISserve** generates the report-ready datasets and writes them to S3
- **RminorElevated** downloads from S3 at worker startup via `load_app_data()`

## What gets updated

All S3 datasets in the configured `data_env` folder (set in `golem-config.yml`).
This includes both `.rds` and `.parquet` files. Local datasets (e.g. `Regions`, 
`program_lookup`) are loaded from `HMISdata` directly and are updated when that 
package is updated.

## How often

Daily. The HUD CSV export runs automatically; the Looker exports and downstream 
pipeline steps are also on a daily schedule.

## How refresh time is computed

`get_s3_refresh_date()` checks the last-modified timestamp of the raw HUD CSV 
export bucket — not the final `shiny-data-cohhio` bucket. This is intentional: 
if the pipeline fails partway through, the displayed refresh time will reflect 
the last successful raw export rather than silently showing a stale timestamp 
from the output bucket.

The result is stored once at worker startup in `APP_META$refresh_time` 
(see `global.R`) and displayed in the sidebar. No additional S3 calls are made 
during the session.

## When does new data appear in the app

`load_app_data()` runs once when a Shiny worker process starts. A running worker 
will continue serving the data it loaded at startup until the worker restarts.

New data becomes visible after:
- A redeployment (`rsconnect::deployApp()`)
- A worker idle timeout (shinyapps.io spins up a fresh worker on next visit)
- A manual restart from the shinyapps.io dashboard

There is currently no in-app mechanism to detect or reload newer S3 data 
mid-session. This is by design for now — it keeps startup simple and avoids 
mid-session data inconsistency.