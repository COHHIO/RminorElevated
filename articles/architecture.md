# Architecture Overview

This article describes how RminorElevated (RME) is structured as of the
1.0.0 release. It is the stable, current-state companion to the dated
planning note in `docs/architecture-review-2026-06.md`, which captured
the work while it was in flight.

## What RME is

RME is a [golem](https://thinkr-open.github.io/golem/)-based Shiny app —
which means the app is an R package — with a `bs4Dash` UI, deployed to
shinyapps.io. It presents HMIS custom reporting (Data Quality, QPR,
prioritization, competition, and utilization views) for the COHHIO
ecosystem, reading its data from packages like `HMISdata`, `HMISprep`,
and `clarity.looker`.

The deployment model matters for everything below: each worker process
loads a large set of HMIS datasets from S3 into memory **once per R
process**, then serves **many concurrent user sessions** out of that one
process. Anything loaded at startup is paid for once but held for the
life of the process; anything scoped incorrectly to the global
environment leaks across unrelated user sessions.

## Data loading and access

### One accessor, one environment

All app datasets live in a single internal environment, `.app_data_env`,
and are reached through one exported accessor,
[`get_app_data()`](https://cohhio.github.io/RminorElevated/reference/get_app_data.md).
This replaced an earlier pattern (`create_data_accessors()`) that
generated a separate zero-argument function per dataset and assigned
them into the global environment.

``` r

# Get one dataset
enrollment <- get_app_data("enrollment")

# List everything currently available (does not trigger lazy loaders)
names(get_app_data())
```

The loading path is:

1.  `load_app_data()` (in `R/app_data.R`) reads the datasets from S3 and
    returns them as a named list. This logic used to live in `global.R`.
2.  `set_app_data()` stores that list as `APP_DATA` inside
    `.app_data_env`. It is called once at boot from `global.R`.
3.  `get_app_data(name)` retrieves a dataset by name. With no argument
    it returns the whole list.

Because access goes through one function against one environment, call
sites are uniform and the storage mechanism can change without touching
them.

### Eager vs. first-access loading

Not every dataset is loaded at startup. Large datasets that aren’t
needed on every session — for example `co_clients_served` and
`enrollment_small` — are registered as **deferred loaders** rather than
loaded eagerly. On the first `get_app_data("<name>")` call for a
deferred dataset,
[`get_app_data()`](https://cohhio.github.io/RminorElevated/reference/get_app_data.md)
runs its loader, decorates the result, stores it back into `APP_DATA`,
and returns it. Subsequent calls hit the stored copy.

``` r

# First call runs the loader and caches the result;
# later calls in the same process are cheap.
co_clients <- get_app_data("co_clients_served")
```

Dataset “decoration” — such as adding Clarity links via
`add_clarity_links_df()` — is likewise done lazily on first access
rather than for every dataset at startup.

> TODO: keep the authoritative list of eager vs. deferred datasets in
> sync with `DEFERRED_LOADERS` in `R/app_data.R`.

## Per-session state

Shiny state that belongs to a user session is scoped to that session,
not to the global environment. An earlier defect stored session state
(an `active` `reactiveValues`) in a global variable, which bled across
sessions and was the most likely cause of the intermittent “clicking a
sidebar tab doesn’t navigate until refresh” reports. Session-scoped
inputs now live in the server function’s environment.

## Error isolation

A single failing module should not blank the whole page. Rendering and
reactive expressions in the heavier modules are wrapped in the
`safe_render()` / `safe_reactive()` /
[`safe_reactive_quoted()`](https://cohhio.github.io/RminorElevated/reference/safe_reactive_quoted.md)
helpers, which catch errors and degrade gracefully instead of taking
down the session.

## Tables

Large `DT` tables render with `server = TRUE` so paging, sorting, and
searching happen server-side rather than shipping the entire table to
the browser. Inputs feeding expensive tables are debounced.

## Where things live

| Concern | Location |
|----|----|
| Data loading from S3 | `R/app_data.R` (`load_app_data()`) |
| Data access | `R/app_data.R` ([`get_app_data()`](https://cohhio.github.io/RminorElevated/reference/get_app_data.md), `.app_data_env`) |
| Dataset decoration | `R/decorate_data.R` |
| App entry point | `R/run_app.R` |
| UI / server assembly | `R/app_ui.R`, `R/app_server.R` |
| Report modules | `R/mod_body_*.R` |
| QPR logic | `R/qpr_expr_*.R` |
| Data refresh workflow | `docs/data-refresh-workflow.md` |
