# Outline: Speed, Stability, and Navigation Fixes for RminorElevated

## Status

| Finding | Status |
|----------|----------|
| Cross-session active bug | Fixed (#52/#53) |
| global.R S3-loading refactor | Done (#55/#59) |
| Single `get_app_data()` accessor | Proposed below |
| Duplicate S3 refresh call + refresh-workflow doc | Proposed below |
| Runtime error handling | Filed (#56) |
| DT server-side rendering | Filed (#57) |
| COHHIO branding | Filed (#58) |

## Context

Users have reported three problems with the RME Shiny app: it's slow, it crashes, and clicking a sidebar tab sometimes doesn't navigate there until the page is refreshed. The app is a golem-based Shiny app (`bs4Dash` UI) deployed to shinyapps.io that loads a large set of HMIS datasets from S3 into memory once per R process, then serves many concurrent user sessions out of that one process per worker.

Direct code reading (not just static guesses) turned up one specific, verified defect that plausibly explains *both* the navigation bug and part of the slowness/crash reports: a piece of session state is being stored in a single shared global variable instead of being scoped per-session. The rest of the findings are the usual Shiny performance suspects (eager startup work, client-side datatables, no caching/debouncing) which compound the problem under load. This outline groups the fixes into priority tiers so the team can decide what to tackle first — it is a planning document only, no code changes are included.

## Priority 0 — Cross-session state bug (root cause of the tab-navigation issue) — ✅ ALREADY FIXED

**Update:** this was independently found and fixed before this plan was written. See [Issue #52](https://github.com/COHHIO/RminorElevated/issues/52) and [PR #53](https://github.com/COHHIO/RminorElevated/pull/53) (merged 2026-06-23) — the fix matches what's described below almost exactly. It's on `origin/main` but one commit ahead of the local checkout used during this investigation (run `git pull` to sync). Left in place below for context on *why* it mattered, since it's the most likely explanation for the original navigation complaint.

**Finding:** [app_server.R:10](R/app_server.R#L10) does:
```r
active <<- reactiveValues()
```
`<<-` walks up the *lexical* scope chain, not the call stack. Since `active` isn't defined in any enclosing environment, this creates a single variable in the package/global environment — shared by **every concurrent user session**, not one per session as `reactiveValues()` is meant to be used. [mod_body.R:30-46](R/mod_body.R#L30-L46) and [mod_theme.R:24](R/mod_theme.R#L24) both read/write this same global `active` by name (they're separate top-level functions, so they can't see a session-local variable any other way — this is almost certainly *why* `<<-` was used as a shortcut).

**Why this matches the symptoms:**
- Multiple users hitting the same worker process all read and write the same `active$tab` / `active$ui` / `active$server` values. One user's sidebar click can be silently overwritten by another user's click landing in between the `observe()` write and the `renderUI` read in [mod_body.R](R/mod_body.R#L29-L49) — producing exactly the "click doesn't navigate, but a refresh fixes it" behavior, and only *intermittently* (only when sessions collide), which matches "not always."
- Every navigation event invalidates the shared `active` object, which invalidates the `renderUI` in [mod_body.R:29](R/mod_body.R#L29) for **every connected session**, not just the one that clicked. Under concurrent load this multiplies server-side render work by the number of connected sessions for every single tab click anywhere in the app — a hidden contributor to "slow" and to crashes under load.

**Fix:** Make `active` session-local and pass it explicitly to the modules that need it, instead of relying on `<<-`:
```r
app_server <- function(input, output, session) {
  active <- reactiveValues()
  observe({ ... })
  mod_navbar_server("navbar")
  mod_sidebar_server("sidebar")
  mod_body_server("body", active)
  mod_theme_server("color_theme", active)
}
```
Update `mod_body_server(id, active)` and `mod_theme_server(id, active)` signatures in [mod_body.R](R/mod_body.R) and [mod_theme.R](R/mod_theme.R) to receive `active` as a parameter and reference the parameter instead of the global.

This is a small, surgical, low-risk change confined to 2-3 files. It should be done first and validated on its own before touching anything else, since it's the most likely fix for the navigation complaint.

**Verification:** Run the app locally and open it in two separate browser sessions (or one normal + one incognito window) at the same time. Click through different tabs in both simultaneously and confirm each session's content always matches its own last click, with no refresh ever required, and that one session's clicks never change what the other session is showing.

## Priority 1 — Startup cost and redundant S3 calls

- [global.R:56-114](R/global.R#L56-L114) synchronously downloads **every** S3 dataset and runs the `qpr_*`/`validation()` derived computations before any session can be served. This runs fresh every time a worker process starts (shinyapps.io spins up new workers on autoscale or after idle recycling), so a burst of new users can mean a burst of slow/cold-starting workers right when load is highest.
- [mod_sidebar.R:12](R/mod_sidebar.R#L12) calls `get_s3_refresh_date()` again, live, **on every single page load/session/refresh** — duplicating work already done once in [global.R:110](R/global.R#L110) (`DATA_REFRESH_TIME`). This adds an avoidable AWS API round-trip to every page load, including the refreshes users are doing to work around the navigation bug. Fix: have the sidebar reuse the already-computed `DATA_REFRESH_TIME` instead of re-querying S3.
- No local caching of the downloaded S3 files between process restarts ([fct_s3.R](R/fct_s3.R) `load_s3_file()`) — every cold start re-downloads everything. Worth evaluating a short-TTL local cache (e.g., write the parquet/rds to a persistent temp/cache dir and skip re-download if fresh) to cut cold-start time.
- Longer-term: evaluate whether every report's dataset truly needs to be loaded eagerly at startup, or whether the least-used reports (e.g. Mahoning-specific reports, program lookup) could load lazily on first tab visit, reducing both cold-start time and steady-state memory footprint.

## Priority 2 — Crash hardening

- No `tryCatch` around the expensive per-report data pipelines (e.g. `qpr_expr_*.R` files invoked from [mod_qpr.R:122](R/mod_qpr.R#L122), the DQ filtering in [mod_body_dq_program_level.R](R/mod_body_dq_program_level.R)). An uncaught error in any reactive (a bad date, an empty filter result, an S3 hiccup) kills the whole session for that user — this is most likely what "crashing" refers to from a user's perspective. Wrap these in `tryCatch()` and surface a friendly message (the app already depends on `shinyalert`) instead of letting the session die.
- Memory: all S3 datasets are held in memory for the life of the worker process and shared (read-only) across every session it serves. This is a reasonable pattern in principle, but combined with Priority 1's full-eager-load and an under-sized shinyapps.io instance, it's a plausible OOM crash path that kills the worker for *every* concurrently-connected user at once. Action item (operational, not code): check the actual configured shinyapps.io instance size/RAM and number of processes against a rough size estimate of `APP_DATA` (e.g. `lobstr::obj_size(APP_DATA)` once locally), and size up if it's close to the limit.

## Priority 3 — Responsiveness (the "slow" complaints)

- All `DT::renderDT(...)` calls across the report modules explicitly pass `server = FALSE` (15 occurrences, e.g. [mod_qpr.R:138,141,146,149](R/mod_qpr.R#L138)), which ships the entire table to the browser as JSON on every render instead of paging server-side (DT's own default is `server = TRUE`). For HMIS-sized tables this bloats payload and can make the browser tab itself sluggish/unresponsive, which reads as "the app is slow" or even "crashed" to a user. Switch these to `server = TRUE`; verify Excel/CSV export buttons (used recently per [NEWS.md](NEWS.md)) still work correctly server-side.
- No use of `debounce()`/`throttle()` on date-range/picker inputs in most report modules, so dragging a date slider or typing re-triggers the full data pipeline on every intermediate value. [mod_body_dq_program_level.R:92-106](R/mod_body_dq_program_level.R#L92-L106) already does this (`|> debounce(1500)`) — extend that same pattern to the QPR modules ([mod_qpr.R](R/mod_qpr.R)) and other DQ modules that don't have it yet.
- No `bindCache()`/memoization anywhere in the codebase. The same filtered dataset is sometimes recomputed multiple times per render (e.g. [mod_body_coc_competition_mahoning.R:89-192](R/mod_body_coc_competition_mahoning.R#L89-L192) calls `pe_summary_final_filter()` and re-derives 4 separate `pivot_longer()` pipelines independently instead of computing once and reusing). Consolidate repeated derivations into a single reactive per output and consider `bindCache()` for the heaviest shared computations.

## Priority 4 — Minor polish (not urgent)

- [inst/js/new_tab_badges.js](inst/js/new_tab_badges.js) polls for ~23 DOM elements every 100ms indefinitely if any are never found — cosmetic only (it just colors sidebar icons green), doesn't affect navigation or crashes, but is an easy cleanup (add a max retry count) whenever someone is already in that file.
- Once Priorities 0-3 are in place and re-measured, consider `future`/`promises` (already a transitive dependency, currently unused) for the heaviest fully-independent report computations so they don't block the main R process for other sessions. This is a bigger architectural lift and only worth it if profiling after the above fixes still shows a bottleneck.

## Suggested sequencing

1. ~~Priority 0~~ — done (#52/#53). `git pull` locally, then validate with the multi-session manual test above on the deployed app to confirm the navigation complaints stop.
2. Priority 1's sidebar S3 call dedup (one-line-ish fix) bundled with Priority 2's tryCatch hardening on the most-visited report modules.
3. Priority 3's DT `server = TRUE` switch + extending debounce, done as a mechanical pass across the report modules.
4. Revisit startup/data-loading architecture (rest of Priority 1) and Priority 4 once the above are deployed and you can see whether crash/slowness reports actually drop.

## Comparison with GitHub Roadmap (#54) and Issue #55

[Roadmap #54](https://github.com/COHHIO/RminorElevated/issues/54) and [Issue #55](https://github.com/COHHIO/RminorElevated/issues/55) map closely onto **Priority 1** above. Issue #55's `load_app_data()`/`get_app_data()` proposal is a well-scoped, concrete version of the startup/data-loading findings here, and Roadmap Phase 2 (local S3 cache, lazy-load infrequent reports, measure startup time) matches the rest of Priority 1. This part of the roadmap is solid as-is.

**Priority 0 is already fixed — just not reflected in the roadmap, and not yet pulled into this local checkout.** [Issue #52](https://github.com/COHHIO/RminorElevated/issues/52) is the exact `active <<-` cross-session bug described above, and it was closed by [PR #53](https://github.com/COHHIO/RminorElevated/pull/53) (merged 2026-06-23), which makes the identical change this plan independently arrived at: `active <<-` → `active <-` in `app_server.R`, with `active` passed explicitly into `mod_body_server`/`mod_theme_server`. That merge commit (`9f92df0`) is on `origin/main` but one commit ahead of the local `main` checked out here (`11fce27`) — `git diff HEAD..origin/main` confirms it's the *only* difference, so nothing else in this plan is stale. **Action: `git pull` locally to pick it up.** No new issue needed for this — dropped from the drafted-issues list below.

**Remaining gap — Priority 2 (crash hardening) and Priority 3 (DT server-side rendering, debounce, bindCache) aren't represented in #54/#55.** The roadmap's "Improve reliability of deployments" goal and Phase 3 smoke test cover *deployment* reliability, but not runtime reliability (uncaught reactive errors killing a session) or client-side datatable bloat — both plausible contributors to remaining "slow"/"crashing" reports now that #52 is fixed. These are independent of the `global.R` refactor and mechanical enough that someone could pick them up in parallel rather than waiting on Phase 1-2 to land. Still worth their own issue(s) — drafted below.

**On bumping to 1.0 after this roadmap:** the phase structure (data refactor → perf → tests → docs/release) is sound for *architecture* maturity, and using 1.0 to mean "stable architecture" for an app already serving real users in production is a reasonable convention. With #52/#53 already shipped, the main remaining reliability gap before calling it 1.0 is the crash-hardening work (Priority 2) — recommend treating that as release-blocking alongside the phases already on the board, since it's a user-facing reliability bug rather than architecture debt. It's worth watching for a few days post-#53 to confirm the navigation complaints actually stop before assuming the rest of the roadmap's data-loading work is what's left to do.

## Phase 1 follow-up (2026-06-24, after #55/#59 merged)

`#55` closed with a narrower scope than its original draft — it covers moving S3/local loading out of `global.R` into `load_app_data()` ([app_data.R](R/app_data.R)) and `add_clarity_links()` ([decorate_data.R](R/decorate_data.R)), but dropped the "single accessor" / "remove direct reliance on global `APP_DATA`" items before closing. Checked the current code on `origin/main` against Phase 1 of the roadmap (#54):

| Phase 1 item | Status |
|---|---|
| Move S3 loading logic out of `global.R` | ✅ Done |
| Replace eager startup loading with cached/lazy loading where appropriate | ❌ Not done — substantively duplicates Phase 2 ("Add local S3 download cache," "Lazy load infrequently used reports," "Move expensive derived calculations out of app startup," which covers the `qpr_tab_choices`/`programs`/`regions` block still sitting in `global.R`). Recommend dropping this checkbox from Phase 1 and letting Phase 2 own it, rather than filing a third overlapping issue. |
| Create single app data accessor (`get_app_data()`) | ❌ Not done — `global.R` still calls `create_data_accessors()`, assigning one global function per dataset name (`validation()`, `Regions()`, etc.) into `.GlobalEnv`. Report modules call these by name directly throughout the app. |
| Remove duplicate S3 refresh calls | ❌ Not done — [mod_sidebar.R:12](R/mod_sidebar.R#L12) still calls `get_s3_refresh_date()` live on every page load, duplicating `APP_META$refresh_time` computed once in [global.R:31](R/global.R#L31). |
| Document data refresh workflow | ❌ Not done |

Two new issues proposed to close out Phase 1 (not yet created on GitHub):

---

### Issue: Replace per-dataset global accessors with a single `get_app_data()`

**Summary**
`global.R` still creates one global function per dataset name via `create_data_accessors()` (e.g. `validation()`, `Regions()`, `co_clients_served()`), assigned into `.GlobalEnv`. This was the original intent of #55 ("Create a single accessor (`get_app_data()`) for loaded data" / "Remove direct reliance on global `APP_DATA`"), but that part was descoped before #55 closed. Report modules across the app call these per-name global functions directly, making the data dependency graph implicit and hard to trace, mock, or test.

**Current behavior**
`global.R` calls `create_data_accessors(APP_DATA)`, which iterates over `APP_DATA` and `assign()`s a zero-arg function per dataset name into the global environment. Report modules call these by name directly (e.g. `qpr_income()`, `Regions()`, `validation()`).

**Goal**
Introduce a single `get_app_data(name)` accessor backed by the already-centralized `APP_DATA` (from `load_app_data()` in `app_data.R`), and migrate modules to use it, eventually removing `create_data_accessors()`/the per-name globals.

**Proposed approach**
- [ ] Add `get_app_data(name)` (and/or `get_app_data()` with no args returning the full list) backed by `APP_DATA`
- [ ] Keep `create_data_accessors()` in place initially so nothing breaks during migration
- [ ] Migrate report modules to `get_app_data("x")` incrementally (file-by-file / PR-by-PR)
- [ ] Once all call sites are migrated, remove `create_data_accessors()` and the per-name global functions
- [ ] Update any docs referencing the old accessor functions

**Acceptance criteria**
- [ ] `get_app_data()` exists and is the documented way to retrieve loaded datasets
- [ ] No remaining direct calls to the per-dataset global functions
- [ ] Existing reports produce the same results
- [ ] App deployment continues to work

**Notes**
Larger and more invasive than #55 since it touches call sites across most report modules — migrate incrementally rather than in one PR. Completes the part of #55's original scope that got dropped.

---

### Issue: Remove duplicate S3 refresh call; document the data refresh workflow

**Summary**
Two small remaining Phase 1 items that #55 didn't cover: a leftover duplicate live S3 API call, and missing documentation of how/when app data refreshes.

**Current behavior**
- [mod_sidebar.R:12](R/mod_sidebar.R#L12) calls `get_s3_refresh_date()` live, on every single page load/session, hitting AWS S3 again even though [global.R:31](R/global.R#L31) already computed `APP_META$refresh_time` once at worker startup.
- There's no documentation describing where S3 data comes from, what triggers a refresh upstream (e.g. the HUD CSV automator mentioned in `NEWS.md`), or how a running shinyapps.io worker picks up newly refreshed data (only on its next cold start, since `load_app_data()` only runs once at process startup).

**Goal**
Remove the redundant AWS call, and write down the data refresh workflow so it's not tribal knowledge.

**Proposed approach**
- [ ] Update `mod_sidebar.R` to read `APP_META$refresh_time` instead of calling `get_s3_refresh_date()` again
- [ ] Write a short doc (e.g. `docs/data-refresh-workflow.md`) covering: what populates the S3 buckets, how often, how `APP_META$refresh_time` is computed, and how/when a running worker picks up newly refreshed data (cold start only)

**Acceptance criteria**
- [ ] Sidebar's displayed refresh date matches `APP_META$refresh_time` with no additional S3 API call
- [ ] Data refresh workflow doc exists and is linked from README or architecture docs

**Notes**
Both small, low-risk, and independent of the `get_app_data()` accessor work above — can ship first or in parallel.

## Branding: COHHIO colors, and why not bslib

Separately from the perf/stability work, there's a want to apply COHHIO's brand colors throughout the app, possibly via a `brand.yaml`/`_brand.yml` file. Researched whether that means switching the UI framework to `{bslib}`:

- Confirmed via bslib's own docs and the bs4Dash maintainer's comments on [RinteRface/bs4Dash#362](https://github.com/RinteRface/bs4Dash/issues/362): **bslib's `brand.yml` auto-theming only applies to bslib-native page functions** (`page_sidebar()`, `page_navbar()`) on Bootstrap 5. It does **not** theme bs4Dash. bs4Dash is built on Bootstrap 4 + AdminLTE3, and AdminLTE still has no stable Bootstrap 5 release — the bs4Dash maintainer's own answer to "will you support bslib/BS5" is "build with `{bslib}` from scratch instead," not "retrofit bs4Dash."
- So "switch to bslib" would mean replacing the dashboard framework, not just adding a theme. Checked the blast radius in this repo: `bs4Dash::` is called directly in 13 files ([mod_sidebar.R](R/mod_sidebar.R), [mod_navbar.R](R/mod_navbar.R), [mod_body.R](R/mod_body.R), [app_ui.R](R/app_ui.R), [mod_qpr.R](R/mod_qpr.R), [mod_welcome.R](R/mod_welcome.R), [mod_body_news.R](R/mod_body_news.R), [mod_body_utilization.R](R/mod_body_utilization.R), [mod_body_vet_active_list.R](R/mod_body_vet_active_list.R), two DQ modules, [utils_helpers.R](R/utils_helpers.R), [utils_ui.R](R/utils_ui.R)), with ~27 sidebar-menu-item calls, 10 `box()` calls, and a dozen-plus infoBox/Accordion/Alert calls. Some of this is centralized behind `ui_row()`/`ui_header_row()`/`ui_solid_box()` in `utils_ui.R` (those wrap `bs4Dash::box`, so updating a few functions there covers part of it), but the dashboard shell and several report modules call bs4Dash directly and would need hand rewriting to bslib's `sidebar()`/`card()`/`value_box()`/nav idioms. Meaningful regression risk for a production app, for what is fundamentally a colors request. (bslib itself is mature and actively maintained as of 2026 — this isn't a maturity concern, it's a "different framework" concern.)
- **Decision: ship COHHIO colors now via the existing SCSS variable mechanism** (`_bootstrap-variables.scss`/`_colors.scss`, compiled by `do_sass()` in `mod_theme.R`) — no framework change, low risk. A possible future bs4Dash → bslib migration is a separate, much larger initiative to consider on its own merits later, independent of branding and not part of the 1.0 milestone. Drafted as its own issue below, not assigned to the 1.0 milestone.

## Drafted issues (pending approval — none created on GitHub yet)

Three new issues, written in the same style as #55. Two fill gaps in the #54/#55 roadmap; one covers the branding work above (the original third gap — the `active <<-` bug — turned out to already be fixed via #52/#53, see above, so it's dropped from this list). On approval, create the first two with `gh issue create --repo COHHIO/RminorElevated --milestone "1.0.0 Production architecture cleanup"`, and the branding one with no milestone, using the bodies below.

---

### Issue: Add error handling around report reactive pipelines to prevent session crashes

**Summary**
Reactive expressions across report modules (QPR `qpr_expr_*.R` via `mod_qpr.R`, DQ filtering in `mod_body_dq_*.R`, etc.) have no `tryCatch` around their data transformations. An uncaught error in any reactive/render (bad date input, empty filter result, transient S3/network hiccup) kills the entire Shiny session for that user with no recovery — visible to the user as the app "crashing."

**Goal**
Uncaught errors in report-rendering reactives should show a friendly in-app message (the app already depends on `shinyalert`) instead of terminating the session.

**Proposed approach**
- [ ] Identify the highest-traffic report modules' reactive pipelines (QPR, DQ program-level, DQ region-level, CoC Competition)
- [ ] Wrap expensive/error-prone reactive expressions in `tryCatch()`, surfacing failures via `shinyalert` or a friendly inline message rather than letting the session die
- [ ] Add a small reusable helper (e.g. `safe_reactive()`) so the pattern is consistent across modules rather than ad hoc
- [ ] Spot-check shinyapps.io instance size vs. in-memory size of `APP_DATA` (e.g. `lobstr::obj_size(APP_DATA)`) to rule out OOM as a separate crash cause

**Acceptance criteria**
- [ ] Forcing an error in a report's data pipeline (e.g. temporarily feeding a bad date range) shows a friendly message instead of disconnecting the session
- [ ] No change to report output for the non-error path

**Notes**
Complements #55 — that issue addresses startup-time data loading; this addresses runtime errors once a session is already running. Independent of the `global.R` refactor, can be done in parallel.

---

### Issue: Switch report DataTables to server-side processing; add debounce/bindCache to reduce redundant recomputation

**Summary**
Every `DT::renderDT()`/`DT::renderDataTable()` call across the report modules explicitly passes `server = FALSE` (15 occurrences, e.g. `mod_qpr.R`, `mod_body_dq_program_level.R`, `mod_body_coc_competition_mahoning.R`), shipping the entire table to the browser as JSON on every render instead of letting DT page server-side (DT's own default is `server = TRUE`). For HMIS-sized tables this bloats payload and can make the browser tab itself sluggish or unresponsive. Separately, most report modules don't debounce date-range/picker inputs (only `mod_body_dq_program_level.R` does), so dragging a slider re-triggers the full data pipeline on every intermediate value, and there's no caching (`bindCache()`) anywhere, so the same filtered dataset can be recomputed multiple times per render (e.g. `mod_body_coc_competition_mahoning.R` re-derives `pe_summary_final_filter()` independently for 4 separate pivot operations).

**Goal**
Reduce perceived "slowness" by cutting payload size and redundant computation.

**Proposed approach**
- [ ] Switch `DT::renderDT(..., server = FALSE)` to `server = TRUE` across report modules; verify Excel/CSV export buttons still work correctly
- [ ] Extend the existing `debounce()` pattern from `mod_body_dq_program_level.R` to `mod_qpr.R` and other DQ modules
- [ ] Consolidate repeated derivations (e.g. `pe_summary_final_filter()` in `mod_body_coc_competition_mahoning.R`) into a single reactive reused by all dependent outputs, adding `bindCache()` where appropriate

**Acceptance criteria**
- [ ] Report tables still sort/filter/page/export correctly after switching to server-side mode
- [ ] Dragging a date-range input no longer triggers a visible recompute on every intermediate value
- [ ] No behavior change to report output, only reduced redundant computation

**Notes**
Orthogonal to the `global.R` startup refactor (#55) — purely runtime responsiveness, mechanical enough to parallelize across contributors.

---

### Issue: Apply COHHIO brand colors via existing SCSS theming

**Summary**
The app's Bootstrap/AdminLTE color variables in `_bootstrap-variables.scss` and `_colors.scss` are still plain Bootstrap defaults (`$blue`, `$gray-600`, `$green`, `$cyan`, `$yellow`, `$red`) rather than COHHIO's brand palette. These variables drive `$primary`/`$secondary`/`$success`/`$info`/`$warning`/`$danger`, which bs4Dash uses throughout (sidebar, navbar, boxes, infoBoxes, alerts) via `status =` props. They're compiled by `do_sass()` in `mod_theme.R` into `custom.min.css`.

**Goal**
Apply COHHIO's brand colors (and optionally typography) consistently across the app using the existing SCSS variable mechanism, without changing the underlying UI framework.

**Proposed approach**
- [ ] Collect COHHIO's brand color palette (primary, secondary, accent colors) and font choices
- [ ] Document the palette in one place as the source of truth (e.g. a `_brand.yml`-style YAML file or a comment block) so colors don't drift back to Bootstrap defaults
- [ ] Update `$primary`/`$secondary`/`$success`/`$info`/`$warning`/`$danger`/etc. in `_bootstrap-variables.scss`/`_colors.scss` to match the brand palette
- [ ] Update `$font-family-base`/`$font-family-heading` if brand typography differs from the current Roboto
- [ ] Re-run `do_sass()` (`mod_theme.R`) to regenerate `custom.min.css`
- [ ] Visually verify sidebar, navbar, boxes, infoBoxes, alerts, and buttons across a sample of report tabs

**Acceptance criteria**
- [ ] App's primary/secondary/status colors visually match COHHIO's brand guidelines
- [ ] No layout regressions introduced by the color change
- [ ] Brand palette is documented in the repo for future reference

**Notes**
Considered switching the UI framework to `bslib` to use `brand.yml`'s automatic theming, but ruled it out for this issue — see "Branding: COHHIO colors, and why not bslib" above. bslib's brand.yml integration only auto-applies to bslib-native page functions on Bootstrap 5, and does not theme bs4Dash. Migrating off bs4Dash would mean rewriting the dashboard shell and the ~13 files that call `bs4Dash::` directly, which is out of scope here. Tracked as a possible separate future initiative if there's appetite independent of branding — not part of this issue or the 1.0 milestone.
