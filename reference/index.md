# Package index

## Running the App

- [`run_app()`](https://cohhio.github.io/RminorElevated/reference/run_app.md)
  : Run the Shiny Application

## Data Access & Loading

Single accessor for app datasets and the helpers behind first-access
loading and decoration.

- [`get_app_data()`](https://cohhio.github.io/RminorElevated/reference/get_app_data.md)
  : Retrieve loaded app data
- [`living_situation()`](https://cohhio.github.io/RminorElevated/reference/living_situation.md)
  : Living Situation Reference Number Translation \`r
  lifecycle::badge("deprecated")\`
- [`iterate()`](https://cohhio.github.io/RminorElevated/reference/iterate.md)
  : Iterative generate output functions

## Reactivity & Error Handling

Helpers that isolate module failures so one error can’t blank the page.

- [`safe_reactive_quoted()`](https://cohhio.github.io/RminorElevated/reference/safe_reactive_quoted.md)
  : safe_reactive_quoted

## DataTable Helpers

- [`datatable_default()`](https://cohhio.github.io/RminorElevated/reference/datatable_default.md)
  : DT Datatable with some helpful defaults

- [`datatable_add_bars()`](https://cohhio.github.io/RminorElevated/reference/datatable_add_bars.md)
  :

  Add [styleColorBar](https://rdrr.io/pkg/DT/man/styleInterval.html) or
  \`styleDivergentBar\` to datatable

- [`datatable_options_update()`](https://cohhio.github.io/RminorElevated/reference/datatable_options_update.md)
  : Update datatable options

- [`styleDivergentBar()`](https://cohhio.github.io/RminorElevated/reference/styleDivergentBar.md)
  : Style DT divergent color bar

- [`qpr_datatable()`](https://cohhio.github.io/RminorElevated/reference/qpr_datatable.md)
  : qpr_datatable

- [`make_columns()`](https://cohhio.github.io/RminorElevated/reference/make_columns.md)
  : Make columns from assorted shiny.tag elements Sorts shiny.tags into
  columns based on the maximum number of columns (\`max_cols\`) per row

## Data Quality Helpers

- [`dq_filter_between()`](https://cohhio.github.io/RminorElevated/reference/dq_filter_between.md)
  : Filter data.frame with default filters for DQ
- [`dq_select_cols()`](https://cohhio.github.io/RminorElevated/reference/dq_select_cols.md)
  : Select default display columns for Data Quality Tables
- [`which_cols()`](https://cohhio.github.io/RminorElevated/reference/which_cols.md)
  : Give DT ready column numbers from names or numbers

## QPR Helpers

- [`qpr_infobox()`](https://cohhio.github.io/RminorElevated/reference/qpr_infobox.md)
  : qpr_infobox

## UI Builders

- [`bs4Alert()`](https://cohhio.github.io/RminorElevated/reference/bs4Alert.md)
  : Create a bootstrap 4 Alert box
- [`server_header()`](https://cohhio.github.io/RminorElevated/reference/server_header.md)
  : Create a default header block
- [`ui_header_row()`](https://cohhio.github.io/RminorElevated/reference/ui_header_row.md)
  : The UI Header output
- [`ui_row()`](https://cohhio.github.io/RminorElevated/reference/ui_row.md)
  : A default full width row box.
- [`ui_list()`](https://cohhio.github.io/RminorElevated/reference/ui_list.md)
  : Construct a list from various elements
- [`ui_icons()`](https://cohhio.github.io/RminorElevated/reference/ui_icons.md)
  : Iterative generation of icons
- [`ui_solid_box()`](https://cohhio.github.io/RminorElevated/reference/ui_solid_box.md)
  : A default full width row box.
- [`ui_date_range()`](https://cohhio.github.io/RminorElevated/reference/ui_date_range.md)
  : A date range picker with sensible defaults
- [`ui_picker_program()`](https://cohhio.github.io/RminorElevated/reference/ui_picker_program.md)
  : A pickerInput that provides active programs to select from
