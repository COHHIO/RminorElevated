# QPR UI Function

A shiny Module to generate the QPR UI.

## Usage

``` r
mod_qpr_ui(
  id,
  choices = NULL,
  date_choices = NULL,
  ns = rlang::caller_env()$ns
)
```

## Arguments

- id:

  Internal parameters for shiny.

- choices:

  `(named list)` of arguments to
  [pickerInput](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html).
  \`FALSE\` to omit the choice drop-down selector. - choices must be
  provided for the picker to show options. Defaults will be used for
  omitted arguments unless arguments are explicitly set to \`NULL\`.
  Defaults are as follows: \`list( inputId = ns("region"), label =
  "Select Region(s)", choices = choices\$choices, \# options =
  shinyWidgets::pickerOptions(liveSearch = TRUE), selected = NULL, width
  = "70 )\` Default value for choices is found in \*global.R\* for each
  tabItem namespace.

- date_choices:

  `(logical)` whether to show a date range picker

## See also

Other QPR:
[`mod_qpr_server()`](https://cohhio.github.io/RminorElevated/reference/mod_qpr_server.md)
