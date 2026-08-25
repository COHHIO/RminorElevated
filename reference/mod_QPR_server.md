# QPR Server Functions

A shiny server Module to generate the header, slider, pickers and plot
for each tabitem.

## Usage

``` r
mod_qpr_server(id, header, ...)
```

## Arguments

- id, input, output, session:

  Internal parameters for shiny.

- header:

  `(character)` The header text passed to the initial
  [h2](https://rstudio.github.io/htmltools/reference/builder.html) tag
  in the header.

- ...:

  Additional `(list/shiny.tag.list/shiny.tag)`s to be appended to the
  header after the
  [h2](https://rstudio.github.io/htmltools/reference/builder.html) tag
  with \`header\`. Defaults to
  `list(h4(input$region), h4(paste(ReportStart, "to", ReportEnd)))` if
  unspecified.

## See also

Other QPR:
[`mod_qpr_ui()`](https://cohhio.github.io/RminorElevated/reference/mod_qpr_ui.md)
