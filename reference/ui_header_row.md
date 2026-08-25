# The UI Header output

The UI Header output

## Usage

``` r
ui_header_row(
  outputId = rlang::caller_env()$ns("header"),
  width = 12,
  headerBorder = FALSE
)
```

## Arguments

- outputId:

  `(namespaced character)` A character vector wrapped in \`ns\` from the
  parent environment.

- width:

  The width of the box, using the Bootstrap grid system. This is used
  for row-based layouts. The overall width of a region is 12, so the
  default card width of 6 occupies 1/2 of that width. For column-based
  layouts, use `NULL` for the width; the width is set by the column that
  contains the box.

- headerBorder:

  Whether to display a border between the header and body. TRUE by
  default

## Value

A fluidrow containing a minimizable box with the header
