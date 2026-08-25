# Make columns from assorted shiny.tag elements Sorts shiny.tags into columns based on the maximum number of columns (\`max_cols\`) per row

Make columns from assorted shiny.tag elements Sorts shiny.tags into
columns based on the maximum number of columns (\`max_cols\`) per row

## Usage

``` r
make_columns(x, max_cols = TRUE, fn = list(bs4Dash::box, bs4Dash::column)[[1]])
```

## Arguments

- x:

  `(shiny.tags)`

- max_cols:

  `(logical/integer)` Either \`TRUE\` \*\*Default\*\* for a default of 4
  columns per row, \`FALSE\` for no columns, or an integer indicating
  the max number of columns.

## Value

`(list(s))`
