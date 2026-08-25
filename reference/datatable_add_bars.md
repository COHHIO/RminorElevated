# Add [styleColorBar](https://rdrr.io/pkg/DT/man/styleInterval.html) or \`styleDivergentBar\` to datatable

Add [styleColorBar](https://rdrr.io/pkg/DT/man/styleInterval.html) or
\`styleDivergentBar\` to datatable

## Usage

``` r
datatable_add_bars(table, columns, valueColumns, ..., divergent = FALSE)
```

## Arguments

- table:

  a table object created from
  [`datatable()`](https://rdrr.io/pkg/DT/man/datatable.html)

- columns:

  the indices of the columns to be formatted (can be character, numeric,
  logical, or a formula of the form `~ V1 + V2`, which is equivalent to
  `c('V1', 'V2')`)

- valueColumns:

  indices of the columns from which the cell values are obtained; this
  can be different with the `columns` argument, e.g. you may style one
  column based on the values of a different column

- ...:

  Arguments passed on to
  [`DT::formatStyle`](https://rdrr.io/pkg/DT/man/formatCurrency.html),
  [`DT::styleColorBar`](https://rdrr.io/pkg/DT/man/styleInterval.html),
  [`styleDivergentBar`](https://cohhio.github.io/RminorElevated/reference/styleDivergentBar.md)

  `target`

  :   the target to apply the CSS styles to (the current cell or the
      full row)

  `fontWeight`

  :   the font weight, e.g. `'bold'` and `'normal'`

  `color`

  :   the font color, e.g. `'red'` and `'#ee00aa'`

  `backgroundColor`

  :   the background color of table cells

  `background`

  :   the background of table cells

  `data`

  :   a numeric vector whose range will be used for scaling the table
      data from 0-100 before being represented as color bars. A vector
      of length 2 is acceptable here for specifying a range possibly
      wider or narrower than the range of the table data itself.

  `angle`

  :   a number of degrees representing the direction to fill the
      gradient relative to a horizontal line and the gradient line,
      going counter-clockwise. For example, 90 fills right to left and
      -90 fills left to right.

  `color_pos`

  :   The color of the bars for the positive values

  `color_neg`

  :   The color of the bars for the negative values

- divergent:

  `(logical)` Whether to use \`styleDivergentBar\`

## Value

`(datatable)`
