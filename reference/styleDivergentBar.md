# Style DT divergent color bar

Style DT divergent color bar

## Usage

``` r
styleDivergentBar(data, color_pos, color_neg)
```

## Arguments

- data:

  The numeric vector whose range will be used for scaling the table data
  from 0-100 before being represented as color bars. A vector of length
  2 is acceptable here for specifying a range possibly wider or narrower
  than the range of the table data itself.

- color_pos:

  The color of the bars for the positive values

- color_neg:

  The color of the bars for the negative values

## Value

This function generates JavaScript and CSS code from the values
specified in R, to be used in DT tables formatting.

## Details

\#' Style DT color bars for values that diverge from 0. From
[federicomarini/GeneTonic](https://github.com/federicomarini/GeneTonic)
This function draws background color bars behind table cells in a
column, width the width of bars being proportional to the column values
\*and\* the color dependent on the sign of the value.

A typical usage is for values such as \`log2FoldChange\` for tables
resulting from differential expression analysis. Still, the
functionality of this can be quickly generalized to other cases - see in
the examples.

The code of this function is heavily inspired from styleColorBar, and
borrows at full hands from an excellent post on StackOverflow -
https://stackoverflow.com/questions/33521828/stylecolorbar-center-and-shift-left-right-dependent-on-sign/33524422#33524422

## Examples

``` r
simplest_df <- data.frame(
  a = c(rep("a", 9)),
  value = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)
)

# or with a very simple data frame
DT::datatable(simplest_df) |>
  DT::formatStyle(
    "value",
    background = styleDivergentBar(
      simplest_df$value,
      scales::alpha("forestgreen", 0.4),
      scales::alpha("gold", 0.4)
    ),
    backgroundSize = "100% 90%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center"
  )

{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9"],["a","a","a","a","a","a","a","a","a"],[-4,-3,-2,-1,0,1,2,3,4]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>a<\/th>\n      <th>value<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"a","targets":1},{"name":"value","targets":2}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background':isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/4 * 50) + '%, #228B2266 ' + (50 + value/4 * 50) + '%,#228B2266  50%,transparent 50%)': 'linear-gradient(90deg, transparent, transparent 50%, #FFD70066 50%, #FFD70066 ' + (50 + value/4 * 50) + '%, transparent ' + (50 + value/4 * 50) + '%)','background-size':'100% 90%','background-repeat':'no-repeat','background-position':'center'});\n}"},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}
```
