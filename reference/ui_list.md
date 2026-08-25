# Construct a list from various elements

The \`icon\` is placed before \`text\`. Any additional arguments will be
added after \`text\`.

## Usage

``` r
ui_list(x, ..., l_style = NULL, ordered = FALSE)
```

## Arguments

- x:

  `(data.frame)` with a \*\*Required\*\* \`text\` column and
  \*\*Optional\*\* \`style\` & \`icon\` columns

- ...:

  named elements with which to make a data.frame.

- ordered:

  `(logical)` Whether the list should be ordered \`\<ol\>\`

## Value

`(shiny.tag)`
