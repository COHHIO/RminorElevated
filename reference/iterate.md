# Iterative generate output functions

Iterative generate output functions

## Usage

``` r
iterate(
  x,
  fn,
  outputId,
  env = rlang::caller_env(),
  output,
  ...,
  rc = shiny::getDefaultReactiveDomain()
)
```

## Arguments

- x:

  `(list)` of items to iterate over

- fn:

  `(fn)` output function to apply

- outputId:

  `(character)` The namespace ID (1,2,3 will be appended for each
  iteration)

- ...:

  Further arguments passed on to \`fn\`

- header_names:

  `(logical)` Whether to create
  [h4](https://rstudio.github.io/htmltools/reference/builder.html)
  headers above each item using the name

- ns:

  `(function)` ns function from the enclosing shiny context

## Value

`(shiny.tag.list)`
