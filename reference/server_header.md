# Create a default header block

Create a default header block

## Usage

``` r
server_header(title, ..., program, date_range, region, county)
```

## Arguments

- title:

  `(character)` Title to be wrapped in
  [h2](https://rstudio.github.io/htmltools/reference/builder.html)

- program:

  `(character)` Program name to be wrapped in
  [h4](https://rstudio.github.io/htmltools/reference/builder.html)

- date_range:

  `(Date)` vector to be collapsed with \`" - "\` in
  [h4](https://rstudio.github.io/htmltools/reference/builder.html)

## Value

`shiny.tag.list`
