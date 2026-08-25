# Iterative generation of icons

Generates a list of icons based on the provided parameters using
\`shiny::icon\`.

## Usage

``` r
ui_icons(name, class = NULL, lib = "font-awesome", ...)
```

## Arguments

- name:

  `(character)` The name(s) of the icon(s) to generate.

- class:

  `(character, optional)` Additional CSS class(es) to apply to the
  icon(s).

- lib:

  `(character)` The library from which to source the icon(s). Default is
  "font-awesome".

- ...:

  Additional arguments passed to
  [`icon`](https://rdrr.io/pkg/shiny/man/icon.html).

## Value

A `tibble` with a column of generated icons as
[`shiny::icon`](https://rdrr.io/pkg/shiny/man/icon.html) objects.

## Examples

``` r
# Generate icons with default parameters
ui_icons(name = c("home", "user", "cog"))
#> [[1]]
#> 
#> [[2]]
#> 
#> [[3]]
#> 
```
