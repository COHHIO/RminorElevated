# Give DT ready column numbers from names or numbers

Given a vector of column names or numbers, return the corresponding
column indices.

## Usage

``` r
which_cols(x, .data)
```

## Arguments

- x:

  `(character/numeric)` Column numbers or names to be converted to
  column indices.

- .data:

  `(data.frame)` The data frame from which column names are extracted.
  This parameter is used only in the character method.

## Value

`numeric` A vector of column indices.

## Examples

``` r
df <- data.frame(A = 1:3, B = 4:6, C = 7:9)
which_cols(c("A", "B"), df)  # Returns: 1 2
#> [1] 1 2
which_cols(c(1, 3), df)      # Returns: 1 3
#> [1] 1 3
```
