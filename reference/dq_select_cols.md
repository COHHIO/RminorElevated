# Select default display columns for Data Quality Tables

Select default display columns for Data Quality Tables

## Usage

``` r
dq_select_cols(
  x,
  ...,
  default = list("UniqueID", "EnrollmentID", `Entry Date` = "EntryDate", "Type", "Issue")
)
```

## Arguments

- x:

  `(data.frame)` The data frame from which columns will be selected.

- ...:

  `(columns to select)` Columns to select, can be unquoted or quoted.

- default:

  `(list or logical)` Columns to select as defaults. If \`TRUE\`,
  defaults are used. If \`FALSE\`, no default columns are selected. Can
  also be a list of column names or expressions.

## Value

`(data.frame)` with selected columns.

## Examples

``` r
# Selecting columns with default columns
dq_select_cols(data.frame(UniqueID = 1:3, EnrollmentID = 4:6, Issue = letters[1:3], EntryDate = 1:3, Type = 1:3))
#>   UniqueID Entry Date EnrollmentID Type Issue
#> 1        1          1            4    1     a
#> 2        2          2            5    2     b
#> 3        3          3            6    3     c

# Selecting columns with custom defaults
dq_select_cols(data.frame(UniqueID = 1:3, EnrollmentID = 4:6, Issue = letters[1:3], EntryDate = 1:3, blah = 1:3), default = list("UniqueID", "EntryDate"))
#>   UniqueID EntryDate
#> 1        1         1
#> 2        2         2
#> 3        3         3
```
