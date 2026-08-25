# Filter data.frame with default filters for DQ

Filters by \`input\$program\` & \`input\$date_range\`

## Usage

``` r
dq_filter_between(x, ..., date_range, program)
```

## Arguments

- x:

  `(data.frame)` with \`ProjectName\`, \`EntryDate\`, \`ExitDate\`

- ...:

  `(character)` Expressions passed on to
  [filter](https://dplyr.tidyverse.org/reference/filter.html)

- program:

  Program ID to filter for

- env:

  `(environment)` The parent environment from which to retrieve input
  reactiveValues

## Value

`(data.frame)` filtered accordingly

## Examples

``` r
test <- data.frame(Issue = 1:5, Type = sample(c("Warning", "Error"), 5, TRUE), ProjectName = letters[1:5], EntryDate = seq.Date(lubridate::floor_date(lubridate::today() - 4, "month"), Sys.Date(), length.out = 5), ExitDate = seq.Date(lubridate::today() - 4, Sys.Date(), by = "day"))
```
