# safe_reactive_quoted

Function to safely evaluate a quoted reactive expression, surfacing
errors via shinyalert instead of crashing the session.

## Usage

``` r
safe_reactive_quoted(
  expr,
  error_message = "Something went wrong loading this report."
)
```
