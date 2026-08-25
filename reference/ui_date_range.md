# A date range picker with sensible defaults

A date range picker with sensible defaults

## Usage

``` r
ui_date_range(
  inputId = rlang::caller_env()$ns("date_range"),
  label = "Date Range",
  start = Sys.Date() - lubridate::days(7),
  end = Sys.Date(),
  min = get_app_data("rm_dates")$meta_HUDCSV$Export_Start,
  width = 300,
  ...
)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- start:

  The initial start date. Either a Date object, or a string in
  `yyyy-mm-dd` format. If NULL (the default), will use the current date
  in the client's time zone.

- end:

  The initial end date. Either a Date object, or a string in
  `yyyy-mm-dd` format. If NULL (the default), will use the current date
  in the client's time zone.

- min:

  The minimum allowed date. Either a Date object, or a string in
  `yyyy-mm-dd` format.

- width:

  The width of the input, e.g. `'400px'`, or `'100%'`; see
  [`validateCssUnit()`](https://rstudio.github.io/htmltools/reference/validateCssUnit.html).

- ...:

  Arguments passed on to
  [`shiny::dateRangeInput`](https://rdrr.io/pkg/shiny/man/dateRangeInput.html)

  `max`

  :   The maximum allowed date. Either a Date object, or a string in
      `yyyy-mm-dd` format.

  `format`

  :   The format of the date to display in the browser. Defaults to
      `"yyyy-mm-dd"`.

  `startview`

  :   The date range shown when the input object is first clicked. Can
      be "month" (the default), "year", or "decade".

  `weekstart`

  :   Which day is the start of the week. Should be an integer from 0
      (Sunday) to 6 (Saturday).

  `language`

  :   The language used for month and day names. Default is "en". Other
      valid values include "ar", "az", "bg", "bs", "ca", "cs", "cy",
      "da", "de", "el", "en-AU", "en-GB", "eo", "es", "et", "eu", "fa",
      "fi", "fo", "fr-CH", "fr", "gl", "he", "hr", "hu", "hy", "id",
      "is", "it-CH", "it", "ja", "ka", "kh", "kk", "ko", "kr", "lt",
      "lv", "me", "mk", "mn", "ms", "nb", "nl-BE", "nl", "no", "pl",
      "pt-BR", "pt", "ro", "rs-latin", "rs", "ru", "sk", "sl", "sq",
      "sr-latin", "sr", "sv", "sw", "th", "tr", "uk", "vi", "zh-CN", and
      "zh-TW".

  `separator`

  :   String to display between the start and end input boxes.

  `autoclose`

  :   Whether or not to close the datepicker immediately when a date is
      selected.
