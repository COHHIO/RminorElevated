# A pickerInput that provides active programs to select from

A pickerInput that provides active programs to select from

## Usage

``` r
ui_picker_program(
  label = "Select Program",
  inputId = rlang::caller_env()$ns("program"),
  choices = programs,
  selected = NULL,
  multiple = TRUE,
  options = shinyWidgets::pickerOptions(liveSearch = TRUE, liveSearchStyle = "contains",
    actionsBox = TRUE),
  ...,
  add_options
)
```

## Arguments

- label:

  Display label for the control, or `NULL` for no label.

- inputId:

  `(character)` Automatically namespace with ID \`'project'\` if non
  specified.

- choices:

  List of values to select from. If elements of the list are named then
  that name rather than the value is displayed to the user.

- selected:

  The initially selected value (or multiple values if
  `multiple = TRUE`). If not specified then defaults to the first value
  for single-select lists and no values for multiple select lists.

- multiple:

  Is selection of multiple items allowed?

- options:

  List of options, see
  [pickerOptions](https://dreamrs.github.io/shinyWidgets/reference/pickerOptions.html)
  for all available options. To limit the number of selection possible,
  see example below.

- ...:

  Arguments passed on to
  [`shinyWidgets::pickerInput`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html)

  `choicesOpt`

  :   A [`list()`](https://rdrr.io/r/base/list.html) of options for
      individual choices in the dropdown menu, each element of the
      `list` should the same length as `choices`. You can use the
      following options :

      - `disabled`: logical vector indicating if the choice can be
        selected or not.

      - `style`: CSS styles applied to the choice tag

      - `class`: CSS class added to the choice tag

      - `icon`: vector of icon names to display before choices (to use
        `icon("arrow-right")`, you have to use `fa-arrow-right` and
        `pickerOptions(iconBase = "fas")`)

      - `subtext` add a text after the choice

      - `content`: replace entire choice with custom content (like raw
        HTML)

      - `tokens`: add tokens associated with choices used in search
        results.

  `width`

  :   The width of the input : 'auto', 'fit', '100px', '75%'.

  `inline`

  :   Display picker inline, to have label and menu on same line use
      `width = "fit"`.

  `stateInput`

  :   Activate or deactivate the special input value
      `input$<inputId>_open` to know if the menu is opened or not, see
      details.

  `autocomplete`

  :   Sets the initial state of the autocomplete property.

- add_options:

  `(list)` of options to add to existing defaults

## Value

A select control that can be added to a UI definition.
