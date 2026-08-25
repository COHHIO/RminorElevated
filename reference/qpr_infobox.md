# qpr_infobox

Function to render infobox from default template for QPR tabitems

## Usage

``` r
qpr_infobox(
  .data,
  .replace = FALSE,
  title = "Average Score",
  color = "purple",
  value = .data$AvgScore,
  icon = shiny::icon("shopping-cart"),
  subtitle = "See table below for detail.",
  ...
)
```

## Arguments

- .data:

  `(any)` Data to be passed in and used subsequent arguments

- .replace:

  `(logical)` whether to replace the default arguments with those
  supplied and eliminate the default arguments, or to replace existing
  defaults & and add additional args specified

- title:

  Info box title.

- color:

  A color for the box. Valid colors are defined as follows:

  - `primary`: \#007bff .

  - `secondary`: \#6c757d .

  - `info`: \#17a2b8 .

  - `success`: \#28a745 .

  - `warning`: \#ffc107 .

  - `danger`: \#dc3545 .

  - `gray-dark`: \#343a40 .

  - `gray`: \#adb5bd .

  - `white`: \#fff .

  - `indigo`: \#6610f2 .

  - `lightblue`: \#3c8dbc .

  - `navy`: \#001f3f .

  - `purple`: \#605ca8 .

  - `fuchsia`: \#f012be .

  - `pink`: \#e83e8c .

  - `maroon`: \#d81b60 .

  - `orange`: \#ff851b .

  - `lime`: \#01ff70 .

  - `teal`: \#39cccc .

  - `olive`: \#3d9970 .

- value:

  The value to display in the box. Usually a number or short text.

- icon:

  An icon tag, created by
  [`icon`](https://rdrr.io/pkg/shiny/man/icon.html).

- subtitle:

  Any extra UI element.

- ...:

  Arguments passed on to
  [`bs4Dash::infoBox`](https://bs4dash.rinterface.com/reference/infoBox.html)

  `width`

  :   The width of the box, using the Bootstrap grid system. This is
      used for row-based layouts. The overall width of a region is 12,
      so the default width of 4 occupies 1/3 of that width. For
      column-based layouts, use `NULL` for the width; the width is set
      by the column that contains the box.

  `href`

  :   An optional URL to link to.

  `fill`

  :   If FALSE (the default), use a white background for the content,
      and the color argument for the background of the icon. If TRUE,
      use the color argument for the background of the content; the icon
      will use the same color with a slightly darkened background.

  `gradient`

  :   Whether to use gradient style for background color. Default to
      FALSE.

  `elevation`

  :   Box elevation.

  `iconElevation`

  :   Icon elevation compared to the main content (relief). 3 by
      default.

  `tabName`

  :   Optional:
      [infoBox](https://bs4dash.rinterface.com/reference/infoBox.html)
      behaves like
      [menuItem](https://bs4dash.rinterface.com/reference/dashboardSidebar.html)
      and may be used to navigate between multiple
      [tabItem](https://bs4dash.rinterface.com/reference/dashboardBody.html).
