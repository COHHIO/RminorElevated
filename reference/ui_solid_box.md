# A default full width row box.

A default full width row box.

## Usage

``` r
ui_solid_box(
  ...,
  title = NULL,
  footer = NULL,
  status = NULL,
  solidHeader = TRUE,
  background = NULL,
  width = 12,
  height = NULL,
  collapsible = TRUE,
  collapsed = FALSE,
  closable = FALSE,
  maximizable = FALSE,
  icon = NULL,
  gradient = FALSE,
  boxToolSize = "sm",
  elevation = NULL,
  headerBorder = TRUE,
  label = NULL,
  dropdownMenu = NULL,
  sidebar = NULL,
  id = NULL
)
```

## Arguments

- ...:

  Contents of the box.

- title:

  Optional title.

- footer:

  Optional footer text.

- status:

  The status of the item. This determines the item's background color.
  Valid statuses are defined as follows:

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

- solidHeader:

  Should the header be shown with a solid color background?

- background:

  If NULL (the default), the background of the box will be white.
  Otherwise, a color string. Valid colors are listed in
  [validColors](https://bs4dash.rinterface.com/reference/validColors.html).
  See below:

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

- width:

  The width of the box, using the Bootstrap grid system. This is used
  for row-based layouts. The overall width of a region is 12, so the
  default card width of 6 occupies 1/2 of that width. For column-based
  layouts, use `NULL` for the width; the width is set by the column that
  contains the box.

- height:

  The height of a box, in pixels or other CSS unit. By default the
  height scales automatically with the content.

- collapsible:

  If TRUE, display a button in the upper right that allows the user to
  collapse the box.

- collapsed:

  If TRUE, start collapsed. This must be used with `collapsible=TRUE`.

- closable:

  If TRUE, display a button in the upper right that allows the user to
  close the box.

- maximizable:

  If TRUE, the card can be displayed in full screen mode.

- icon:

  Header icon. Displayed before title. Expect
  [`icon`](https://rdrr.io/pkg/shiny/man/icon.html).

- gradient:

  Whether to allow gradient effect for the background color. Default to
  FALSE.

- boxToolSize:

  Size of the toolbox: choose among "xs", "sm", "md", "lg".

- elevation:

  Card elevation.

- headerBorder:

  Whether to display a border between the header and body. TRUE by
  default

- label:

  Slot for
  [boxLabel](https://bs4dash.rinterface.com/reference/boxLabel.html).

- dropdownMenu:

  List of items in the boxtool dropdown menu. Use
  [boxDropdown](https://bs4dash.rinterface.com/reference/boxDropdown.html).

- sidebar:

  Slot for
  [boxSidebar](https://bs4dash.rinterface.com/reference/boxSidebar.html).

- id:

  Card id.

## Value

A [box](https://bs4dash.rinterface.com/reference/box.html) with solid
header

## Examples

``` r
ui_solid_box("Hi")
#> <div class="row ui_row">
#>   <div class="col-sm-12">
#>     <div class="card bs4Dash">
#>       <div class="card-header">
#>         <h3 class="card-title">‌</h3>
#>         <div class="card-tools float-right">
#>           <button class="btn btn-tool btn-sm" type="button" data-card-widget="collapse">
#>             <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
#>           </button>
#>         </div>
#>       </div>
#>       <div class="card-body">Hi</div>
#>     </div>
#>     <script type="application/json">{"solidHeader":true,"width":12,"collapsible":true,"closable":false,"maximizable":false,"gradient":false}</script>
#>   </div>
#> </div>
```
