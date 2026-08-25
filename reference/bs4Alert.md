# Create a bootstrap 4 Alert box

Create a bootstrap 4 Alert box

## Usage

``` r
bs4Alert(..., status = "primary", style = NULL, id = NULL, width = 6)
```

## Arguments

- ...:

  Contents of the box.

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

- style:

  `(character)` Inline style parameters to add

- id:

  Card id.

- width:

  The width of the box, using the Bootstrap grid system. This is used
  for row-based layouts. The overall width of a region is 12, so the
  default card width of 6 occupies 1/2 of that width. For column-based
  layouts, use `NULL` for the width; the width is set by the column that
  contains the box.
