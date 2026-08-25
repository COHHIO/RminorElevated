# Convert the occurrence of a symbol in an R document to a function call by the same name

Useful to replace calls to an object by symbol name with an accessor
function or reactive with the same name

## Usage

``` r
sym2function(x, file)
```

## Arguments

- x:

  `(character)` All symbols to change

- file:

  `(character)` Path to file in which to change them

## Value

`(character)` vector of text in the file with the new function calls
