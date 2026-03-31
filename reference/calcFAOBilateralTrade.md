# calcFAOBilateralTrade

Calculates bilateral trade values based on FAO trade matrix

## Usage

``` r
calcFAOBilateralTrade(
  output = "value",
  products = "kcr",
  prodAgg = TRUE,
  harmonize = FALSE,
  fiveYear = TRUE
)
```

## Arguments

- output:

  "value", "qty", or "price"

- products:

  "kcr", "kli", or "kothers"

- prodAgg:

  binary to keep FAO product level or magpie

- harmonize:

  combine input and export sheets with harmonization algorithm based on
  reliability index(Gelhar 1996) default off as there are some big
  differences and perhaps using the import sheet is better overall i.e.
  more matching with massbal

- fiveYear:

  if TRUE calculate only 5 years (1995 to 2020, 5-year steps) to reduce
  memory load

## Value

List of magpie objects with results on bilateral country level, weight
on bilateral country level, unit and description.

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FAOBilateralTrade", output = "qty", products = "kcr")
} # }
```
