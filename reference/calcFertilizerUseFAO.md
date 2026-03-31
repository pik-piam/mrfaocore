# calcFertilizerUseFAO

calculates dataset of fertilizer use in tonnes (either referring to the
amount of fertilizer products used, or to the amount of nutrients within
the fertilizer used) based on FAO data

## Usage

``` r
calcFertilizerUseFAO(subtype = "N", by = "nutrient")
```

## Arguments

- subtype:

  "N" for fertilizer containing nitrogen, "P" for fertilizer containing
  phosphorus (note that there is an overlap between those categories, as
  some fertilizers include both nutrients)

- by:

  "nutrient" if referring to amount of nutrients (N or P) in total used
  fertilizer, or "product" if referring to total amount of fertilizer
  used

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FertilizerUseFAO", subtype = "N", by = "nutrient")
} # }
```
