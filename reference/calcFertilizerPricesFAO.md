# calcFertilizerPricesFAO

calculates dataset of fertilizer prices in US\$MER17/tonne (either
referring to the amount of fertilizer product, or to the amount of
nutrients within the fertilizer) based on FAO data

## Usage

``` r
calcFertilizerPricesFAO(subtype = "N", by = "nutrient")
```

## Arguments

- subtype:

  "N" for fertilizer containing nitrogen, "P" for fertilizer containing
  phosphorus

- by:

  "nutrient" if referring to price per amount of nutrients (N or P)
  within the fertilizer products, or "product" if referring to price per
  amount of fertilizer product

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FertilizerPricesFAO", subtype = "N", by = "nutrient")
} # }
```
