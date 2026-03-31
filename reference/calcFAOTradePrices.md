# calcFAOTradePrices

calculates USD per kg of FAOSTAT Trade data for import and export prices

## Usage

``` r
calcFAOTradePrices(aggregation = "k")
```

## Arguments

- aggregation:

  "none", "k", "fbs" or "springmann" for the last uses Marco
  Springmann's custom product mapping

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("calcFAOTradePrices")
} # }
```
