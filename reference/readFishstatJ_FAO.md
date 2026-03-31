# readFishstatJ_FAO

Reads data of fisheries generated using the FishstatJ app of FAO.
Read-in specifically, exports_value, exports_quantity, and/or overall
production of fish/aquatic products.

## Usage

``` r
readFishstatJ_FAO(subtype = "Production")
```

## Arguments

- subtype:

  data subtype needed. Either "exportsValue", "exportsQuantity", or
  "Production"

## Value

magpie object of either tonnes of liveweight or 1000 current USD

## See also

\[readSource()\]

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FishstatJ_FAO", "Production")
a <- readSource("FishstatJ_FAO", "exportsQuantity")
a <- readSource("FishstatJ_FAO", "exportsValue")
} # }
```
