# Combine FAO datasets

Allows to combine two similar FAO datasets with dublicates being
removed. For instance combine Production:Crops and Production: Crops
Processed to one magpie object

## Usage

``` r
toolFAOcombine(..., combine = "Item")
```

## Arguments

- ...:

  two magpie objects with FAO data

- combine:

  "Item" to combine datasets that for instance both contain palm oil
  data

## Value

MAgPIE object with data from both inputs but dublicates removed

## See also

\[readSource()\]

## Author

Ulrich Kreidenweis

## Examples

``` r
if (FALSE) { # \dontrun{
a <- toolFAOcombine(Crop, CropPro, combine = "Item")
} # }
```
