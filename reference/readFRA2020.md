# Read FRA2020

Read-in an FRA (forest resource assessment) dataset from 2020.

## Usage

``` r
readFRA2020(subtype)
```

## Arguments

- subtype:

  data subtype. Available subtypes: forest_area, deforestation,
  growing_stock, biomass_stock, carbon_stock, management, disturbance,
  forest_fire

## Value

Magpie object of the FRA 2020 data

## See also

\[readSource()\]

## Author

Abhijeet Mishra

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FRA2020", "growing_stock")
} # }
```
