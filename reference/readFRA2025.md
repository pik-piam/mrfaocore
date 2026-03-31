# Read FRA2025

Read-in an FRA (forest resource assessment) dataset from 2025. Three
subtypes are added compared to FRA2020: expansion, afforestation,
net_change. Three files are required for running: FRA_Years_2025.csv,
Annual_2025.csv and Intervals_2025.csv.

## Usage

``` r
readFRA2025(subtype)
```

## Arguments

- subtype:

  data subtype. Available subtypes: forest_area, deforestation,
  growing_stock, biomass_stock, carbon_stock, management, disturbance,
  forest_fire, expansion, afforestation, net_change

## Value

Magpie object of the FRA 2025 data

## See also

\[readSource()\]

## Author

Simin Yu

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FRA2025", "growing_stock")
} # }
```
