# Convert FAO data

Converts FAO data to fit to the common country list and removes or
converts relative values where possible. Yields (Hg/ha) are for instance
removed since they can later easily be calculated from production and
area but might be problematic in the spatial aggregation. Per capita
demand values are transformed into absolute values using population
estimates from the calcPopulationPast function.

## Usage

``` r
convertFAO_online(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing original values

- subtype:

  The FAO file type, e.g.: CBCrop

## Value

Data as MAgPIE object with common country list

## Details

Update 23-Jan-2017 - Added FAO Forestry production and trade data (Abhi)

## See also

\[readFAO()\], \[readSource()\],

## Author

Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic, David Klein,
Daivd Chen, Edna Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FAO_online", "Crop", convert = TRUE)
} # }
```
