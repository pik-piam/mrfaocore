# calcCombineFodder

Combine old FAO fodder data (pre-2010 item codes, 1961-2011) with new
Fodder2010 data (CPC codes, 2010-2023). The old data is kept up to its
last year (2011), the new data is used afterwards. New items without an
old equivalent are aggregated into the old item codes (see
FodderItemMapping.csv). Old items without a new equivalent (648 Carrots
for fodder, global production \< 0.01 Mt) are set to 0 after the last
old year. Gaps in the newdata (zero production) are filled by carrying
the last available value forward.

## Usage

``` r
calcCombineFodder()
```

## Value

Combined fodder data in tonnes (production, feed, domestic_supply) and
ha (area_harvested) as a list with MAgPIE object, weight, unit, and
description

## See also

\[readSource()\], \[calcOutput()\]

## Author

David Chen

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("CombineFodder")
} # }
```
