# Convert FRA 2020 data

Convert FRA 2020 data

## Usage

``` r
convertFRA2020(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing original values

- subtype:

  The FAO FRA 2020 subtype.

## Value

Data as MAgPIE object with common country list

## See also

\[readSource()\],

## Author

Abhijeet Mishra

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FRA2020", "growing_stock", convert = TRUE)
} # }
```
