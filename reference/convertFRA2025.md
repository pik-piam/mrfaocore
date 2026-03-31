# Convert FRA2025 data

Convert FRA2025 data

## Usage

``` r
convertFRA2025(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing original values

- subtype:

  The FAO FRA 2025 subtype.

## Value

Data as MAgPIE object with common country list

## See also

\[readSource()\]

## Author

Simin Yu

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FRA2025", "growing_stock", convert = TRUE)
} # }
```
