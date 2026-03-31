# Convert FRA 2015 data Update dd-Jmm-jjjj - Please add comment if changes made here (Abhi)

Convert FRA 2015 data Update dd-Jmm-jjjj - Please add comment if changes
made here (Abhi)

## Usage

``` r
convertFAO_FRA2015(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing original values

- subtype:

  The FAO FRA 2015 file type, e.g.: fac, production, biodiversity or
  anndat.

## Value

Data as MAgPIE object with common country list

## See also

\[readSource()\],

## Author

Abhijeet Mishra

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FRA2015", "production", convert = TRUE)
} # }
```
