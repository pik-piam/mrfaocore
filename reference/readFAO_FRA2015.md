# Read FAO_FRA2015

Read-in an FRA data from 2015 (forest resource assessment)

## Usage

``` r
readFAO_FRA2015(subtype)
```

## Arguments

- subtype:

  data subtype. Either "production" or "fac" (forest area and
  characteristics) or "biodiversity" or "anndat" (Annual Data)

## Value

magpie object of the FRA 2015 data

## See also

\[readSource()\]

## Author

Abhijeet Mishra

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FAO_FRA2015", "production")
} # }
```
