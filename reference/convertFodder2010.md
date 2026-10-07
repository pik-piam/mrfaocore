# Convert Fodder2010 data

Converts new FAO Fodder2010 data to fit to the common country list. Data
starts in 2010 so no historical country transitions are needed, but
standard ones are included defensively. Units are kept as tonnes and ha.

## Usage

``` r
convertFodder2010(x)
```

## Arguments

- x:

  MAgPIE object containing original values

## Value

Data as MAgPIE object with common country list

## See also

\[readFodder2010()\], \[readSource()\]

## Author

David Chen

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("Fodder2010", convert = TRUE)
} # }
```
