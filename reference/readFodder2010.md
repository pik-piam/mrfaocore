# Read Fodder2010

Read in new FAO fodder data (2010-2023) downloaded from the FAO SWS
system. This dataset uses CPC item codes and M49 country codes. Contains
elements: Area Harvested (5312, ha) and Production (5510, tonnes).

## Usage

``` r
readFodder2010()
```

## Value

FAO fodder data as MAgPIE object

## See also

\[readSource()\]

## Author

David Chen

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("Fodder2010")
} # }
```
