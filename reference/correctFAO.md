# correctFAO

Corrects FAO data for known mismatches or insufficiencies

## Usage

``` r
correctFAO(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing original values

- subtype:

  The FAO file type, e.g.: CBCrop

## Value

Data as MAgPIE object

## See also

\[readFAO()\], \[readSource()\],

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FAO", "Crop", convert = TRUE)
} # }
```
