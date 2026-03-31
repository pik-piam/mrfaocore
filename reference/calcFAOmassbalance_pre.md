# calcFAOmassbalance_pre

Calculates an extended version of the Food Balance Sheets. Makes
explicit the conversion processes that convert one type of product into
another. Includes processes like milling, distilling, extraction etc.
Adds certain byproducts like distillers grains or ethanol.

## Usage

``` r
calcFAOmassbalance_pre(version = "join2010", years = NULL)
```

## Arguments

- version:

  whether to return the new post-2010 massbalance "post2010", the older
  sheets "pre2010", or or join them at 2010 (replace old with new
  at 2010) "join2010"

- years:

  years to be estimated, if NULL, then all years in FAOharmonized are
  returned

## Value

List of magpie objects with results on country level, weight on country
level, unit and description. This is an intermediary result, which is
used e.g. for estimating the feed baskets. For most uses, it is more
appropriate to use the FAOmasbalance instead of the FAOmassbalance_pre.

## See also

\[calcFAOmassbalance()\]

## Author

Benjamin Leon Bodirsky, David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FAOmassbalance_pre")
} # }
```
