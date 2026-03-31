# calcFAOharmonized

Calculate harmonized FAO Commodity Balance and Food Supply data based on
CB, only harvested areas are taken from ProdSTAT. This functions adds
the CBCrop, CBLive, FSCrop and FSLive data together.

## Usage

``` r
calcFAOharmonized(src = "pre2010", output = "FB")
```

## Arguments

- src:

  source "pre2010", "post2010", or "join2010". "pre2010" returns the FAO
  sheet that runs until 2013 (actually 2013, but gets effectively
  chopped to 2010 by massbalance). "post2010" combines the new FAO FB,
  SUA, and CB sheets that were re-done for 2010 onwards. "join2010"
  joins pre2010 and post2010 data, taking new values from 2010 onwards.

- output:

  whether to return FB (Food balance sheet) or SUA (SUpply Utilization
  Accounts)

## Value

FAO harmonized data, weight as NULL, and a description as as a list of
MAgPIE objects

## Author

Ulrich Kreidenweis, David Chen, Kristine Karstens, Patrick Rein

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("FAOharmonized")
} # }
```
