# toolExtrapolateFodder

Extrapolate fodder data, based on two time steps (5-averages around this
years)

## Usage

``` r
toolExtrapolateFodder(x, exyears = c(2004, 2009), average = 5, endyear = 2015)
```

## Arguments

- x:

  input data

- exyears:

  two years

- average:

  the averaging_range in toolTimeInterpolate

- endyear:

  year till when it should be extrapolated

## Value

magpie object including extrapolated years

## Author

Kristine Karstens
