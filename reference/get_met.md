# Get match equity table

Get match equity table from \*.met file (used by eXtreme Gammon).
Available tables are `GnuBG-11-point.met`, `Jacobs-Trice.met`,
`Snowie.met`, `Woolsey.met`, `Kazaross-XG2.met`, and
`Rockwell-Kazaross.met`

## Usage

``` r
get_met(filename = "Kazaross-XG2.met")
```

## Arguments

- filename:

  name of \*.met file. Defaults to "Kazaross-XG2.met"

## Value

matrix

## Examples

``` r
met <- get_met() # Get the default Kazaross XG2 table
met[1:5, 1:5]
#>         1       2       3       4       5
#> 1 0.50000 0.67736 0.75076 0.81436 0.84179
#> 2 0.32264 0.50000 0.59947 0.66870 0.74359
#> 3 0.24924 0.40053 0.50000 0.57150 0.64795
#> 4 0.18564 0.33130 0.42850 0.50000 0.57732
#> 5 0.15821 0.25641 0.35205 0.42268 0.50000
met <- get_met("Woolsey.met") # Get the older Kit Woolsey table
met[1:5, 1:5]
#>      1    2    3    4    5
#> 1 0.50 0.70 0.75 0.83 0.85
#> 2 0.30 0.50 0.60 0.68 0.75
#> 3 0.25 0.40 0.50 0.59 0.66
#> 4 0.17 0.32 0.41 0.50 0.58
#> 5 0.15 0.25 0.34 0.42 0.50
```
