# Match winning chances at score

Look up match winning chances at different scores in a given match
equity table

## Usage

``` r
mwc(x, y, met)
```

## Arguments

- x:

  number of points that player needs

- y:

  number of points that opponent needs

- met:

  match equity table

## Value

double. Match winning chance

## Examples

``` r
met <- get_met() # Get the default Kazaross XG2 table
mwc(3, 5, met)   # Match winning chance at 3-away, 5-away
#> [1] 0.64795
```
