# Simple take point calculation

Calculate cubeless, gammonless take points at different scores

## Usage

``` r
tp(x, y, cube, met, last_roll = FALSE)
```

## Arguments

- x:

  number of points that player needs

- y:

  number of points that opponent needs

- cube:

  cube value (before doubling)

- met:

  match equity table

- last_roll:

  treat this as a last roll position; no automatic redouble available

## Value

double. Take point

## Examples

``` r
met <- get_met() # Get the default Kazaross XG2 table
tp(3, 5, 1, met)
#> [1] 0.2091928
```
