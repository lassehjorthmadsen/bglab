# Take point calculation including gammons

Calculate cubeless take points at different scores, including gammons
and backgammons

## Usage

``` r
tp_gammons(x, y, probs, cube, met, last_roll = FALSE)
```

## Arguments

- x:

  number of points that player needs

- y:

  number of points that opponent needs

- probs:

  numeric vector of length 6, representing outcome probabilities (must
  always sum to 1 or 100)

- cube:

  cube value (before doubling)

- met:

  match equity table

- last_roll:

  treat this as a last roll position; no automatic redouble available

## Value

double. Take point
