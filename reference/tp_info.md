# Take points in different flavors

Calculate cubeless and cubeful take points at different scores, as a
function of gammons, and backgammons, cube level, cube efficiency, and
match equity table

## Usage

``` r
tp_info(x, y, probs, cube, met, cube_eff = 2/3, last_roll = FALSE)
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

- cube_eff:

  Cube efficiency, defaults to 0.68

- last_roll:

  this as a last roll position; no automatic redouble available.
  Defaults to FALSE

## Value

List of take points in different flavors, along with informative metrics
from the calculation
