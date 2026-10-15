# Take points according to eXtreme Gammon

Calculate dead cube and live cube take points like eXtreme Gammon does
in the 'Cube Information' window. Based on match score score, gammons,
and backgammons, cube level, and match equity table

## Usage

``` r
tp_xg(x, y, probs, cube, met, last_roll = FALSE)
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

  this as a last roll position; no automatic redouble available.
  Defaults to FALSE

## Value

List of take point: 'Dead Cube' and 'Live Cube'
