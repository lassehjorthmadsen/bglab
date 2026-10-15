# Double point calculation including gammons

Calculate cubeless double points at different scores, including gammons
and backgammons

## Usage

``` r
dp_gammons(x, y, probs, cube, met)
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

## Value

double. Double point
