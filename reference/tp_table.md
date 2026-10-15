# Compare take points

Compute take points at a given match score, create a nice table to
compare those with money game take points

## Usage

``` r
tp_table(x, y, probs, cube, met, cube_eff = 2/3)
```

## Arguments

- x:

  number of points that player needs

- y:

  number of points that opponent needs

- probs:

  numeric vector of length 6, representing outcome probabilities (must
  sum to 1 or 100)

- cube:

  cube value

- met:

  match equity table

- cube_eff:

  cube-life index, between 0 and 1, defaults to 2/3

## Value

data.frame

## Examples

``` r
met <- get_met()
# XGID=-a-BaBC-A---eE---c-e----B-:0:0:1:00:0:0:0:0:10
# 4-ply winning chances, reported in a cumulative fashion:
cum_probs <- c(61.94, 24.09, 1.04, 38.06, 8.54, 0.42)
probs <- outcome_probs(cum_probs)
tp_table(3, 5, probs, 1, met)
#>        Cube assumptions Money game take point 3-away, 5-away take point
#> 1 Dead cube, no gammons             0.2500000                 0.2091928
#> 2      Cube, no gammons             0.2142857                 0.1871285
#> 3    Dead cube, gammons             0.2784478                 0.2822967
#> 4 Both cube and gammons             0.2472436                 0.2525219
```
