# Probability table

Put winning probabilities in nice table

## Usage

``` r
probs_table(probs, margins = TRUE)
```

## Arguments

- probs:

  numeric vector of length 6, representing outcome probabilities (must
  sum to 1 or 100)

- margins:

  boolean. Should margin totals be added? Defaults to TRUE

## Value

data.frame with rownames

## Examples

``` r
# XGID=-a-BaBC-A---eE---c-e----B-:0:0:1:00:0:0:0:0:10
# 4-ply winning chances, reported in a cumulative fashion:
cum_probs <- c(61.94, 24.09, 1.04, 38.06, 8.54, 0.42)
probs <- outcome_probs(cum_probs)
probs_table(probs)
#>               Regular Gammon Backgammon    Sum
#> Player wins     37.85  23.05       1.04  61.94
#> Opponent wins   29.52   8.12       0.42  38.06
#> Sum             67.37  31.17       1.46 100.00
```
