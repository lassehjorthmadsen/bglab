# Outcome probabilities

Convert outcome distributions from eXtreme Gammon to probabilities

## Usage

``` r
outcome_probs(xg_probs)
```

## Arguments

- xg_probs:

  numeric vector of length 6, corresponding to the winning chances
  reported by eXtreme Gammon. Can be percentages or decimal fractions.

## Value

named numeric vector of length 6, representing outcome probabilities
(always sum to 1 or 100)

## Details

Both eXtreme Gammon and GNU Backgammon report estimated outcome
probabilities in a cumulative way: The number under "wins" is the
probability of winning an ordinary game, a gammon or a backgammon.
Sometimes it can be convenient to have discrete probabilities.

## Examples

``` r
# XGID=-a-BaBC-A---eE---c-e----B-:0:0:1:00:0:0:0:0:10
# 4-ply winning chances, reported in a cumulative fashion:
cum_probs <- c(61.94, 24.09, 1.04, 38.06, 8.54, 0.42)
outcome_probs(cum_probs)
#>  win_single  win_gammon      win_bg lose_single lose_gammon     lose_bg 
#>       37.85       23.05        1.04       29.52        8.12        0.42 
```
