# Equity, money game

Calculate equity for money game, Janowski-style

## Usage

``` r
eq_money(probs, C, p, cube_eff = 2/3)
```

## Arguments

- probs:

  numeric vector of length 6, representing outcome probabilities (must
  always sum to 1 or 100)

- C:

  Cube position: 0: Center; 1: player; -1: opponent

- p:

  Probability of winning

- cube_eff:

  cube-life index, between 0 and 1

## Value

double. Equity

## Examples

``` r
probs <- c(31, 4, 0, 47, 17, 1)
eq_money(probs = probs, C = 1, p = 0.5)
#> [1] 0.07765568
```
