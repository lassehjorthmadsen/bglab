# Scale match winning chances to money game

This scales a match winning chance at a certain score to the more
familiar \[-1, +1\] money game-like interval.

## Usage

``` r
emg(mwc, a, b, cube, met)
```

## Arguments

- mwc:

  current match winning chances for player

- a:

  number of points that player needs

- b:

  number of points that opponent needs

- cube:

  cube value

- met:

  match equity table

## Value

double

## Examples

``` r
# If I have 0.7 to win the match at 3-away, 5-away,
# how good is this on an money game scale?
met <- get_met() # Get the default Kazaross XG2 table
emg(0.7, 3, 5, 1, met)
#> [1] 0.4934046
```
