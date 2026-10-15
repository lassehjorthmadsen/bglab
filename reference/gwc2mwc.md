# Calculate match winning chance from game winning chances

Given the game winning probability, match score, and cube value, what is
a players match winning probability?

## Usage

``` r
gwc2mwc(pwin, x, y, cube, met)
```

## Arguments

- pwin:

  game winning chances (cubeless)

- x:

  number of points that player needs

- y:

  number of points that opponent needs

- cube:

  cube value

- met:

  match equity table

## Value

double

## Examples

``` r
met <- get_met() # Get the default Kazaross XG2 table
gwc2mwc(0.5, 3, 5, 1, met)
#> [1] 0.657545
```
