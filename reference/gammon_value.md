# Gammon value

Calculate gammon value for a score and cube level: The gain from winning
a gammon instead of single game, divided by the loss from losing a
single game instead of winning one.

## Usage

``` r
gammon_value(x, y, cube, met)
```

## Arguments

- x:

  number of points that player needs

- y:

  number of points that opponent needs

- cube:

  cube value

- met:

  match equity table

## Value

a real number, the value of winning a gammon
