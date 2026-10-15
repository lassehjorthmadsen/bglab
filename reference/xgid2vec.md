# Checker locations as integer vector

Takes the checker locations from a XGID string (first 26 elements) and
turns it into a signed integer vector with 26 elements where positive
numbers indicate how many checkers one player has on a point; negative
numbers are the other player's count of checkers. The remainder of
elements represent derived features, like amount of contact.

## Usage

``` r
xgid2vec(xgid, flip = FALSE)
```

## Arguments

- xgid:

  character

- flip:

  logical, defaults to FALSE. Should the sides be flipped?

## Value

vector of length 26 with signed integers

## Examples

``` r
xgid2vec("XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:3:0:10")
#>  [1]   0   2   0   0   0   0  -5   0  -3   0   0   0   5  -5   0   0   0   3   0
#> [20]   5   0   0   0   0  -2   0 167 167 167
```
