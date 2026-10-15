# From GNU BG ids to XGIDs

Convert GNU Backgammon position and match ids to XGID, the format used
by eXtreme Gammon

## Usage

``` r
gnuid2xgid(pos_id, match_id)
```

## Arguments

- pos_id:

  character. GNU Backgammon position id

- match_id:

  character. GNU Backgammon match id

## Value

character

## Examples

``` r
gnuid2xgid(pos_id = "4HPwATDgc/ABMA", match_id = "8IhuACAACAAE")
#> [1] "XGID=-b----E-C---eE---c-e----B-:0:0:1:53:1:2:1:3:10"
```
