# Moves from backgammon matches

A set of backgammon moves/decisions from matches played on Backgammon
Galaxy, analyzed by GNU Backgammon. Positions are the total set of moves
from more than 1000 matches, mostly played my me against various
opponents. It is the analyses files in text format, parsed into a single
data frame, using
[`bglab::txt2df()`](https://lassehjorthmadsen.github.io/bglab/reference/txt2df.md)

## Usage

``` r
bgmoves
```

## Format

### `bgmoves`

A data frame with 191,012 rows and 23 columns:

- file:

  name of the original file; may serve as a match id

- date:

  date of match

- player1, player2:

  nicknames of players

- game_no:

  game number in this match

- length:

  match length (a few unlimited games have the limit as match length)

- score1, score2:

  match score, player 1 and player2

- crawford:

  TRUE if this is the Crawford game, FALSE otherwise

- pos_id:

  position id in GNU Backgammon format

- match_id:

  match id in GNU Backgammon format

- move_no:

  move number

- play:

  the play made, one of "Accepts", "Doubles", "Rejects", "Rolls"

- turn:

  nickgame of player who made the play

- roll:

  dice roll, if applicable (NA if this is a cube decision)

- proper_ca:

  Proper, i.e. optimal, cube action, as reported by GNU Backgammon

- mistake_ca:

  TRUE if a cube action mistake was made, FALSE if correct cube action,
  NA if checker play decision

- move_err:

  The size of checker play error, 0 if no error, NA in case of no move,
  or cube decision

- cube_err:

  The sice of cube action error, 0 if no error, NA in case of no cube
  available, or checker play decision

- board:

  An ASCII text representation of the board, as displayed by GNU
  Backgammon

- cube_eq:

  An ASCII text with the cube decision equities, as displayed by GNU
  Backgammon

- move_eq:

  An ASCII text with the checker play decision equities, as displayed by
  GNU Backgammon

- xgid:

  eXtreme Backgammon position identifier, xgid

## Source

<https://github.com/lassehjorthmadsen/bglab>
