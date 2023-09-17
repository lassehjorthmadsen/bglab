#' Moves from backgammon matches
#'
#' A set of backgammon moves/decisions from matches played on
#' Backgammon Galaxy, analyzed by GNU Backgammon. Positions are
#' the total set of moves from a a total of 312 matches, mostly
#' 3-pointers, played against various opponents. It is the
#' analysis files in text format, parsed into a single data frame,
#' using `backgammon::galaxy2df()`
#'
#' @format ## `bgmoves`
#' A data frame with 35,536 rows and 21 columns:
#' \describe{
#'   \item{file}{name of the original file; may serve as a match id}
#'   \item{date}{date of match}
#'   \item{game_no}{game number in this match}
#'   \item{player1, player2}{nicknames of players}
#'   \item{length}{match length (a few unlimited games have the limit as match length)}
#'   \item{score1, score2}{match score, player 1 and player2}
#'   \item{crawford}{TRUE if this is the Crawford game, FALSE otherwise}
#'   \item{pos_id}{position id in GNU Backgammon format}
#'   \item{match_id}{match id in GNU Backgammon format}
#'   \item{move_no}{move number}
#'   \item{play}{the play made, one of "Accepts", "Doubles", "Rejects", "Rolls"}
#'   \item{turn}{nickgame of player who made the play}
#'   \item{roll}{dice roll, if applicable (NA if this is a cube decision)}
#'   \item{proper_ca}{Proper, i.e. optimal, cube action, as reported by GNU Backgammon}
#'   \item{mistake}{TRUE if a cube action mistake was made, FALSE if correct cube action, NA if checker play decision}
#'   \item{move_err}{The size of checker play error, 0 if no error, NA in case of no move, or cube decision}
#'   \item{cube_err}{The sice of cube action error, 0 if no error, NA in case of no cube available, or checker play decision}
#'   \item{board}{An ASCII text representation of the board, as displayed by GNU Backgammon}
#'   \item{cube_eq}{An ASCII text with the cube decision equities, as displayed by GNU Backgammon}
#'   \item{move_eq}{An ASCII text with the checker play decision equities, as displayed by GNU Backgammon}
#' }
#' @source <https://github.com/lassehjorthmadsen/backgammon>
"bgmoves"
