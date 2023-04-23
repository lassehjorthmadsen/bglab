#' Scale match winning chance to equivalent to money game
#'
#' @param mwc current match winning chances for player
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param cube cube value
#' @param met match equity table
#'
#' @return double
#' @export
#'
emg <- function(mwc, a, b, cube, met) {

  win <- mwc(a - cube, b, met)
  lose <- mwc(a, b - cube, met)

  return ((mwc - lose) / (win - lose) * 2 - 1)
}


#' Calculate match winning chance from game winning chances
#'
#' @param pwin game winning chances (cubeless)
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param cube cube value
#' @param met match equity table
#'
#' @return double
#' @export
#'
gwc2mwc <- function(pwin, a, b, cube, met) {
  return (pwin * mwc(a - cube, b, met) + (1 - pwin) * mwc(a, b - cube, met))
}


#'  Look up match winning chances at different scores
#'
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param met match equity table
#'
#' @return double. Match winning chance
#' @export
#'
mwc <- function(a, b, met) {
  if (a <= 0) {
    win <- 1
    } else if (b <= 0) {
    win <- 0
    } else {
    win <- met[a, b]
  }

  return(win)
}


#' Calculate cubeless, gammonless take points at different scores
#'
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param cube cube value (before doubling)
#' @param last_roll treat this as a last roll position; no automatic redouble available
#' @param met match equity table
#'
#' @return double. Take point
#' @export
#'
tp <- function(a, b, cube, met, last_roll = FALSE) {

  if (!last_roll & b <= 2 * cube) {
    multiply <- 4 # We have an automatic recube
  } else {
    multiply <- 2 # The cube value will be double if we take
  }

  drop <- mwc(a, b - cube, met)
  takewin <- mwc(a - multiply * cube, b, met)
  takelose <- mwc(a, b - multiply * cube, met)

  gain <- takewin - drop
  loss <- drop - takelose

  return(loss / (loss + gain))
}



#' Calculate cubeless, take points at different scores, including gammons and backgammons
#'
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param gamfreq_a proportion of player's wins that are gammons
#' @param bgfreq_a proportion of player's wins that are backgammons
#' @param gamfreq_b proportion of player's losses that are gammons
#' @param bgfreq_b proportion of player's losses that are backgammons
#' @param cube cube value (before doubling)
#' @param met match equity table
#'
#' @return double. Take point
#' @export
#'
tp_gammons <- function(a, b, gamfreq_a, bgfreq_a, gamfreq_b, bgfreq_b, cube, met) {

  if ((gamfreq_a + bgfreq_a > 1) | (gamfreq_b + bgfreq_b > 1)) {
    stop("Sum of gammon and backgammon freqencies cannot exceed 1 for either player")
  }

  if (b <= 2 * cube) {
    multiply <- 4 # We have an automatic recube
  } else {
    multiply <- 2 # The cube value will be double if we take
  }

  drop <- mwc(a, b - cube, met)

  takewin <-
    (1 - gamfreq_a - bgfreq_a) * mwc(a - multiply * cube, b, met) + # ordinary win
    (gamfreq_a) * mwc(a - 2 * multiply * cube, b, met) +            # gammon win
    (bgfreq_a) * mwc(a - 3 * multiply * cube, b, met)               # backgammon win

  takelose <-
    (1 - gamfreq_b - bgfreq_b) * mwc(a, b - multiply * cube, met) + # ordinary loss
    (gamfreq_b) * mwc(a, b - 2 * multiply * cube, met) +            # gammon loss
    (bgfreq_b) * mwc(a, b - 3 * multiply * cube, met)               # backgammon loss

  gain <- takewin - drop
  loss <- drop - takelose

  return(loss / (loss + gain))
}


#' Get match equity table from *.met file (used by Extreme Gammon)
#'
#' @param filename file location
#'
#' @return matrix
#'
#' @importFrom rlang .data
#'
#' @export
#'
get_met <- function(filename = "data-raw\\Kazaross XG2.met") {

  top9 <- readr::read_delim(filename, skip = 12, delim = " ", n_max =  9, col_names = as.character(0:25), col_types = list(.default = "c")) %>%
    dplyr::select(-.data$`0`)

  rest <- readr::read_delim(filename, skip = 21, delim = " ", n_max = 16, col_names = as.character(1:25), col_types = list(.default = "c"))

  met <- dplyr::bind_rows(top9, rest) %>%
    dplyr::mutate(`1` = stringr::str_remove(.data$`1`, "^.+=")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    as.matrix()

  rownames(met) <- colnames(met)

  return(met)
}
