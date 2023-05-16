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
  risk <- drop - takelose

  return(risk / (risk + gain))
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
  risk <- drop - takelose

  return(risk / (risk + gain))
}


#' Calculate cubeful take points for money game, Janowski-style
#'
#' @param W Average cubeless value of games ultimately won
#' @param L Average cubeless value of games ultimately lost
#' @param x cube-life index, between 0 and 1
#' @return double. Take point
#' @export
#'
tp_janowski <- function(W, L, x = 2/3) {
  tp = (L - 0.5) / (W + L + 0.5 * x)
  return(tp)
}


#' Calculate equity for money game, Janowski-style
#'
#' @param outcome_probs numeric vector of length 6, representing outcome
#' probabilities (must always sum to 1 or 100)
#' @param C Cube position: 0: Center; 1: player; -1: opponent
#' @param p Probability of winning
#' @param x cube-life index, between 0 and 1
#' @return double. Equity
#' @examples
#' probs <- c(31, 4, 0, 47, 17, 1)
#' eq_janowski(outcome_probs = probs, C = 1, p = 0.5)
#'
#' @export
eq_janowski <- function(outcome_probs, C, p, x = 2 / 3) {
  outcome_probs <-
    dplyr::case_when(
      sum(outcome_probs) == 1 ~ outcome_probs,
      sum(outcome_probs) == 100 ~ outcome_probs / 100,
      TRUE ~ NaN
    )

  if (any(is.nan(outcome_probs))) stop("outcome_probs must sum to 1 or 100")

  expected_outcome <- outcome_probs * c(1, 2, 3, -1, -2, -3)
  W <- sum(expected_outcome[1:3]) / sum(outcome_probs[1:3])
  L <- - sum(expected_outcome[4:6]) / sum(outcome_probs[4:6])

  eq <- dplyr::case_when(
    C ==  1 ~ p * (W + L + 0.5 * x) - L,
    C == -1 ~ p * (W + L + 0.5 * x) - L - 0.5 * x,
    C ==  0 ~ (4 / (4 - x)) * (p * (W + L + 0.5 * x) - L - 0.25 * x),
    TRUE ~ NaN
  )

  if (is.nan(eq)) stop("Cube position, C, must be one of -1, 0, 1")
  return(eq)
}


#' Convert outcome distributions from eXtreme Gammon to probabilities
#'
#' @param xg_probs numeric vector of length 6, corresponding to the
#' winning chances reported by eXtreme Gammon. Can be percentages or
#' decimal fractions.
#'
#' @return named numeric vector of length 6, representing outcome
#' probabilities (always sum to 1 or 100)
#'
#' @examples
#' # XGID=-a-BaBC-A---eE---c-e----B-:0:0:1:00:0:0:0:0:10
#' # 4-ply winning chances:
#' outcome_probs(c(61.94, 24.09, 1.04, 38.06, 8.54, 0.42))
#'
#' @export
outcome_probs <- function(xg_probs) {

  if (!(xg_probs[1] + xg_probs[4]) %in% c(1, 100))
    stop("Winning probabilities must sum to either 1 or 100")

  out <- c("win_single" = xg_probs[1] - xg_probs[2],
           "win_gammon" = xg_probs[2] - xg_probs[3],
           "win_bg" = xg_probs[3],
           "lose_single" = xg_probs[4] - xg_probs[5],
           "lose_gammon" = xg_probs[5] - xg_probs[6],
           "lose_bg" = xg_probs[6]
           )

  return(out)
}

#' Get match equity table from *.met file (used by Extreme Gammon)
#'
#' @param filename file location
#' @return matrix
#' @importFrom rlang .data
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
