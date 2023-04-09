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
#' @param pwin
#' @param a
#' @param b
#' @param cube
#' @param met
#'
#' @return
#' @export
#'
gwc2mwc <- function(pwin, a, b, cube, met) {
  return (pwin * mwc(a - cube, b, met) + (1 - pwin) * mwc(a, b - cube, met))
}


#'  Look up match winning chances at different scores
#'
#' @param a
#' @param b
#' @param met
#'
#' @return
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


#' Get match equity table from *.met file (used by Extreme Gammon)
#'
#' @param filename file location
#'
#' @return data frame
#' @export
#'
get_met <- function(filename = "data-raw\\Kazaross XG2.met") {

  top9 <- read_delim(filename, skip = 12, delim = " ", n_max =  9, col_names = as.character(0:25), col_types = list(.default = "c")) %>%
    select(-`0`)

  rest <- read_delim(filename, skip = 21, delim = " ", n_max = 16, col_names = as.character(1:25), col_types = list(.default = "c"))

  met <- bind_rows(top9, rest) %>%
    mutate(`1` = str_remove(`1`, "^.+=")) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()

  rownames(met) <- colnames(met)

  return(met)
}
