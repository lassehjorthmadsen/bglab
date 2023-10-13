#' Convert GNU BG position and match ids to xgid
#'
#' @param pos_id character. GNU Backgammon position id
#' @param match_id character. GNU Backgammon match id
#'
#' @return character
#' @export
#'
#' @examples
#' gnuid2xgid(pos_id = "4HPwATDgc/ABMA", match_id = "8IhuACAACAAE")
#'
gnuid2xgid <- function(pos_id, match_id) {

  mid <- matchid2xgid(match_id)
  mid_split <- stringr::str_split(mid, ":")[[1]]

  if ((mid_split[[4]] == "1" & mid_split[[5]] != "D") |
      (mid_split[[4]] == "-1" & mid_split[[5]] == "D")
  ) {
    turn <- "bottom"} else {
      turn <- "top"
    }

  pid <- posid2xgid(pos_id, turn = turn)
  xgid <- paste0("XGID=", pid, mid)

  return(xgid)
}


chr2bin <- function(char) {
  # Convert Base64 character to its 6-bit binary representation
  idx <- which(gnu_chars == char) - 1  # -1 because R is 1-indexed
  bin_str <- intToBits(idx)[1:6] %>% as.integer() %>% rev() %>% paste0(collapse = "")
  return(bin_str)
}

id2bin <- function(pos_id) {
  # Convert GNU backgammon position or match id in Base64 to binary string
  big_bin <- stringr::str_split(pos_id, "") %>%
    purrr::pluck(1) %>%
    purrr::map_chr(chr2bin) %>%
    paste0(collapse = "")

  # Convert every 8 bits to their little-endian form
  starts <- seq(1, nchar(big_bin), by = 8)

  big_bin_endian <- purrr::map_chr(starts, ~ stringi::stri_reverse(substr(big_bin, ., . + 7))) %>%
    paste0(collapse = "") %>%
    stringr::str_sub(1,80)

  return(big_bin_endian)
}

posid2xgid <- function(pos_id, turn = "bottom") {
  # Converts binary string with GNU BG position id to XGID position sub-string

  # pos_id to bit string
  pos_id_bin <- id2bin(pos_id)
  split_id <- stringr::str_split(pos_id_bin, "") %>% purrr::pluck(1)

  # initialize point matrix with NA
  point <- matrix(rep(NA, 50), nrow = 2, ncol = 25)
  i <- 0

  for (player in c(1, 2)) {
    point_no <- 1
    checkers <- 0

      while (point_no <= 25) {
        i <- i + 1

        if (split_id[i] == "1") {
          checkers <- checkers + 1
          point[player, point_no] <- checkers
          } else {
            point_no <- point_no + 1
            checkers <- 0
          }
      }
  }

  # Board is numbered from 0-25, (index 1-26)  from player 1 perspective.
  # Point O is where player 2 is on the bar, point 25 is where player 1 is on the bar.
  # (Since the bar is the logical 25th point for player 1).

  # No. of checkers on each point for player 1 and 2:
  vec1 <- c(NA, point[1, ])      # Point 0 must be empty, only player 2 can be here
  vec2 <- c(rev(point[2, ]), NA) # Point 25 must be empty, only player 1 can be here

  # Convert to letters, XG-style:
  vec1a <- LETTERS[vec1]
  vec2a <- letters[vec2]

  # We can now safely coalesce, since points can have only one player's checkers:
  xg_pos <- dplyr::coalesce(vec1a, vec2a) %>%
    tidyr::replace_na("-") %>%
    paste0(collapse = "")

  # xg uses top/bottom perspective; gnubg uses turnower/non-turnowner perspective
  if (turn == "bottom") xg_pos <- chartr("A-Za-z", "a-zA-Z", stri_reverse(xg_pos))

  return(xg_pos)
}


matchid2xgid <- function(match_id) {
  # match_id to bit string
  match_id_bin <- id2bin(match_id)

  starts <- c(1, 5, 7, 8, 9, 12, 13, 14, 16, 19, 22, 37, 52)
  ends <- c(starts[-1] - 1, 66)

  split_id <- purrr::map2_chr(starts, ends, ~ substr(match_id_bin, .x, .y)) %>%
    stringi::stri_reverse()

  cube <- split_id[1] %>% strtoi(2)

  cubeowner <- dplyr::case_when(split_id[2] == "00" ~ -1,
                         split_id[2] == "01" ~ 1,
                         split_id[2] == "11" ~ 0)

  diceowner <- dplyr::case_when(split_id[3] == "0" ~ 1,
                         split_id[3] == "1" ~ -1)

  crawford <- split_id[4] %>% strtoi(2)

  gamestate <- dplyr::case_when(split_id[5] == "000" ~ "No game started",
                         split_id[5] == "001" ~ "Playing a game",
                         split_id[5] == "010" ~ "Game over",
                         split_id[5] == "011" ~ "Resigned",
                         split_id[5] == "100" ~ "Droped")

  turnowner <- split_id[6] %>% strtoi(2) %>% `*`(2) %>% `-`(1)

  double <- split_id[7] %>% strtoi(2)

  resign <- dplyr::case_when(split_id[8] == "00" ~ "No resignation",
                      split_id[8] == "01" ~ "Resigns single",
                      split_id[8] == "10" ~ "Resigns gammon",
                      split_id[8] == "11" ~ "Resigns backgmmon",
                      TRUE ~ NA)

  die1 <- split_id[9] %>% strtoi(2)
  die2 <- split_id[10] %>% strtoi(2)
  dice <- paste0(die1, die2)

  if (double == 1) dice <- "D"

  length <- split_id[11] %>% strtoi(2)
  score2 <- split_id[12] %>% strtoi(2)
  score1 <- split_id[13] %>% strtoi(2)

  xg_mat <- paste("", cube, cubeowner, turnowner, dice, score1, score2, crawford, length, "10", sep = ":")

  return(xg_mat)
}
