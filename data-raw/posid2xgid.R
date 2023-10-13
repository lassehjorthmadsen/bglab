library(tidyverse)
library(stringi)
devtools::load_all()

charset <- c(LETTERS, letters, 0:9, "+", "/")

chr2bin <- function(char, charset) {
  # Convert Base64 character to its 6-bit binary representation
  idx <- which(charset == char) - 1  # -1 because R is 1-indexed
  bin_str <- intToBits(idx)[1:6] %>% as.integer() %>% rev() %>% paste0(collapse = "")
  return(bin_str)
}

id2bin <- function(pos_id, charset) {
  # Convert GNU backgammon position or match id in Base64 to binary string
  big_bin <- str_split(pos_id, "") %>%
    pluck(1) %>%
    map_chr(chr2bin, charset = charset) %>%
    paste0(collapse = "")

  # Convert every 8 bits to their little-endian form
  starts <- seq(1, nchar(big_bin), by = 8)

  big_bin_endian <- map_chr(starts, ~ stri_reverse(substr(big_bin, ., . + 7))) %>%
    paste0(collapse = "") %>%
    str_sub(1,80)

  return(big_bin_endian)
}

posid2xgid <- function(pos_id, charset, turn = "bottom") {
  # Converts binary string with GNU BG position id to XGID position sub-string

  # pos_id to bit string
  pos_id_bin <- id2bin(pos_id, charset)
  split_id <- str_split(pos_id_bin, "") %>% pluck(1)

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
  xg_pos <- coalesce(vec1a, vec2a) %>%
    replace_na("-") %>%
    paste0(collapse = "")

  # xg uses top/bottom perspective; gnubg uses turnower/non-turnowner perspective
  if (turn == "bottom") xg_pos <- chartr("A-Za-z", "a-zA-Z", stri_reverse(xg_pos))

  return(xg_pos)
}


matchid2xgid <- function(match_id, charset) {
  # match_id to bit string
  match_id_bin <- id2bin(match_id, charset)

  starts <- c(1, 5, 7, 8, 9, 12, 13, 14, 16, 19, 22, 37, 52)
  ends <- c(starts[-1] - 1, 66)

  split_id <- map2_chr(starts, ends, ~ substr(match_id_bin, .x, .y)) %>%
    stri_reverse()

  cube <- split_id[1] %>% strtoi(2)

  cubeowner <- case_when(split_id[2] == "00" ~ -1,
                         split_id[2] == "01" ~ 1,
                         split_id[2] == "11" ~ 0)

  diceowner <- case_when(split_id[3] == "0" ~ 1,
                         split_id[3] == "1" ~ -1)

  crawford <- split_id[4] %>% strtoi(2)

  gamestate <- case_when(split_id[5] == "000" ~ "No game started",
                         split_id[5] == "001" ~ "Playing a game",
                         split_id[5] == "010" ~ "Game over",
                         split_id[5] == "011" ~ "Resigned",
                         split_id[5] == "100" ~ "Droped")

  turnowner <- split_id[6] %>% strtoi(2) %>% `*`(2) %>% `-`(1)

  double <- split_id[7] %>% strtoi(2)

  resign <- case_when(split_id[8] == "00" ~ "No resignation",
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


gnuid2xgid <- function(pos_id, match_id, charset) {

  mid <- matchid2xgid(match_id, charset)

  if ((str_split(mid, ":")[[1]][[4]] == "1" & str_split(mid, ":")[[1]][[5]] != "D") |
      (str_split(mid, ":")[[1]][[4]] == "-1" & str_split(mid, ":")[[1]][[5]] == "D")
      ) {
    turn <- "bottom"} else {
      turn <- "top"
    }

  pid <- posid2xgid(pos_id, charset, turn = turn)
  xgid <- paste0("XGID=", pid, mid)

  return(xgid)
}


# Random spot-checks:
for (i in seq(50)) {
  cat("\014")
  temp <- bgmoves %>% slice_sample(n = 1)

  cat("File: ", temp$file, "\n",
      "Move: ", temp$move_no, "\n",
      "Position id: ", temp$pos_id, "\n",
      "Match id: ", temp$match_id, "\n",
      "Match to: ", temp$length, "\n",
      temp$board, "\n",
      temp$cube_eq, "\n",
      temp$move_eq, "\n",
      "Checker play error: ", temp$move_err, "\n",
      "Cube action error: ", temp$cube_err, "\n",
      sep = "")

  xgid <- gnuid2xgid(temp$pos_id, temp$match_id, charset)
  print(ggboard(xgid))

  readline(prompt="Press [enter] to continue")
}


# loop through one random game:
random_file <- bgmoves %>% slice_sample(n = 1) %>% pull(file)
random_game <- bgmoves %>% filter(file == random_file)
random_game <- bgmoves %>% filter(file == "match1105425_003.txt")

for (i in (1:nrow(random_game))) {
  temp <- random_game %>% slice(i)

  cat("File: ", temp$file, "\n",
      "Move: ", temp$move_no, "\n",
      "Position id: ", temp$pos_id, "\n",
      "Match id: ", temp$match_id, "\n",
      "Match to: ", temp$length, "\n",
      temp$board, "\n",
      temp$cube_eq, "\n",
      temp$move_eq, "\n",
      "Checker play error: ", temp$move_err, "\n",
      "Cube action error: ", temp$cube_err, "\n",
      sep = "")

  xgid <- gnuid2xgid(temp$pos_id, temp$match_id, charset)
  print(ggboard(xgid))

  readline(prompt="Press [enter] to continue")
}


# loop through all play types
examples <- bgmoves %>%
  group_by(play, turn == "lasse") %>%
  slice_sample(n = 1) %>%
  ungroup()

for (i in (1:nrow(examples))) {
  temp <- examples %>% slice(i)
  cat("\014")

  cat("File: ", temp$file, "\n",
      "Move: ", temp$move_no, "\n",
      "Position id: ", temp$pos_id, "\n",
      "Match id: ", temp$match_id, "\n",
      "Match to: ", temp$length, "\n",
      temp$board, "\n",
      temp$cube_eq, "\n",
      temp$move_eq, "\n",
      "Checker play error: ", temp$move_err, "\n",
      "Cube action error: ", temp$cube_err, "\n",
      sep = "")

  xgid <- gnuid2xgid(temp$pos_id, temp$match_id, charset)
  print(ggboard(xgid))

  readline(prompt="Press [enter] to continue")
}


# Can we calculate xgid for all gnu ids? Seems so.
ids <- map2(bgmoves$pos_id, bgmoves$match_id, gnuid2xgid, charset)
ids %>% map_int(nchar) %>% table()
