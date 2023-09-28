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

pos_id2bin <- function(pos_id, charset) {
  # Convert GNU backgammon position id in Base64 to binary string
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


match_id2bin <- function(match_id, charset) {
  # Convert GNU backgammon position id in Base64 to binary string
  big_bin <- str_split(match_id, "") %>%
    pluck(1) %>%
    map_chr(chr2bin, charset = charset) %>%
    paste0(collapse = "")

  # Convert every 8 bits to their little-endian form
  starts <- seq(1, nchar(big_bin), by = 4)

  big_bin_endian <- map_chr(starts, ~ stri_reverse(substr(big_bin, ., . + 5))) %>%
    paste0(collapse = "") %>%
    str_sub(1,80)

  return(big_bin_endian)
}



pos_id2xg <- function(pos_id, charset) {
  # Converts binary string with GNU BG position id to XGID position sub-string

  # pos_id to bit string
  pos_id_bin <- pos_id2bin(pos_id, charset)
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

  # No. of checkers on each point for player 1:
  vec1 <- c(NA, point[1, ])      # Point 0 must be empty, only player 2 can be here

  # No. of checkers on each point for player 2:
  vec2 <- c(rev(point[2, ]), NA) # Point 25 must be empty, only player 1 can be here

  # Convert to letters, XG-style:
  vec1a <- LETTERS[vec1]
  vec2a <- letters[vec2]

  # We can now safely coalesce, since points can have only one player's checkers:
  xg_pos <- coalesce(vec1a, vec2a) %>%
    replace_na("-") %>%
    paste0(collapse = "")

  return(xg_pos)
}


match_id2xg <- function(match_id, charset) {
  # match_id to bit string
  match_id_bin <- match_id2bin(match_id, charset)
  split_id <- str_split(match_id_bin, "") %>% pluck(1)

  xg_match <- match_id_bin
  return(xg_match)
}

# Example
# For example, assume the score is 2-4 in a 9 point match with player 0 holding a 2-cube, and
# player 1 has just rolled 52. The
# match key for this will be (note that the bit order is reversed below for readability)
# 1000 00 1 0 100 1 0 00 101 010 100100000000000 010000000000000 001000000000000
# In little endian the bytes looks like:
#   01000001 10001001 00101010 00000001 00100000 00000000 00100000 00000000 00

match_id <- "QYkqASAAIAAA"
match_id2xg(match_id, charset)

check <- "010000011000100100101010000000010010000000000000001000000000000000"


# Spot checks:
xg <- pos_id2xg("sHPMATDgc/ADIA", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

xg <- pos_id2xg("0PPgAyCwc8wBMA", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

xg <- pos_id2xg("jOfgAFKwecwIQg", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

xg <- pos_id2xg("33YDAEDbthsAAA", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

xg <- pos_id2xg("2+4OAADfdgMAQA", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

xg <- pos_id2xg("bQEAgG+7EQAAAA", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

xg <- pos_id2xg("X90AAKAKAAAAAA", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)
