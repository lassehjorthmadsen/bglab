library(tidyverse)
library(stringi)
devtools::load_all()

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


pos_id2xg <- function(pos_id, charset, perspective) {
  # Converts binary string with GNU BG position id to XGID position substring

  if (!perspective %in% c(1, 2)) stop("perspective parameter must be either 1 or two")

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

  char_notation <- cbind(c(letters[point[1, 25]], LETTERS[point[1, 1:24]], NA),
                            c(NA, rev(letters[point[2, 1:24]]), LETTERS[point[2, 25]]))

  if (perspective == 1) {
    vec1 <- c(letters[point[1, 25]], LETTERS[point[1, 1:24]], NA)
    vec2 <- c(NA, rev(letters[point[2, 1:24]]), LETTERS[point[2, 25]])
  } else {
    vec1 <- c(letters[point[2, 25]], LETTERS[point[2, 1:24]], NA)
    vec2 <- c(NA, rev(letters[point[1, 1:24]]), LETTERS[point[1, 25]])
  }

xg_pos <- coalesce(vec1, vec2) %>%
  replace_na("-") %>%
  paste0(collapse = "")

  return(xg_pos)
}


xg <- pos_id2xg("sHPMATDgc/ADIA", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

xg <- pos_id2xg("0PPgAyCwc8wBMA", charset, 2)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

xg <- pos_id2xg("jOfgAFKwecwIQg", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)

# BUG:
xg <- pos_id2xg("33YDAEDbthsAAA", charset, 1)
dummy <- paste0("XGID=", xg, ":0:0:1:52:0:0:3:0:10")
ggboard(dummy)
