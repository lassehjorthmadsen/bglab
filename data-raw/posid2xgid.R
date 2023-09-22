library(bitops)
library(base64enc)
library(tidyverse)
library(stringi)

# 00000111 11001110 00001111 10000000 00001100 00000111 11001110 00001111 10000000 00001100

# Starting point example
id <- "00000111110011100000111110000000000011000000011111001110000011111000000000001100"
pad <- rep("0", 84 - nchar(id)) %>% paste0(collapse = "")
charset <- c(LETTERS, letters, 0:9, "+", "/")

starts8 <- seq(1, 80, 8)
starts6 <- seq(1, 80, 6)

id_chars <- id %>%
  str_sub(starts8, starts8 + 7) %>%
  stri_reverse() %>%
  paste0(collapse = "") %>%
  paste0(pad, collapse = "") %>%
  str_sub(starts6, starts6 + 5) %>%
  strtoi(base = 2) %>%
  map_chr(~ charset[.x + 1]) %>%
  paste0(collapse = "")

id_chars
id_chars == "4HPwATDgc/ABMA"

# Reverse engineer
id_chars <- "4HPwATDgc/ABMA"

id_int <- id_chars %>%
  str_split("") %>%
  pluck(1) %>%
  map_int(~ which(.x == charset))

chr2bin <- function(char, charset) {
  # Convert Base64 character to its 6-bit binary representation

  idx <- which(charset == char) - 1  # -1 because R is 1-indexed
  bin_str <- intToBits(idx)[1:6] %>% as.integer() %>% rev() %>% paste0(collapse = "")
  return(bin_str)
}


posid2bin <- function(pos_id, charset) {
  # From GNU backgammon position id in Base64  to binary string

  big_bin <- str_split(pos_id, "") %>%
    pluck(1) %>%
    map_chr(chr2bin, charset = charset) %>%
    paste0(collapse = "")

  # Convert every 8 bits to their little-endian form
  starts <- seq(1, nchar(big_bin), by = 8)

  big_bin_endian <- map_chr(starts, ~ stri_reverse(substr(big_bin, ., . + 7))) %>%
    paste0(collapse = "")

  return(little_endian_bits)
}

# Test the function
id_char <- "4HPwATDgc/ABMA"
posid2bin(id_char, charset)
id

