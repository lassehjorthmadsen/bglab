# This is to create the bgmoves dataset included as `bglab::bgmoves`
# Using the function `bglab::txt2df()`

library(tidyverse)
devtools::load_all()

# Get files
file_paths <- c(
  "data-raw\\lasse\\analyzed\\4-ply",
  "data-raw\\lasse\\analyzed\\2-ply",
  "data-raw\\Llabba\\analyzed\\4-ply"
  )

files <- file_paths |> 
  map_dfr(\(x) bind_cols(
    tibble("path" = list.files(x,  pattern = "*.txt", full.names = TRUE)),
    tibble("file" = list.files(x,  pattern = "*.txt", full.names = FALSE))
    )) |> 
  mutate(ply = str_extract(path, "\\d-ply")) |>
  # In case of same position analyzes at several plies, keep the higher ply only.
  # Relies on higher plies being first in the data frame, so order in file_paths is important.
  distinct(file, .keep_all = TRUE) 

# Parse files
bgmoves2 <- txt2df(files$path)
usethis::use_data(bgmoves, overwrite = TRUE)

########################################
# Checks of parsing quality
########################################

# Do we have the right file(s)? YES (although two files have no match and are not included)
setdiff(basename(files$path), unique(bgmoves$file))

# Are match length and score as expected? YES
# Note that unlimited games show up as matches to e.g. 8, 16
# Also: An issue with NA in length for 30 files. 
bgmoves %>% count(length)
bgmoves %>% mutate(okay = score1 < length & score2 < length) %>% count(okay)

# Are values for Crawford games consistent with score? YES
bgmoves %>% count(crawford, (length - score1) == 1 | (length - score2) == 1)

# Are both player's scores and the match length constant within each game? YES
bgmoves %>%
  group_by(file) %>%
  summarise(across(c(length, score1, score2), ~ min(.x) - max(.x))) %>%
  select(-1) %>%
  colSums()

# Names for both players are populated? YES
bgmoves %>% count(player2, player1)
bgmoves %>% count(is.na(player1), is.na(player2))

# Inspect all possible plays. Looks good.
bgmoves %>% count(play)

# Is a double always followed by a take or a pass? 
# No quite, some issues in apparently messed up files like raw/match2900236.txt
bgmoves %>% mutate(next_play = lead(play, 1)) %>%
  filter(play == "Doubles") %>%
  select(play, next_play) %>%
  count(play, next_play)

bgmoves %>% mutate(next_play = lead(play, 1)) %>%
  filter(play == "Doubles", next_play == "Rolls")

# Inspect all possible "proper cube actions". Looks good.
bgmoves %>% count(proper_ca)

# Does the cube mistake flag agree with "proper_ca" and "play" YES
bgmoves %>% count(mistake_ca, play, proper_ca) %>% view("moves")

# Do we have valid dice rolls? YES
bgmoves %>% count(roll, sort = T) %>% view("rolls")

# Are rolls always NA in case of cube decisions? YES
bgmoves %>% count(play, is.na(roll))

# Are rolls never doubles and never NA on the first move? YES
bgmoves %>% filter(move_no == 1) %>% count(roll)

# Are move numbers consecutive integers as long as each game? YES
bgmoves %>%
  group_by(file) %>%
  summarise(rows = n(), moves = max(move_no)) %>%
  mutate(okay = rows == moves) %>%
  count(okay)

# Do each player have at most one less turn than the other? YES
bgmoves %>% group_by(file) %>%
  count(turn) %>%
  summarise(rolls_diff = min(n) - max(n)) %>%
  ungroup() %>%
  count(rolls_diff)

# Are cube errors always negative? Yes.
summary(bgmoves$cube_err)

# Does the mistake flag agree with the cube errors? YES
# Few cases where mistake == TRUE and cube_err == 0, likely because of rounding
bgmoves %>% count(mistake_ca, cube_err < 0)

# Random spot-checks:
for (i in seq(50)) {
  temp <- bgmoves %>% filter(cube_err < 0) %>% slice_sample(n = 1)

  cat("\014",
      "File: ", temp$file, "\n",
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

  print(ggboard(temp$xgid))

  readline(prompt="Press [enter] to continue")
}

# Walk through one random game:
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

  print(ggboard(temp$xgid))

  readline(prompt="Press [enter] to continue")
}


# loop through all play types
examples <- bgmoves %>%
  group_by(turn == "lasse", proper_ca, play) %>%
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

  print(ggboard(temp$xgid))

  readline(prompt="Press [enter] to continue")
}
