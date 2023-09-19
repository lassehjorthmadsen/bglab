# This is to create the bgmoves dataset included as `backgammon::bgmoves`
# Using the function `backgammon::txt2df()`

devtools::load_all()

# Get files
file_path <- "data-raw\\galaxy-matches\\analyzed"
files <- list.files(file_path, pattern = "*.txt", full.names = TRUE)

# Parse files
bgmoves <- txt2df(files)
usethis::use_data(bgmoves, overwrite = TRUE)

########################################
# Checks of parsing quality
########################################

# Do we have the right file(s)? YES
setdiff(basename(files), unique(bgmoves$file))

# Are match length and score as expected? YES
# (But note that unlimited games show up as matches to e.g. 8, 16)
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

# Is a double always followed by a take or a pass? YES
bgmoves %>% mutate(next_play = lead(play, 1)) %>%
  filter(play == "Doubles") %>%
  select(play, next_play) %>%
  count(play, next_play)

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

# Walk-through one random game
game <- unique(bgmoves$file) %>% sample(1)
temp <- bgmoves %>% filter(file == game)

for (i in 1:nrow(temp)) {
  if (i == 1) cat("File: ", game, "\n")
  cat("Move: ", temp$move_no[i], "\n")
  cat(temp$board[i], "\n\n")
  cat(temp$cube_eq[i], "\n\n")
  cat(temp$move_eq[i], "\n\n")
  cat("Checker play error: ", temp$move_err[i], "\n")
  cat("Cube action error: ", temp$cube_err[i], "\n")
  readline(prompt="Press [enter] to continue")
}

# Random spot-checks:
for (i in seq(10)) {
  temp <- bgmoves %>% filter(cube_err < 0) %>% slice_sample(n = 1)

  cat("File: ", temp$file, "\n",
      "Move: ", temp$move_no, "\n",
      "Match to:", temp$length, "\n",
      temp$board, "\n",
      temp$cube_eq, "\n",
      temp$move_eq, "\n",
      "Checker play error: ", temp$move_err, "\n",
      "Cube action error: ", temp$cube_err, "\n",
      sep = "")

  readline(prompt="Press [enter] to continue")
}
