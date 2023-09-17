library(tidyverse)
library(stringr)

# Function to parse Galaxy match log files
galaxy2df <- function(files) {
  # This assumes that each file contains a single game from a match

  files <- normalizePath(files)
  legal_rolls <- expand_grid(x = 1:6, y = 1:6)
  legal_rolls <- paste0(legal_rolls$x, legal_rolls$y)
  big_df <- NULL

  # Loop over all files:
  for (f in seq_along(files)) {

    file = basename(files[f])

    # Read the lines of the file into a vector
    lines <- read_lines(files[f])

    # Extract information on whether this is a Crawford-game
    crawford <- str_detect(lines[1], "Crawford game")

    # Extract game number, score, match length and player names from the first line
    match_info <- str_split(lines[1], " |,") %>% unlist()

    game_no <- match_info[4] %>% as.numeric() %>% `+`(1)
    player1 <- match_info[7]
    score1  <- match_info[8] %>% as.numeric()
    player2 <- match_info[10]
    score2  <- match_info[11] %>% as.numeric()
    length  <- match_info[14] %>% as.numeric()

    # Extract date and place of playing from line 4 and 5
    date <- str_extract(lines[4], "(?<=Date: ).*")
    place <- str_extract(lines[5], "(?<=Place: ).*")

    # Split file into a list of positions, using "Move number" as separator
    splits <- str_detect(lines, "Move number") %>% cumsum()
    positions <- split(lines, splits)
    positions <- positions[-1]  # Remove the first element with game metadata

    # Initialize vars
    no_pos    <- length(positions)
    pos_id    <- character(length = no_pos)
    match_id  <- character(length = no_pos)
    move_no   <- integer(length = no_pos)
    play      <- character(length = no_pos) # Play made: Roll/Double/Take, etc.
    turn      <- character(length = no_pos)
    cube_eq   <- character(length = no_pos)
    move_eq   <- character(length = no_pos)
    board     <- character(length = no_pos)
    roll      <- character(length = no_pos)
    proper_ca <- character(length = no_pos) # Proper cube action: "No double, take" etc.
    mistake   <- logical(length = no_pos)   # TRUE if a cube mistake was made (remove later)
    cube_err  <- numeric(length = no_pos)   # Size of cube error (0 if correct action, NA if no cube available)
    move_err  <- numeric(length = no_pos)   # Size of move error (0 if correct move, including forced and no moves)

    # Loop over all positions
    for (p in seq_along(positions)) {

      # Extract move number and roll (if available) from first line
      move_no[p] <- str_extract(positions[[p]][1], "\\d+") %>% as.integer()
      roll[p] <- str_sub(positions[[p]][1], -2, -1)
      if (!roll[p] %in% legal_rolls) roll[p] <- NA # If this is a take/pass decision, we have no roll

      # Extract Position ID and Match ID from lines containing "Position ID" and "Match ID"
      pos_id[p] <- str_extract(positions[[p]][3], "(?<=: ).*")
      match_id[p] <- str_extract(positions[[p]][4], "(?<=: ).*")

      # Extract play and turn from line 20
      play[p] <- str_extract(positions[[p]][20], "moves|doubles|accepts|rejects|resigns|cannot")
      turn[p] <- str_extract(positions[[p]][20], "\\*\\s\\w+") %>% str_remove("\\* ")

      # Extract board
      board_lines <- str_detect(positions[[p]], "^(\\s\\+|\\s\\||v\\||Pip)")
      board[p] <- positions[[p]][board_lines] %>% paste(collapse = "\n")

      # Extract cube analysis
      cube_lines <- str_detect(positions[[p]], "^(Cube analysis|[0-9]-ply cube)|Cubeful equities|^\\s{2}\\d|^1\\.\\s|^2\\.\\s|^3\\.\\s|Proper")
      cube_eq[p] <- positions[[p]][cube_lines] %>% paste(collapse = "\n")
      if (cube_eq[p] == "") cube_eq[p] <- NA

      # Extract proper cube action
      proper_line <- str_detect(positions[[p]], "Proper cube action:")
      proper_text <- positions[[p]][proper_line]
      if (length(proper_text) == 0) proper_text <- NA
      proper_text <- str_remove(proper_text, "Proper cube action: ")
      proper_text <- str_remove(proper_text, " \\(.+\\)")
      proper_ca[p] <- proper_text

      # Extract move analysis
      move_lines <- str_detect(positions[[p]], "^\\s{5}\\d\\.|^\\*\\s{4}\\d\\.")
      move_eq[p] <- positions[[p]][move_lines] %>% paste(collapse = "\n")

      # The line with the chosen move, beginning with *
      play_line <- str_detect(positions[[p]], "^\\*\\s{4}")

      # Extract move error, if any. (The number in parenthesis at the end)
      error <- positions[[p]][play_line] %>%
        str_extract("\\(\\-.+\\)$") %>%
        str_remove_all("[\\(\\)]") %>%
        as.numeric()

      if (move_eq[p] == "") {
        error <- NA                   # No move equities found (this is a cube decision)
      } else {
        if (is.na(error)) error <- 0  # No move error found (correct play was made)
      }

      move_err[p] <- error

      # Extract cube error
      mistake[p] <-
        ((str_detect(proper_ca[p], "pass") & play[p] == "accepts"))   | # Wrong take
        ((str_detect(proper_ca[p], "take") & play[p] == "rejects"))   | # Wrong pass
        ((str_detect(proper_ca[p], "No|Too") & play[p] == "doubles")) | # Wrong double
        ((str_detect(proper_ca[p], "Double|Redouble") &
            str_detect(play[p], "moves|cannot")))                       # Wrong no double

      potential_error <- positions[[p]][cube_lines][4:6] %>%
        str_extract("\\(.+\\)$") %>%
        str_remove_all("\\(|\\)") %>%
        as.numeric()

      potential_error <-
        ifelse(all(is.na(potential_error)), NA, min(potential_error, na.rm = TRUE))

      cube_err[p] <- mistake[p] * potential_error
    }

      # Put together in nice data frame
      df <- tibble(
        file = file,
        place = place,
        date = date,
        game_no = game_no,
        player1 = player1,
        player2 = player2,
        length = length,
        score1 = score1,
        score2 = score2,
        crawford = crawford,
        pos_id = pos_id,
        match_id = match_id,
        move_no = move_no,
        play = play,
        turn = turn,
        roll = roll,
        proper_ca = proper_ca,
        mistake = mistake,
        move_err = move_err,
        cube_err = cube_err,
        board = board,
        cube_eq = cube_eq,
        move_eq = move_eq
      )

    big_df <- bind_rows(big_df, df)
  }

  # A bit of cleaning
  big_df <- big_df %>%
    select(-place) %>%  # Not very useful
    mutate(play = case_match(play, c("moves", "cannot") ~  "Rolls", .default = play),
           play = str_to_title(play))

  return(big_df)
}

file_path <- "data-raw\\galaxy-matches\\analyzed"
files <- list.files(file_path, pattern = "*.txt", full.names = TRUE)

# Parse everything:
bgmoves <- galaxy2df(files)
usethis::use_data(bgmoves, overwrite = TRUE)
devtools::load_all()

# Checks
# Do we have the right file(s)? YES (We miss one that's empty)
bgmoves %>% count(file)
setdiff(basename(files), unique(bgmoves$file))

# Are match length and score as expected? YES
# (But note that unlimited games show up as matches to e.g. 8, 16)
bgmoves %>% count(score1)
bgmoves %>% count(score2)
bgmoves %>% count(length)
bgmoves %>% count(score1, score2, length) %>% view()

# Are values for Crawford games consistent with score? YES
bgmoves %>% count(crawford, (length - score1) == 1 | (length - score2) == 1)

# Are both player's scores and the match length constant within each game? YES
bgmoves %>%
  group_by(file) %>%
  summarise(across(c(length, score1, score2), ~ min(.x) - max(.x))) %>%
  select(-1) %>%
  colSums()

# Names for both players are populated? YES
bgmoves %>% count(player1)
bgmoves %>% count(player2)
bgmoves %>% count(is.na(player1), is.na(player2))

# Inspect all possible plays and "proper cube actions" Looks good.
# Do we have same number of doubles and takes + rejects? YES
bgmoves %>% count(play)
bgmoves %>% count(proper_ca)

# Does the mistake flag agree with "proper_ca" and "play" YES
bgmoves %>% count(mistake, play, proper_ca) %>% view()

# Figure out the one cases of "doubles" and proper_ca = NA
bgmoves %>% filter(is.na(mistake), play == "Doubles", is.na(proper_ca)) %>% view("temp")

# Do we have valid dice rolls? YES
bgmoves %>% count(roll, sort = T) %>% view()

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

# Do each player have about the same no of turns? YES
# (Investigate the one case of diff = -2)
bgmoves %>% group_by(file) %>%
  count(turn) %>%
  summarise(rolls_diff = min(n) - max(n)) %>%
  ungroup() %>%
  count(rolls_diff)

# Do cube errors look right?
# Investigate a few strange cases
bgmoves %>% filter(mistake, cube_err == 0) %>% view()

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
  temp <- bgmoves %>% slice_sample(n = 1)

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
