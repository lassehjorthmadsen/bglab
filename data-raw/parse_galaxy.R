library(tidyverse)
library(stringr)

# Function to parse a single file
parse_file <- function(file_path) {

  file = basename(file_path)

  # Read the lines of the file into a vector
  lines <- read_lines(file_path)

  # 1. Extract score, match length and player names from the first line using regex
  # This assumes that each file contains a single game from a match

  match_info <- str_match(lines[1], "(.*): (.*) ([0-9]+), (.*) ([0-9]+) \\(match to ([0-9]+) points\\)")
  player1 <- match_info[3]
  score1  <- match_info[4] %>% as.numeric()
  player2 <- match_info[5]
  score2  <- match_info[6] %>% as.numeric()
  length  <- match_info[7] %>% as.numeric()

  # 2. Extract date and place from line 4 and 5
  date <- str_extract(lines[4], "(?<=Date: ).*") %>% parse_date(format = "%m/%d/%Y")
  place <- str_extract(lines[5], "(?<=Place: ).*")

  # 3. Split file into a list of positions, using "Move number" as separator
  splits <- str_detect(lines, "Move number") %>% cumsum()
  positions <- split(lines, splits)
  positions <- positions[-1]  # Remove the first element which is empty

  # Initialize vars
  # Data frame where each row will be one or two decisions,
  # a) a no double + move decision
  # b) a move decision only (no cube available)
  # c) a double decision
  # d) a take/pass decision

  no_pos   <- length(positions)
  pos_id   <- character(length = no_pos)
  match_id <- character(length = no_pos)
  action   <- character(length = no_pos) # Action made: roll/double/take/reject/cannot move
  turn     <- character(length = no_pos)
  cube_eq  <- character(length = no_pos)
  move_eq  <- character(length = no_pos)
  board    <- character(length = no_pos)
  roll     <- character(length = no_pos)
  proper   <- character(length = no_pos) # Proper cube action: "No double, take" etc.
  mistake  <- logical(length = no_pos) # TRUE if a cube mistake was made
  cube_err <- numeric(length = no_pos)   # Size of cube error (0 if correct action, NA if no cube available)
  move_err <- numeric(length = no_pos)   # Size of move error (0 if correct move, including forced and no moves)

  legal_rolls <- expand_grid(x = 1:6, y = 1:6)
  legal_rolls <- paste0(legal_rolls$x, legal_rolls$y)

  # 4. For each position:

  for (p in seq_along(positions)) {
    # Extract roll (if available) from first line
    # CAN WE RELY ON THIS INFORMATION ALWAYS BEING IN LINE 1?
    roll[p] <- str_sub(positions[[p]][1], -2, -1)
    if (!roll[p] %in% legal_rolls) roll[p] <- NA

    # Extract Position ID and Match ID from lines containing "Position ID" and "Match ID"
    # CAN WE RELY ON THIS INFORMATION ALWAYS BEING IN LINE 3 AND 4?
    pos_id[p] <- str_extract(positions[[p]][3], "(?<=: ).*")
    match_id[p] <- str_extract(positions[[p]][4], "(?<=: ).*")

    # Extract action and turn from line 20
    # CAN WE RELY ON THIS INFORMATION ALWAYS BEING IN LINE 20?
    action[p] <- str_extract(positions[[p]][20], "moves|doubles|accepts|rejects|resigns|cannot")
    turn[p] <- str_extract(positions[[p]][20], "\\*\\s\\w+") %>% str_remove("\\* ")

    # Extract board
    board_lines <- str_detect(positions[[p]], "^(\\s\\+|\\s\\||v\\||Pip)")
    board[p] <- positions[[p]][board_lines] %>% paste(collapse = "\n")

    # Extract cube analysis
    cube_lines <- str_detect(positions[[p]], "^(Cube analysis|[0-9]-ply cube)|Cubeful equities|1\\. No|2\\. Double|3\\. Double|Proper")
    cube_eq[p] <- positions[[p]][cube_lines] %>% paste(collapse = "\n")
    if (cube_eq[p] == "") cube_eq[p] <- NA

    # Extract proper cube action
    proper_line <- str_detect(positions[[p]], "Proper cube action:")
    proper_text <- positions[[p]][proper_line]
    if (length(proper_text) == 0) proper_text <- NA
    proper_text <- str_remove(proper_text, "Proper cube action: ")
    proper_text <- str_remove(proper_text, " \\(.+\\)")
    proper[p] <- proper_text

    # Extract move analysis
    move_lines <- str_detect(positions[[p]], "^\\s+\\d|^\\*\\s+\\d")
    move_eq[p] <- positions[[p]][move_lines] %>% paste(collapse = "\n")
    if (move_eq[p] == "") move_eq[p] <- NA

    # Extract move error
    play_line <- str_detect(positions[[p]], "^\\*\\s{4}")

    error <- positions[[p]][play_line] %>%
      str_extract("\\(\\-.+\\)$") %>%
      str_remove_all("[\\(\\)]") %>%
      as.numeric()

    if (length(error) == 0) error <- NA

    move_err[p] <- error

    # Extract cube error
    mistake[p] <-
      (proper[p] == "Double, take" & action[p] != "doubles") |
      (proper[p] == "Double, pass" & action[p] != "doubles") |
      (proper[p] == "No double, take" & action[p] == "doubles") |
      (proper[p] == "Too good to double, pass" & action[p] == "doubles") |
      (proper[p] == "Too good to double, take" & action[p] == "doubles")

    cube_error <- cube_txt[p] %>%
      str_extract("\\(\\-.+\\)$") %>%
      str_remove_all("[\\(\\)]") %>%
      as.numeric()

    cube_err[p] <- cube_error
  }

    # Put together in nice data frame
    df <- tibble(
      file = file,
      date = date,
      place = place,
      player1 = player1,
      player2 = player2,
      length = length,
      score1 = score1,
      Score2 = score2,
      pos_id = pos_id,
      match_id = match_id,
      action = action,
      turn = turn,
      roll = roll,
      proper = proper,
      mistake = mistake,
      move_err = move_err,
      cube_err = cube_err,
      board = board,
      cube_eq = cube_eq,
      move_eq = move_eq
    )

  # 7. Clean the dataframe
  # ... (Add your code here)

  return(df)
}

file_path <- "c:\\Users\\LMDN\\AppData\\Local\\gnubg\\scripts\\output\\match1486073_analyzed.txt"
file_path <- "c:\\Users\\lasse\\Dropbox\\Backgammon\\Matches\\Galaxy matches\\analyzed\\match807838_analyzed.txt"

# Example usage:
df <- parse_file(file_path)
df

# Inspect
df %>%
  select(-c(1,2,3,4,5,6,9,10,18)) %>%
  filter(mistake)

# Checks
df %>% count(file) # Do we have the right files
df %>% count(roll) # Do we have (all) valid dice rolls
df %>% summary()

# Do we have same number of doubles and takes + rejects?
