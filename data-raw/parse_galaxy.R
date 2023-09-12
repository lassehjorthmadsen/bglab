library(tidyverse)
library(stringr)

file_path <- "c:\\Users\\LMDN\\AppData\\Local\\gnubg\\scripts\\output\\match1486073_analyzed.txt"
file_path <- "c:\\Users\\lasse\\Dropbox\\Backgammon\\Matches\\Galaxy matches\\analyzed\\match807838_analyzed.txt"

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

  df_list <- list()

  # Initialize vars
  no_pos   <- length(positions)
  pos_id   <- character(length = no_pos)
  match_id <- character(length = no_pos)
  decision <- character(length = no_pos)
  turn     <- character(length = no_pos)
  cube_eq  <- character(length = no_pos)
  move_eq  <- character(length = no_pos)
  board    <- character(length = no_pos)
  roll     <- character(length = no_pos)

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

    # Extract decision type, turn and possibly roll from the first line
    # CAN WE RELY ON THIS INFORMATION ALWAYS BEING IN LINE 20?
    decision[p] <- str_extract(positions[[p]][20], "moves|doubles|accepts|passes|resigns")
    turn[p] <- str_extract(positions[[p]][20], "\\*\\s\\w+") %>% str_remove("\\* ")

    # Extract board
    board_lines <- str_detect(positions[[p]], "^(\\s\\+|\\s\\||v\\||Pip)")
    board[p] <- positions[[p]][board_lines] %>% paste(collapse = "\n")

    # Extract cube analysis
    cube_lines <- str_detect(positions[[p]], "^(Cube analysis|[0-9]-ply cube)|Cubeful equities|1\\. No|2\\. Double|3\\. Double|Proper")
    cube_eq[p] <- positions[[p]][cube_lines] %>% paste(collapse = "\n")
    if (cube_eq[p] == "") cube_eq[p] <- NA

    # Extract move analysis
    move_lines <- str_detect(positions[[p]], "^\\s+\\d|^\\*\\s+\\d")
    move_eq[p] <- positions[[p]][move_lines] %>% paste(collapse = "\n")
    if (move_eq[p] == "") move_eq[p] <- NA
    move_eq[p] %>% cat()

    # ... (Add your code here for the other tasks)


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
      decision = decision,
      turn = turn,
      roll = roll,
      board = board,
      cube_eq = cube_eq,
      move_eq = move_eq
    )

  # 7. Clean the dataframe
  # ... (Add your code here)

  return(df)
}

# Example usage:
df <- parse_file(file_path)
print(df)
