library(tidyverse)
library(stringr)

file_path <- "c:\\Users\\LMDN\\AppData\\Local\\gnubg\\scripts\\output\\match1486073_analyzed.txt"

# Function to parse a single file
parse_file <- function(file_path) {

  # Read the lines of the file into a vector
  lines <- read_lines(file_path)

  # 1. Extract score, match length and player names from the first line using regex
  match_info <- str_match(lines[1], "(.*): (.*) ([0-9]+), (.*) ([0-9]+) \\(match to ([0-9]+) points\\)")
  player1 <- match_info[3]
  score1 <- match_info[4] %>% as.numeric()
  player2 <- match_info[5]
  score2 <- match_info[6] %>% as.numeric()
  match_length <- match_info[7] %>% as.numeric()

  # 2. Extract date and place from line 4 and 5
  date <- str_extract(lines[4], "(?<=Date: ).*") %>% parse_date(format = "%d/%m/%Y")
  place <- str_extract(lines[5], "(?<=Place: ).*")

  # 3. Split file into a list of positions, using "Move number" as separator
  splits <- str_detect(lines, "Move number") %>% cumsum()
  positions <- split(lines, splits)
  positions <- positions[-1]  # Remove the first element which is empty

  df_list <- list()

  # 4. For each position:
  for (p in seq_along(positions)) {
    # position_lines <- str_split(position, "\n")[[1]]

    # Extract Position ID and Match ID from lines containing "Position ID" and "Match ID"
    # CAN WE RELY ON THIS INFORMATION ALWAYS BEING IN LINE 3 AND 4?
    position_id[p] <- str_extract(positions[[p]][3], "(?<=: ).*")
    match_id[p] <- str_extract(positions[[p]][4], "(?<=: ).*")

    # Extract decision type, turn and possibly roll from the first line
    # CAN WE RELY ON THIS INFORMATION ALWAYS BEING IN LINE 20?
    decision[p] <- str_extract(positions[[p]][20], "moves|doubles|accepts|passes|resigns")
    turn[p] <- str_extract(positions[[p]][20], "\\*\\s\\w+") %>% str_remove("\\* ")

    # Other tasks like extracting decision, board text, etc.
    # Note: You may need to adjust the regex patterns and string manipulations

    # ... (Add your code here for the other tasks)

  }

    # Create a data frame for this position
    position_df <- data.frame(
      Player1 = player1,
      Player2 = player2,
      MatchLength = match_length,
      MatchDate = date,
      MatchPlace = place,
      Score1 = score1,
      Score2 = score2,
      PositionID = position_id,
      MatchID = match_id,
      DecisionType = decision_type,
      Turn = turn,
      Roll = roll,
      stringsAsFactors = FALSE
    )

    df_list[[length(df_list) + 1]] <- position_df


  # 5. Unlist all the positions into a dataframe
  df <- bind_rows(df_list)

  # 6. Pivot longer, so each row becomes two, if it is both a checker play and a cube decision
  # ... (Add your code here)

  # 7. Clean the dataframe
  # ... (Add your code here)

  return(df)
}

# Example usage:
file_path <- normalizePath("C:/path/to/your/file/match1486073_analyzed.txt", winslash = "/")
df <- parse_file(file_path)
print(df)
