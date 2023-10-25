#' Scale match winning chance to equivalent to money game
#'
#' @param mwc current match winning chances for player
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param cube cube value
#' @param met match equity table
#'
#' @return double
#'
#' @examples
#' met <- get_met()
#' # If I have 0.7 to win the match at 3-away, 5-away,
#' # how good is this on an money game scale?
#' emg(0.7, 3, 5, 1, met)
#'
#' @export
#'
emg <- function(mwc, a, b, cube, met) {

  win <- mwc(a - cube, b, met)
  lose <- mwc(a, b - cube, met)

  return ((mwc - lose) / (win - lose) * 2 - 1)
}


#' Calculate match winning chance from game winning chances
#'
#' @param pwin game winning chances (cubeless)
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param cube cube value
#' @param met match equity table
#'
#' @return double
#'
#' @examples
#' met <- get_met()
#' gwc2mwc(0.5, 3, 5, 1, met)
#'
#' @export
#'
gwc2mwc <- function(pwin, a, b, cube, met) {
  return (pwin * mwc(a - cube, b, met) + (1 - pwin) * mwc(a, b - cube, met))
}

#'  Look up match winning chances at different scores
#'
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param met match equity table
#'
#' @return double. Match winning chance
#'
#' @examples
#' met <- get_met() # Get the default Kazaross XG2 table
#' mwc(3, 5, met)   # Match winning chance at 3-away, 5-away
#'
#' @export
#'
mwc <- function(a, b, met) {
  if (a <= 0) {
    win <- 1
    } else if (b <= 0) {
    win <- 0
    } else {
    win <- met[a, b]
  }

  return(win)
}


#' Calculate cubeless, gammonless take points at different scores
#'
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param cube cube value (before doubling)
#' @param last_roll treat this as a last roll position; no automatic redouble available
#' @param met match equity table
#'
#' @return double. Take point
#'
#' @examples
#' met <- get_met()
#' tp(3, 5, 1, met)
#'
#' @export
#'
tp <- function(a, b, cube, met, last_roll = FALSE) {

  if (!last_roll & b <= 2 * cube) {
    multiply <- 4 # We have an automatic recube
  } else {
    multiply <- 2 # The cube value will be double if we take
  }

  drop <- mwc(a, b - cube, met)
  takewin <- mwc(a - multiply * cube, b, met)
  takelose <- mwc(a, b - multiply * cube, met)

  gain <- takewin - drop
  risk <- drop - takelose

  return(risk / (risk + gain))
}


#' Calculate cubeless, take points at different scores, including gammons and backgammons
#'
#' @param a number of points that player needs
#' @param b number of points that opponent needs
#' @param probs numeric vector of length 6, representing outcome
#' probabilities (must always sum to 1 or 100)
#' @param cube cube value (before doubling)
#' @param met match equity table
#'
#' @return double. Take point
#' @export
#'
tp_gammons <- function(x, y, probs, cube, met) {

  probs <- check_probs(probs)

  if (y <= 2 * cube) {
    auto <- 2 # We have an automatic recube
  } else {
    auto <- 1 # We do not have an automatic recube
  }

  D <- mwc(x, y - cube, met)                # drop, lose cube value

  outcomes <- c(mwc(x - 2 * cube * auto, y, met),  # Take, win regular
                mwc(x - 4 * cube * auto, y, met),  # Take, win gammon
                mwc(x - 6 * cube * auto, y, met),  # Take, win backgammon
                mwc(x, y - 2 * cube * auto, met),  # Take, lose regular
                mwc(x, y - 4 * cube * auto, met),  # Take, lose gammon
                mwc(x, y - 6 * cube * auto, met)   # Take, lose backgammon
  )

  expected_outcome <- probs * outcomes

  W <- sum(expected_outcome[1:3]) / sum(probs[1:3])
  L <- sum(expected_outcome[4:6]) / sum(probs[4:6])

  risk <- D - L
  gain <- W - D

  tp_dead <- risk / (risk + gain)

  return(tp_dead)
}


#' Calculate cubeful take points at different scores, as a function of gammons,
#' and backgammons, cube level, cube efficiency, and match equity table
#'
#' @param x number of points that player needs
#' @param y number of points that opponent needs
#' @param probs numeric vector of length 6, representing outcome
#' probabilities (must always sum to 1 or 100)
#' @param cube cube value (before doubling)
#' @param met match equity table
#' @param cube_eff Cube efficiency, defaults to 0.68
#'
#' @return List of take points in different flavors, along with informative
#' metrics from the calculation
#'
#' @export
#'
tp_info <- function(x, y, probs, cube, met, cube_eff = 0.68) {

  probs <- check_probs(probs)
  probs_fliped <- c(probs[4:6], probs[1:3]) # For opponent's perspective

  D <- mwc(x, y - cube, met)                # drop, lose cube value

  outcomes <- c(mwc(x - 2 * cube, y, met),  # Take, win regular
                mwc(x - 4 * cube, y, met),  # Take, win gammon
                mwc(x - 6 * cube, y, met),  # Take, win backgammon
                mwc(x, y - 2 * cube, met),  # Take, lose regular
                mwc(x, y - 4 * cube, met),  # Take, lose gammon
                mwc(x, y - 6 * cube , met)  # Take, lose backgammon
                )

  expected_outcome <- probs * outcomes

  W <- sum(expected_outcome[1:3]) / sum(probs[1:3])
  L <- sum(expected_outcome[4:6]) / sum(probs[4:6])

  risk <- D - L
  gain <- W - D

  tp_dead <- risk / (risk + gain)

  if (tp_dead == 0) {
    tp_live <- 0
    tp_real <- 0
  } else {
    tp_live <- tp_dead * (1 - tp_info(y, x, probs_fliped, 2 * cube, met, cube_eff)["tp_live"])
    tp_real <- cube_eff * tp_live + (1 - cube_eff) * tp_dead
  }

  info <- c(
    "D" = D,
    "W" = W,
    "L" = L,
    "risk" = risk,
    "gain" = gain,
    "tp_dead" = tp_dead,
    "tp_live" = unname(tp_live),
    "tp_real" = unname(tp_real)
  )
  return(info)
}

#' Calculate gammon value for a score and cube level:
#' The gain from winning a gammon instead of single game, divided
#' by the loss from losing a single game instead of winning one.
#'
#' @param x number of points that player needs
#' @param y number of points that opponent needs
#' @param cube cube value
#' @param met match equity table
#'
#' @return a real number, the value of winning a gammon
#' @export
#'
gammon_value <- function(x, y, cube, met) {

  gain <- mwc(x - 2 * cube, y, met) - mwc(x - cube, y, met)
  loss <- mwc(x - cube, y, met) - mwc(x, y - cube, met)

  return(gain/loss)
}


#' Calculate take points for money game, Janowski-style
#'
#' @param probs numeric vector of length 6, representing outcome
#' probabilities (must always sum to 1 or 100)
#' @param x cube-life index, between 0 and 1
#' @return double. Take point
#' @export
#'
tp_money <- function(probs, x = 2/3) {

  probs <- check_probs(probs)

  expected_outcome <- probs * c(1, 2, 3, -1, -2, -3)
  W <- sum(expected_outcome[1:3]) / sum(probs[1:3])
  L <- - sum(expected_outcome[4:6]) / sum(probs[4:6])

  tp = (L - 0.5) / (W + L + 0.5 * x)
  return(tp)
}


#' Calculate equity for money game, Janowski-style
#'
#' @param probs numeric vector of length 6, representing outcome
#' probabilities (must always sum to 1 or 100)
#' @param C Cube position: 0: Center; 1: player; -1: opponent
#' @param p Probability of winning
#' @param x cube-life index, between 0 and 1
#' @return double. Equity
#' @examples
#' probs <- c(31, 4, 0, 47, 17, 1)
#' eq_money(probs = probs, C = 1, p = 0.5)
#'
#' @export
eq_money <- function(probs, C, p, x = 2/3) {

  if (!1 %in% c(-1, 0, 1)) stop("Cube position, C, must be one of -1, 0, 1")

  probs <- check_probs(probs)
  expected_outcome <- probs * c(1, 2, 3, -1, -2, -3)
  W <- sum(expected_outcome[1:3]) / sum(probs[1:3])
  L <- - sum(expected_outcome[4:6]) / sum(probs[4:6])

  eq <- dplyr::case_when(
    C ==  1 ~ p * (W + L + 0.5 * x) - L,
    C == -1 ~ p * (W + L + 0.5 * x) - L - 0.5 * x,
    C ==  0 ~ (4 / (4 - x)) * (p * (W + L + 0.5 * x) - L - 0.25 * x)
  )

  return(eq)
}

check_probs <- function(probs) {

  if (!is.numeric(probs)) stop("probs must be numeric vector")
  if (length(probs) != 6)  stop("probs must have length 6")

  if (any(probs > 1)) probs <- probs / 100 # Convert percentages

  if (abs(1 - sum(probs)) > 1e-7) stop("probs must sum to 1 or 100")

  return (probs)
}

#' Convert outcome distributions from eXtreme Gammon to probabilities
#'
#' Both eXtreme Gammon and GNU Backgammon report estimated outcome
#' probabilities in a cumulative way: The number under "wins" is the
#' probability of winning an ordinary game, a gammon or a backgammon.
#' Sometimes it can be convenient to have discrete probabilities.
#'
#' @param xg_probs numeric vector of length 6, corresponding to the
#' winning chances reported by eXtreme Gammon. Can be percentages or
#' decimal fractions.
#'
#' @return named numeric vector of length 6, representing outcome
#' probabilities (always sum to 1 or 100)
#'
#' @examples
#' # XGID=-a-BaBC-A---eE---c-e----B-:0:0:1:00:0:0:0:0:10
#' # 4-ply winning chances, reported in a cummulative fashion:
#' cum_probs <- c(61.94, 24.09, 1.04, 38.06, 8.54, 0.42)
#' outcome_probs(cum_probs)
#'
#' @export
outcome_probs <- function(xg_probs) {

  if (!(xg_probs[1] + xg_probs[4]) %in% c(1, 100))
    stop("Winning probabilities must sum to either 1 or 100")

  out <- c("win_single" = xg_probs[1] - xg_probs[2],
           "win_gammon" = xg_probs[2] - xg_probs[3],
           "win_bg" = xg_probs[3],
           "lose_single" = xg_probs[4] - xg_probs[5],
           "lose_gammon" = xg_probs[5] - xg_probs[6],
           "lose_bg" = xg_probs[6]
           )

  return(out)
}

#' Put winning probabilities in nice table
#'
#' @param probs numeric vector of length 6, representing outcome
#' probabilities (must sum to 1 or 100)
#' @param margins boolean. Should margin totals be added?
#' Defaults to TRUE
#'
#' @return data.frame with rownames
#'
#' @importFrom stats addmargins
#'
#' @examples
#' # XGID=-a-BaBC-A---eE---c-e----B-:0:0:1:00:0:0:0:0:10
#' # 4-ply winning chances, reported in a cumulative fashion:
#' cum_probs <- c(61.94, 24.09, 1.04, 38.06, 8.54, 0.42)
#' probs <- outcome_probs(cum_probs)
#' probs_table(probs)
#'
#' @export
#'
probs_table <-  function(probs, margins = TRUE) {

  m <- matrix(probs, nrow = 2, byrow = TRUE,
              dimnames = list(c("Player wins", "Opponent wins"),
                              c("Regular", "Gammon", "Backgammon")))

  if (margins) m <- m %>% addmargins()
  tab <- m %>% as.data.frame()

  return(tab)
}


#' Compute take points at a given match score, create a nice table
#' to compare those with money game take points
#'
#' @param x number of points that player needs
#' @param y number of points that opponent needs
#' @param probs numeric vector of length 6, representing outcome
#' probabilities (must sum to 1 or 100)
#' @param cube cube value
#' @param met match equity table
#'
#' @examples
#' met <- get_met()
#' # XGID=-a-BaBC-A---eE---c-e----B-:0:0:1:00:0:0:0:0:10
#' # 4-ply winning chances, reported in a cumulative fashion:
#' cum_probs <- c(61.94, 24.09, 1.04, 38.06, 8.54, 0.42)
#' probs <- outcome_probs(cum_probs)
#' tp_table(3, 5, probs, 1, met)
#'
#' @return data.frame
#'
#' @export
#'
tp_table <-  function(x, y, probs, cube, met) {

  col_names <- c("Cube assumptions",
                 "Money game take point",
                 paste0(x, "-away, ", y, "-away take point"))

  dummy <- c(0.5, 0, 0, 0.5, 0, 0)

  tp_tab <- data.frame(
    col1 = c("Dead cube, no gammons", "Dead cube, gammons", "Both cube and gammons"),
    col2 = c(tp_money(dummy, 0), tp_money(probs, x = 0), tp_money(probs, x = 0.68)),
    col3 = c(tp_info(x, y, dummy, cube, met)["tp_dead"],
                    tp_info(x, y, probs, cube, met)["tp_dead"],
                    tp_info(x, y, probs, cube, met)["tp_real"])
  )

  names(tp_tab) <- col_names

  return(tp_tab)
}

#' Get match equity table from *.met file (used by eXtreme Gammon)
#'
#' @param filename name of *.met file. Defaults to "Kazaross-XG2.met"
#' @return matrix
#' @importFrom rlang .data
#' @export
#'
#'@examples
#'met <- get_met()
#'met[1:5, 1:5]
#'met <- get_met("Woolsey.met")
#'met[1:5, 1:5]

get_met <- function(filename = "Kazaross-XG2.met") {

  met_path <- system.file("extdata", package = "backgammon")
  available_mets <- list.files(path = met_path, pattern = "*.met")
  if (!filename %in% available_mets) stop(paste(filename, "not found.\nAvailable mets:", paste(available_mets, collapse = ", ")))

  path <- system.file(file.path("extdata", filename), package = "backgammon")

  lines <- readr::read_lines(path)
  firstline <- stringr::str_detect(lines, " 1=") %>% which()

  top_lines <- readr::read_delim(path, skip = firstline - 1, delim = " ", n_max =  9, col_names = as.character(0:25), col_types = list(.default = "c")) %>%
    dplyr::select(-.data$`0`) # Use `0` not .data$`0`?
        # Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
        # i Please use `"0"` instead of `.data$0`

  rest <- readr::read_delim(path, skip = firstline + 8, delim = " ", col_names = as.character(1:25), col_types = list(.default = "c"))

  met <- dplyr::bind_rows(top_lines, rest) %>%
    dplyr::select(tidyselect::vars_select_helpers$where(~!all(is.na(.x)))) %>%
    dplyr::mutate(`1` = stringr::str_remove(.data$`1`, "^.+=")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    as.matrix()

  rownames(met) <- colnames(met)

  return(met)
}


#' Parse GNU Backgammon Galaxy analysis files into data frame format
#'
#' @param files character vector with names of *.txt files to parse
#'
#' @return tibble
#' @export
#'
txt2df <- function(files) {
  # Assumes that each files contains file paths to files with
  # a single game from a backgammon match analyzed by GNU Backgammon

  files <- normalizePath(files)
  legal_rolls <- tidyr::expand_grid(x = 1:6, y = 1:6)
  legal_rolls <- paste0(legal_rolls$x, legal_rolls$y)
  big_df <- NULL

  # Loop over all files:
  for (f in seq_along(files)) {

    file = basename(files[f])

    # Read the lines of the file into a vector
    lines <- readr::read_lines(files[f])

    # Extract information on whether this is a Crawford-game
    crawford <- stringr::str_detect(lines[1], "Crawford game")

    # Extract game number, score, match length and player names from the first line
    match_info <- stringr::str_split(lines[1], " |,") %>% unlist()

    game_no <- match_info[4] %>% as.numeric() %>% `+`(1)
    player1 <- match_info[7]
    score1  <- match_info[8] %>% as.numeric()
    player2 <- match_info[10]
    score2  <- match_info[11] %>% as.numeric()
    length  <- match_info[14] %>% as.numeric()

    # Extract date and place of playing from line 4 and 5
    date <- stringr::str_extract(lines[4], "(?<=Date: ).*")
    place <- stringr::str_extract(lines[5], "(?<=Place: ).*")

    # Split file into a list of positions, using "Move number" as separator
    splits <- stringr::str_detect(lines, "Move number") %>% cumsum()
    positions <- split(lines, splits)
    positions <- positions[-1]  # Remove the first element with game metadata

    # Initialize vars
    no_pos     <- length(positions)
    pos_id     <- character(length = no_pos)
    match_id   <- character(length = no_pos)
    move_no    <- integer(length = no_pos)
    play       <- character(length = no_pos) # Play made: Roll/Double/Take, etc.
    turn       <- character(length = no_pos)
    cube_eq    <- character(length = no_pos)
    move_eq    <- character(length = no_pos)
    board      <- character(length = no_pos)
    roll       <- character(length = no_pos)
    proper_ca  <- character(length = no_pos) # Proper cube action: "No double, take" etc.
    mistake_ca <- logical(length = no_pos)   # TRUE if a cube mistake was made (remove later)
    cube_err   <- numeric(length = no_pos)   # Size of cube error (0 if correct action, NA if no cube available)
    move_err   <- numeric(length = no_pos)   # Size of move error (0 if correct move, including forced and no moves)

    # Loop over all positions
    for (p in seq_along(positions)) {

      # Extract move number and roll (if available) from first line
      move_no[p] <- stringr::str_extract(positions[[p]][1], "\\d+") %>% as.integer()
      roll[p] <- stringr::str_sub(positions[[p]][1], -2, -1)
      if (!roll[p] %in% legal_rolls) roll[p] <- NA # If this is a take/pass decision, we have no roll

      # Extract Position ID and Match ID from lines containing "Position ID" and "Match ID"
      pos_id[p] <- stringr::str_extract(positions[[p]][3], "(?<=: ).*")
      match_id[p] <- stringr::str_extract(positions[[p]][4], "(?<=: ).*")

      # Extract play and turn from line 20
      play[p] <- stringr::str_extract(positions[[p]][20], "moves|doubles|accepts|rejects|resigns|cannot")
      turn[p] <- stringr::str_extract(positions[[p]][20], "\\*\\s\\w+") %>% stringr::str_remove("\\* ")

      # Extract board
      board_lines <- stringr::str_detect(positions[[p]], "^(\\s\\+|\\s\\||Pip)|BAR")
      board[p] <- positions[[p]][board_lines] %>% paste(collapse = "\n")

      # Extract cube analysis
      cube_lines <- stringr::str_detect(positions[[p]], "^(Cube analysis|[0-9]-ply cube)|Cubeful equities|^\\s{2}[\\d\\-]|^1\\.\\s|^2\\.\\s|^3\\.\\s|Proper")
      cube_eq[p] <- positions[[p]][cube_lines] %>% paste(collapse = "\n")
      if (cube_eq[p] == "") cube_eq[p] <- NA

      # Extract proper cube action
      proper_line <- stringr::str_detect(positions[[p]], "Proper cube action:")
      proper_text <- positions[[p]][proper_line]
      if (length(proper_text) == 0) proper_text <- NA
      proper_text <- stringr::str_remove(proper_text, "Proper cube action: ")
      proper_text <- stringr::str_remove(proper_text, " \\(.+\\)")
      proper_ca[p] <- proper_text

      # Extract cube error
      mistake_ca[p] <-
        ((stringr::str_detect(proper_ca[p], "pass") & play[p] == "accepts"))   | # Wrong take
        ((stringr::str_detect(proper_ca[p], "take") & play[p] == "rejects"))   | # Wrong pass
        ((stringr::str_detect(proper_ca[p], "No|Too") & play[p] == "doubles")) | # Wrong double
        ((stringr::str_detect(proper_ca[p], "Double|Redouble") &
            stringr::str_detect(play[p], "moves|cannot")))                       # Wrong no double

      potential_error <- positions[[p]][cube_lines][5:7] %>%
        stringr::str_extract("\\(.+\\)$") %>%
        stringr::str_remove_all("\\(|\\)") %>%
        as.numeric()

      potential_error <-
        ifelse(all(is.na(potential_error)), NA, min(potential_error, na.rm = TRUE))

      # Special case if the mistake is a take of a too-good double
      if (stringr::str_starts(proper_ca[p], "Too good") & play[p] == "accepts") {
        potential_error <- cube_eq[p] %>%
          stringr::str_extract("Double, take\\s+\\-\\d\\.\\d+") %>%
          stringr::str_remove("Double, take\\s+") %>%
          as.numeric() %>%
          `+`(1)
        }

      cube_err[p] <- mistake_ca[p] * potential_error

      # Extract move analysis
      move_lines <- stringr::str_detect(positions[[p]], "^\\s{5}\\d\\.|^\\*\\s{4}\\d\\.")
      move_eq[p] <- positions[[p]][move_lines] %>% paste(collapse = "\n")

      # The line with the chosen move, beginning with *
      play_line <- stringr::str_detect(positions[[p]], "^\\*\\s{2,4}")

      # Extract move error, if any. (The number in parenthesis at the end)
      error <- positions[[p]][play_line] %>%
        stringr::str_extract("\\(\\-.+\\)$") %>%
        stringr::str_remove_all("[\\(\\)]") %>%
        as.numeric()

      if (move_eq[p] == "") {
        error <- NA                   # No move equities found (this is a cube decision)
      } else {
        if (is.na(error)) error <- 0  # No move error found (correct play was made)
      }

      move_err[p] <- error
    }

    # Put together in nice data frame
    df <- tidyr::tibble(
      file = file,
      place = place,
      date = date,
      player1 = player1,
      player2 = player2,
      game_no = game_no,
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
      mistake_ca = mistake_ca,
      move_err = move_err,
      cube_err = cube_err,
      board = board,
      cube_eq = cube_eq,
      move_eq = move_eq
    )

    big_df <- dplyr::bind_rows(big_df, df)
  }

  # Add xgid
  big_df <- big_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(xgid = gnuid2xgid(pos_id, match_id)) %>%
    dplyr::ungroup()

  # A bit of cleaning
  big_df <- big_df %>%
    dplyr::select(-place) %>%  # Not very useful
    dplyr::mutate(
      play = dplyr::case_match(play, c("moves", "cannot") ~  "Rolls", .default = play),
      play = stringr::str_to_title(play)
      )

  return(big_df)
}
