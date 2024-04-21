#' Create diagram from eXtreme Gammon id
#'
#' Generate a `ggplot2::ggplot()` object containing a backgammon diagram
#' from an eXtreme Gammon id string
#'
#' @param xgid character
#' @param bearoff character. Side to bear off from. Either "right" (default) or "left"
#' @param scheme name of color scheme, defaults to "bw"
#'
#' @return ggplot object
#'
#' @examples
#' # Starting position:
#' ggboard("XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:3:0:10")
#'
#' # Middle game position, match to 11, bottom player owns cube
#' ggboard("XGID=-b--BBC-C---cC---cBbc-b---:1:1:1:00:0:0:0:11:10")
#'
#' # Same position, bottom player to play 51:
#' ggboard("XGID=-b--BBC-C---cC---cBbc-b---:1:1:1:51:0:0:0:11:10")
#'
#' # Position with multiple checkers off and one on the bar:
#' ggboard("XGID=aFDaA--------------a-Acbb-:1:-1:1:42:3:0:0:7:10")
#'
#' # Same position, bear off at the left:
#' ggboard("XGID=aFDaA--------------a-Acbb-:1:-1:1:42:3:0:0:7:10", "left")
#'
#' # Both sides have several points with excess checkers:
#' ggboard("XGID=e----FI------------fd-----:3:-1:1:52:0:0:3:0:10")
#'
#' # Opening game, match to 3, Crawford:
#' ggboard("XGID=-b---BD-B---cE--abbe----B-:0:0:1:44:2:0:1:3:10")
#'
#' # Moneygame with beaver and Jacoby rule (Kauder paradox)
#' ggboard("XGID=-BBBBBC------A--caacbbba-A:0:0:1:00:0:0:3:0:10")
#'
#' # ggboard() returns a ggplot object; you can add title and more
#' id <- "XGID=-a--BBCBB--A-C----bbdbb-b-:1:1:1:00:2:0:0:5:10"
#'
#' ggboard(id) +
#' ggplot2::labs(title = "Should White double, should Black take?",
#'               subtitle = "This one is tough",
#'               caption = id)
#'
#' # All checkers off, just to see how that looks like:
#' ggboard("XGID=--------------------------:1:-1:1:00:3:0:0:7:10")
#'
#' @importFrom ggforce geom_circle
#' @importFrom stringi stri_reverse
#'
#' @export
ggboard <- function(xgid, bearoff = "right", scheme = "bw") {

  if (!scheme %in% names(color_schemes)) stop("Unknown color scheme")
  if (nchar(xgid) < 50) stop("xgid string does not contain at least 50 characters")

  color_scheme <- color_schemes %>% purrr::pluck(scheme)

  position <- ggplot2::ggplot() +
    show_board(color_scheme$board_fill, color_scheme$board_border) +
    show_points() +
    show_bar(color_scheme$bar_fill, color_scheme$board_border) +
    # show_tray() +
    show_numbers(bearoff) +
    show_checkers(xgid, bearoff) +
    show_excess_checkers(xgid, bearoff) +
    show_off_checkers(xgid) +
    show_cube(xgid) +
    show_cube_value(xgid) +
    show_game_info(xgid) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_manual(
      values = c("odd" = color_scheme$odd_points_fill,
                 "even" = color_scheme$even_points_fill,
                 "top" = color_scheme$top_checker_fill,
                 "bottom" = color_scheme$bottom_checker_fill,
                 "board" = color_scheme$board_fill)
      ) +
    ggplot2::scale_color_manual(
      values = c("top" = color_scheme$top_checker_off_border,
                 "bottom" = color_scheme$bottom_checker_off_border)
      ) +
    ggplot2::theme_void(base_size = 20) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, margin = ggplot2::margin(10, 0, 0, 0)),
                   plot.subtitle = ggplot2::element_text(size = 9, margin = ggplot2::margin(5, 0, 5, 0)),
                   plot.caption = ggplot2::element_text(size = 7, margin = ggplot2::margin(5, 0, 0, 0)))

  return(position)
}


show_points <- function() {
  x <- 0:12 %>% purrr::map(~ rep(0:2/26, 2) + .x * 2/26) %>% unlist()
  y <- rep(c(0, 5/11, 0, 1, 6/11, 1), 13) * board_ratio
  fill <- rep(c(rep("odd", 3), rep("even", 3), rep("even", 3), rep("odd", 3)), 3)
  fill <- c(fill, rep("odd", 6), fill)
  group <- 1:26 %>% purrr::map(rep, 3) %>% unlist()

  triangles <- dplyr::tibble(x = x, y = y, fill = fill, group = group)

  ggplot2::geom_polygon(data = triangles,
                          ggplot2::aes(x = x, y = y, fill = fill, group = group),
                 show.legend = F, color = "black", size = 0.2)
}


show_board <- function(fill, color) {
  ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 1, ymin = 0, ymax = board_ratio),
            fill = fill, color = color, size = 0.2)
}


show_bar <- function(fill, color) {
  ggplot2::geom_rect(ggplot2::aes(xmin = 6/13, xmax = 7/13, ymin = 0, ymax = board_ratio),
            fill = fill, color = color, size = 0.2, inherit.aes = F)
}

show_tray <- function() {
  ggplot2::geom_rect(ggplot2::aes(xmin = 13/13, xmax = 14/13, ymin = 0, ymax = board_ratio),
                     fill = NA, colour = "black", size = 0.2)
}


show_cube <- function(xgid) {
  cube_size <- 2 * checker_radius
  cube_position <- stringr::str_split(xgid, ":")[[1]][3] %>% as.numeric()

  y_position <- (10/22 + cube_position * - 10/22) * board_ratio

  ggplot2::geom_rect(ggplot2::aes(xmin = -0.01 - cube_size,
                                  xmax = -0.01,
                                  ymin = y_position,
                                  ymax = y_position + cube_size),
                     color = "black",
                     size = 0.2,
                     fill = "white",
                     linejoin = "round")
}


show_cube_value <- function(xgid) {
  cube_value <- stringr::str_split(xgid, ":")[[1]][2] %>% as.numeric()
  cube_value <- 2^cube_value
  if (cube_value == 1) cube_value <- 64

  cube_position <- stringr::str_split(xgid, ":")[[1]][3] %>% as.numeric()

  y_position <- 10/22 * board_ratio + cube_position * - 10/22 * board_ratio

  ggplot2::geom_text(ggplot2::aes(x = -0.05, y = y_position, label = cube_value),
                     size = ggplot2::rel(4.5),  color = "black", vjust = 0,
                     nudge_y = 0.016, nudge_x = 0.004)
}


show_numbers <- function(bearoff = "right") {

  x <- rep(0:12  * 2/26 + 1/26, 2)
  y <- c(rep(-0.02, 13), rep(board_ratio + 0.03, 13))

  if (bearoff == "left") {
    label <- as.character(c(1:6, "", 7:12, 24:19, "", 18:13))
  } else if  (bearoff == "right") {
    label = as.character(c(12:7, "", 6:1, 13:18, "", 19:24))
  } else {
    stop("bearoff parameter must be either 'right' or 'left'")
  }

  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = label),
    color = "grey20",
    size = ggplot2::rel(3)
  )
}


show_checkers <-  function(xgid, bearoff = "right") {

  if (bearoff == "left") xgid <- flip_xgid(xgid)
  df <- xgid2df(xgid)
  if (nrow(df) == 0) return(NULL) # No checkers on board
  df <- dplyr::filter(df, !is.na(.data$y))  # Don't draw excess checkers (>5)

  ggforce::geom_circle(data = df, ggplot2::aes(x0 = .data$x, y0 = .data$y, fill = .data$player, r = checker_radius),
                       size = 0.2, show.legend = F)
  }


show_excess_checkers <-  function(xgid, bearoff = "right") {

  if (bearoff == "left") xgid <- flip_xgid(xgid)

  df <- xgid2df(xgid)

  if (nrow(df) == 0) return(NULL) # No checkers on board

  excess <- df %>%
    dplyr::add_count(.data$point, .data$player) %>%
    dplyr::filter(.data$n > 5 | (.data$n > 4 & .data$point %in% c(1, 26)))

  if (nrow(excess) == 0) return(NULL) # No excess checkers found

  # We split into top-half and bottom half; each share y-coordinate
  top_half <- excess %>% dplyr::filter(.data$point > 13)

  if (nrow(top_half) > 0) {
    # For top-half we pick the minimum y-coordinate
    top_half <- top_half %>% dplyr::filter(.data$y == min(.data$y, na.rm = T))
    }

  bottom_half <- excess %>% dplyr::filter(.data$point < 14)

  if (nrow(bottom_half) > 0) {
    # For bottom-half we pick the maximum y-coordinate
    bottom_half <- bottom_half %>% dplyr::filter(.data$y == max(.data$y, na.rm = T))
  }

  excess <- dplyr::bind_rows(top_half, bottom_half)

  ggplot2::geom_text(data = excess, ggplot2::aes(x = .data$x,
                                             y = .data$y,
                                             label = .data$n,
                                             color = .data$player),
                     fontface = "bold",
                     hjust = 0.5, vjust = 0.5,
                     size = ggplot2::rel(3),
                     show.legend = F)
}


show_off_checkers <-  function(xgid, bearoff = "right") {
  df <- xgid2df(xgid)

  if (nrow(df) == 0) {
    # No checkers on board
    checker_count <- c(0, 0)
  } else {
    checker_count <- df %>%
      dplyr::count(player) %>%
      dplyr::pull(.data$n)
  }

  if (sum(checker_count) >= 30) return(NULL) # No checkers off

  bottom_off <- 15 - checker_count[1]
  top_off <- 15 - checker_count[2]

  thickness <- 0.6 * checker_radius  # How thick are the checkers?
  ymax <- board_ratio - thickness

  # x-coords for plotting checkers off
  x <- rep(1.01, 30 - sum(checker_count))

  # y-coords for plotting checkers off
  if (bottom_off > 0) {
    y_bottom <- seq(0, (bottom_off - 1) * thickness, thickness)
  } else {
    y_bottom <- NULL}

  if (top_off > 0) {
    y_top <- seq(ymax, ymax - (top_off - 1) * thickness, -thickness)
  } else {
    y_top <- NULL}

  y <- c(y_bottom, y_top)

  player <- c(rep("bottom", bottom_off), rep("top", top_off))

  #off_checker_border <- c(rep("top", bottom_off), rep("bottom", top_off))

  df <- dplyr::tibble(x = x, y = y, player = player)

  ggplot2::geom_rect(data = df, ggplot2::aes(xmin = .data$x, xmax = .data$x + 1.95 * checker_radius,
                                             ymin = .data$y, ymax = .data$y + thickness,
                                             fill = .data$player,
                                             color = .data$player),
                     linejoin = "round",
                     show.legend = F)
}


show_game_info <-  function(xgid) {
  info <- get_game_info(xgid)
  pip_count <- get_pip_count(xgid)

  pips_bottom <- paste0("Pip count: ", pip_count["pips_bottom"])

  pips_top <- paste0("Pip count: ", pip_count["pips_top"])

  crawford_note <- dplyr::case_when(info["match_length"] != "0" &
                                      info["crawford_jacoby"] == "1" ~ ", Crawford",
                                    TRUE ~ "")

  jacoby_note <- dplyr::case_when(info["match_length"] == "0" &
                                    info["crawford_jacoby"] == "1" ~ ", Jacoby, no beaver",
                                  info["match_length"] == "0" &
                                    info["crawford_jacoby"] == "2" ~ ", no Jacoby, beaver",
                                  info["match_length"] == "0" &
                                    info["crawford_jacoby"] == "3" ~ ", Jacoby and beaver",
                                  TRUE ~ "")

  match_bottom <- dplyr::case_when(info["match_length"] == "0" ~ paste0("Moneygame", jacoby_note),
                            info["match_length"] != "0" ~
                              paste0("Score: ", info["score_bottom"], "/", info["match_length"], crawford_note))

  match_top <- dplyr::case_when(info["match_length"] == "0" ~ "",
                            info["match_length"] != "0" ~ paste0("Score: ", info["score_top"], "/", info["match_length"]))

  turn <- dplyr::case_when(info["turn"] == "1" ~ "White to play",
                           info["turn"] == "-1" ~ "Black to play")

  action <-  dplyr::case_when(!info["dice"] %in% c("00", "D", "B", "R") ~ paste(turn, info["dice"]),
                              info["dice"] == "00" ~ paste0(turn, ". Cube action?"),
                              info["dice"] == "D" & info["turn"] == "1" ~ "Black doubles. Take or pass?",
                              info["dice"] == "D" & info["turn"] == "-1" ~ "White doubles. Take or pass?",
                              TRUE ~ paste0(turn, ". Beaver? Racoon?"))

  df <- dplyr::tibble(x = c(0, 0, 1, 1, 0.5),
               y = c(-0.08, 0.94, -0.08, 0.94, -0.14),
               hjust = c(0, 0, 1, 1, 0.5),
               fontface = c(rep("plain", 4), "bold"),
               info_text = c(match_bottom, match_top, pips_bottom, pips_top, action))

  ggplot2::geom_text(data = df, ggplot2::aes(x = .data$x, y = .data$y,
                                             label = .data$info_text,
                                             hjust = .data$hjust,
                                             fontface = .data$fontface),
                     size = ggplot2::rel(3), color = "black")
}


xgid2df <- function(xgid) {
  # Init vars to populate
  x <- NULL
  y <- NULL
  player <- NULL
  point <- NULL

  # x-coordinates for each point
  x_coord <- rep(NULL, 26)
  x_coord[2:7]     <- 13:8 * 2/26 - 1/26
  x_coord[8:19]    <- 5:0 * 2/26 + 1/26
  x_coord[14:19]   <- 0:5 * 2/26 + 1/26
  x_coord[20:25]   <- 8:13 * 2/26 - 1/26
  x_coord[c(1,26)] <- 12/26 + 1/26

  # y-coordinates for bottom and top points
  y_coord_bottom <- seq(1/22, 9/22, 2/22) * board_ratio
  y_coord_top <- seq(21/22, 13/22, -2/22) * board_ratio

  # Loop over all 26 points (#1 is our bar, #26 is opp's bar)
  for (i in seq(1, 26)) {
    entry <- substr(xgid, i + 5, i + 5)

    if (entry != "-") {
      player_checkers <- match(entry, LETTERS)
      opponent_checkers <- match(entry, letters)
      no_checkers <- dplyr::coalesce(player_checkers, opponent_checkers)

      if (is.na(no_checkers)) stop(paste("Illegal character found in XGID string: ", entry))

      x_point <- rep(x_coord[i], no_checkers)

      y_point <- dplyr::case_when(i == 1 ~ y_coord_bottom[5:2][1:no_checkers],
                                  i == 26 ~ y_coord_top[5:2][1:no_checkers],
                                  i < 14 ~ y_coord_bottom[1:no_checkers],
                                  i > 13 ~ y_coord_top[1:no_checkers])

      p_point <-  rep(dplyr::if_else(is.na(player_checkers), "top", "bottom"), no_checkers)
      point_no <- rep(i, no_checkers)


      x <-  c(x, x_point)
      y <-  c(y, y_point)
      player <- c(player, p_point)
      point <- c(point, point_no)
    }
  }

  df <- dplyr::tibble(x = x, y = y, player = player, point = point)

  return(df)
}


flip_xgid <- function(xgid) {
  substr(xgid, 7, 18)  <- stringi::stri_reverse(substr(xgid, 7, 18))
  substr(xgid, 19, 30) <- stringi::stri_reverse(substr(xgid, 19, 30))

  return(xgid)
}


get_game_info <- function(xgid) {
  info <- stringr::str_split(xgid, ":")[[1]]
  names(info) <- c("position", "cube_value", "cube_position", "turn", "dice",
                   "score_bottom", "score_top", "crawford_jacoby", "match_length",
                   "max_cube")

  return(info)
}


get_pip_count <- function(xgid) {

  position <- stringr::str_split(xgid, ":")[[1]][1] %>% stringr::str_remove("XGID=")
  pips_bottom <- 0
  pips_top <- 0

  for (i in seq(2, 26)) pips_bottom <- pips_bottom + match(substr(position, i, i), LETTERS, nomatch = 0) * (i - 1)
  for (i in seq(1, 25)) pips_top <- pips_top + match(substr(position, i, i), letters, nomatch = 0) * (26 - i)

  return (c("pips_bottom" = pips_bottom, "pips_top" = pips_top))

}
