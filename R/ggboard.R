#' Display a backgammon diagram from an eXtreme Gammon id
#'
#' @param xgid character
#'
#' @return ggplot object
#'
#' @example
#' ggboard("XGID=-b----E-C---eE---c-e----B-:0:0:1:64:0:0:0:7:10")
#'
#' @importFrom ggforce geom_circle
#'
#' @export
ggboard <- function(xgid) {

  position <- empty_board() + show_cube() + show_points()
  return(position)
}


empty_board <- function() {

  x <- 0:12 %>% purrr::map(~ rep(0:2/26, 2) + .x * 2/26) %>% unlist()
  y <- rep(c(0, 5/11, 0, 1, 6/11, 1), 13)
  fill <- rep(c(rep(T, 3), rep(F, 3), rep(F, 3), rep(T, 3)), 3)
  fill <- c(fill, rep(F, 6), fill)
  group <- 1:26 %>% purrr::map(rep, 3) %>% unlist()

  triangles <- dplyr::tibble(x = x, y = y, fill = fill, group = group)

  board <- ggplot2::ggplot(data = NULL) +
    ggplot2::geom_polygon(data = triangles,
                          ggplot2::aes(x = x, y = y, fill = fill, group = group),
                 show.legend = F, color = "darkgrey") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
              fill = NA, colour = "darkgrey", size = 0.5) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 6/13, xmax = 7/13, ymin = 0, ymax = 1),
              fill = "white", colour = "darkgrey", size = 0.5, inherit.aes = F) +
    ggplot2::scale_fill_manual(values = c("darkgrey", "white")) +
    ggplot2::theme_minimal()

  return(board)
}


show_cube <- function(cube = 2, cube_position = -1) {

  cube_value = 2^cube
  if (cube_value == 1) cube_value <-64
  y_position <- -0.5 * cube_position + 0.5

  cube <- ggplot2::geom_label(ggplot2::aes(x = -0.06, y = y_position, label = cube_value),
                     size = 4,  color = "black", label.padding = ggplot2::unit(1.5, "mm"),
                     label.size = 0.5, label.r = ggplot2::unit(0.5, "mm"))

  return(cube)
}


show_points <- function(bearoff = "right") {
  points <- ggplot2::scale_x_continuous(breaks = seq(1/26, 25/26, 2/26),
                               labels = c(12, 11, 10, 9, 8, 7, "", 6, 5, 4, 3, 2, 1 ))
  return(points)
}


show_checkers <-  function(xgid, bearoff = "right", player_color = "black") {

    if (nchar(xgid) != 51) stop("XGID does not contain exactly 51 characters")

    # Init vars to populate
    x <- NULL
    y <- NULL
    player <- NULL

    # x-coordinates for each point
    x_coord <- rep(NULL, 26)
    x_coord[2:7] <- ((2:7) - 2) * 2/26
    x_coord[8:13] <- ((8:13) - 1) * 2/26
    x_coord[14:19] <- ((13:8) - 1) * 2/26
    x_coord[20:25] <- ((7:2) - 2) * 2/26
    x_coord[c(1,26)] <- 12/26

    # y-coordinates for bottom and top points
    y_coord_bottom <- seq(0, 4/11, 1/11)
    y_coord_top <- seq(10/11, 6/11, -1/11)

    # Loop over all 26 points (#1 is our bar, #26 is opp's bar)
    for (i in seq(1, 26)) {
      entry <- substr(xgid, i + 5, i + 5)

      if (entry != "-") {
        player_checkers <- match(entry, LETTERS)
        opponent_checkers <- match(entry, letters)
        no_checkers <- coalesce(player_checkers, opponent_checkers)

        if (is.na(no_checkers)) stop(paste("Illegal character found in XGID string: ", entry))

        x_point <- rep(x_coord[i], no_checkers)

        y_point <- case_when(i < 14 ~ y_coord_bottom[1:no_checkers],
                             i > 13 ~ y_coord_top[1:no_checkers])

        p_point <-  rep(if_else(is.na(player_checkers), "top", "bottom"), no_checkers)


        x <-  c(x, x_point)
        y <-  c(y, y_point)
        player <- c(player, p_point)
        }
    }

    df <- tibble(x = x, y = y, player = player)
    return(df)
}

library(tidyverse)
library(ggforce)

df <- show_checkers(xgid = xgid)

df %>% ggplot() +
  geom_circle(aes(x0 = x, y0 = y, fill = player, r = 1/22), show.legend = F) +
  theme_void()
