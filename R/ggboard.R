#' Display a backgammon diagram from an eXtreme Gammon id
#'
#' @param xgid character
#'
#' @return ggplot object
#'
#' @example
#' ggboard("XGID=-b----E-C---eE---c-e----B-:0:0:1:64:0:0:0:7:10")
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
