#' @name board_ratio
#' @title The height/width ratio of diagrams
#' @description This variable stores the height/width ratio of the diagrams
#'   produced by `ggboard()`. It is shared among many helper functions.
board_ratio <- 11/13 # By design, board is 11 checkers high, 13 checkers wide


#' @name checker_radius
#' @title The radius of checkers
#' @description This variable stores the radius of checkers used in
#'   diagrams produced by `ggboard()`. For use in some helper functions.
checker_radius <- 1/26 # Since board is 13 checkers wide

#' @name gnu_chars
#' @title Characters used in position and match ids
#' @description This variable stores the set of characters used by
#'   GNU Backgammon in position id and match id
gnu_chars <- c(LETTERS, letters, 0:9, "+", "/")

#' @name color_schemes
#' @title Color schemes
#' @description This list stores colors uses in different color schemes.
color_schemes <- list(
  "bw" = list(
    "odd_points_fill" = "white",
    "even_points_fill" = "#cccccc",
    "top_checker_fill" = "black",
    "bottom_checker_fill" = "white",
    "top_checker_off_border" = "white",
    "bottom_checker_off_border" = "black",
    "board_fill" = "white",
    "board_border" = "black",
    "bar_fill" = "white"
    ),
  "soft" = list(
    "odd_points_fill" = "#e8d8c4",
    "even_points_fill" = "#96b3da",
    "top_checker_fill" = "#010100",
    "bottom_checker_fill" = "#f7fffc",
    "top_checker_off_border" = "#f7fffc",
    "bottom_checker_off_border" = "#010100",
    "board_fill" = "#7dcde4",
    "board_border" = "#678a9a",
    "bar_fill" = "#c3e8f4"
    )
  )

