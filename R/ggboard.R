#' Display a backgammon diagram from an eXtreme Gammon id
#'
#' @param xgid character
#'
#' @return ggplot object
#'
#' @examples
#' ggboard("XGID=-b----E-C---eE---c-e----B-:0:0:1:64:0:0:0:7:10")
#' ggboard("XGID=-a-B--E-B-a-dDB--b-bcb----:1:1:-1:63:0:0:0:3:8")
#' ggboard("XGID=-a-B--E-B-a-dDB--b-bcb----:1:1:-1:63:0:0:0:3:8", bearoff = "left)
#' ggboard("XGID=-a-a-BCBC---cC---cbd-Ba---:1:-1:1:00:0:0:0:7:10")
#'
#' @importFrom ggforce geom_circle
#'
#' @export
ggboard <- function(xgid, bearoff = "right") {

  if (bearoff == "left") xgid <- flip_xgid(xgid)

  position <- ggplot() +
    show_points() +
    show_cube(xgid) +
    show_points() +
    show_bar() +
    show_border() +
    show_numbers(bearoff) +
    show_checkers(xgid) +
    show_cube(xgid, bearoff) +
    #show_cube_value(xgid, bearoff) +
    coord_fixed() +
    ggplot2::scale_fill_manual(values = c("white", "darkgrey", "black", "lightgrey")) +
    ggplot2::theme_void()

  return(position)
}


show_points <- function() {
  ratio <- 11/13 # Board is 11 checkers high, 13 checkers wide. Move to global env?

  x <- 0:12 %>% purrr::map(~ rep(0:2/26, 2) + .x * 2/26) %>% unlist()
  y <- rep(c(0, 5/11, 0, 1, 6/11, 1), 13) * ratio
  fill <- rep(c(rep(T, 3), rep(F, 3), rep(F, 3), rep(T, 3)), 3)
  fill <- c(fill, rep(F, 6), fill)
  group <- 1:26 %>% purrr::map(rep, 3) %>% unlist()

  triangles <- dplyr::tibble(x = x, y = y, fill = fill, group = group)

  ggplot2::geom_polygon(data = triangles,
                          ggplot2::aes(x = x, y = y, fill = fill, group = group),
                 show.legend = F, color = "darkgrey")
}


show_border <- function() {
  ratio <- 11/13 # Board is 11 checkers high, 13 checkers wide. Move to global env?

  ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 1, ymin = 0, ymax = ratio),
            fill = NA, colour = "darkgrey", size = 0.5)
}


show_bar <- function() {
  ratio <- 11/13 # Board is 11 checkers high, 13 checkers wide. Move to global env?
  ggplot2::geom_rect(ggplot2::aes(xmin = 6/13, xmax = 7/13, ymin = 0, ymax = ratio),
            fill = "white", colour = "darkgrey", size = 0.5, inherit.aes = F)
}


show_cube <- function(xgid) {
  ratio <- 11/13 # Board is 11 checkers high, 13 checkers wide. Move to global env?

  cube_size <- 0.08
  cube_position = substr(xgid, 35, 36) %>% str_remove(":") %>% as.numeric()

  y_position <- 10/22 * ratio + cube_position * -10/22 * ratio

  ggplot2::geom_rect(ggplot2::aes(xmin = -0.01 - cube_size,
                                  xmax = -0.01,
                                  ymin = y_position,
                                  ymax = y_position + cube_size),
                     color = "darkgrey",
                     fill = "white",
                     linejoin = "round")
}


show_cube_value <- function(xgid) {

  cube_value = substr(xgid, 33, 33) %>% as.numeric()
  cube_value = 2^cube_value
  if (cube_value == 1) cube_value <-64

  cube_position = substr(xgid, 35, 36) %>% str_remove(":") %>% as.numeric()
  y_position <- -0.5 * cube_position + 0.5


  ggplot2::geom_label(ggplot2::aes(x = -0.06, y = y_position, label = cube_value),
                      size = 4,  color = "black")
}


show_numbers <- function(bearoff = "right") {
  ratio <- 11/13 # Board is 11 checkers high, 13 checkers wide. Move to global env?

  x <- rep(0:12  * 2/26 + 1/26, 2)
  y <- c(rep(-0.02, 13), rep(ratio + 0.03, 13))

  if (bearoff == "left") {
    label <- as.character(c(1:6, NA, 7:12, 24:19, NA, 18:13))
  } else if  (bearoff == "right") {
    label = as.character(c(12:7, NA, 6:1, 13:18, NA, 19:24))
  } else {
    stop("bearoff parameter must be either 'right' or 'left'")
  }

  df = tibble(x, y, label)

  ggplot2::geom_text(data = df, mapping = aes(x = x, y = y, label = label), color = "black", size = 3)

}


show_checkers <-  function(xgid, bearoff = "right") {
  df <- xgid2df(xgid)
  ggforce::geom_circle(mapping = ggplot2::aes(x0 = x, y0 = y, fill = player, r = 1/26),
                       show.legend = F, inherit.aes = F, data = df)
  }


xgid2df <- function(xgid) {

  if (nchar(xgid) < 51) stop("xgid string does not contain at least 51 characters")

  ratio <- 11/13 # Board is 11 checkers high, 13 checkers wide. Move to global env?

  # Init vars to populate
  x <- NULL
  y <- NULL
  player <- NULL

  # x-coordinates for each point
  x_coord <- rep(NULL, 26)
  x_coord[2:7]     <- 13:8 * 2/26 - 1/26
  x_coord[8:19]    <- 5:0 * 2/26 + 1/26
  x_coord[14:19]   <- 0:5 * 2/26 + 1/26
  x_coord[20:25]   <- 8:13 * 2/26 - 1/26
  x_coord[c(1,26)] <- 12/26 + 1/26

  # y-coordinates for bottom and top points
  y_coord_bottom <- seq(1/22, 9/22, 2/22) * ratio
  y_coord_top <- seq(21/22, 13/22, -2/22) * ratio

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


flip_xgid <- function(xgid) {
  substr(xgid, 7, 18)  <- stringi::stri_reverse(substr(xgid, 7, 18))
  substr(xgid, 19, 30) <- stringi::stri_reverse(substr(xgid, 19, 30))

  return(xgid)
}
