
#' Get match equity table from *.met file (used by Extreme Gammon)
#'
#' @param filename file location
#'
#' @return dataframe
#' @export
#'
get_met <- function(filename = "c:\\Program Files (x86)\\eXtreme Gammon 2\\MET\\Kazaross XG2.met") {

  top9 <- read_delim(filename, skip = 12, delim = " ", n_max =  9, col_names = as.character(0:25), col_types = list(.default = "c")) %>%
    select(-`0`)

  rest <- read_delim(filename, skip = 21, delim = " ", n_max = 16, col_names = as.character(1:25), col_types = list(.default = "c"))

  met <- bind_rows(top9, rest) %>%
    mutate(`1` = str_remove(`1`, "^.+=")) %>%
    pivot_longer(cols = everything(), names_to = "y", values_to = "mwc") %>%
    mutate(x = sort(rep(1:25, 25)),
           y = as.integer(y),
           mwc = as.numeric(mwc)) %>%
    select(x, y, mwc) %>%
    arrange(y, x)

}
