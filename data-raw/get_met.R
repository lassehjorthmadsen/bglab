library(tidyverse)
library(readr)
theme_set(theme_minimal())

met_file <- "c:\\Program Files (x86)\\eXtreme Gammon 2\\MET\\Kazaross XG2.met"

met <- read_delim(met_file, skip = 12, delim = " ", col_names = as.character(1:25)) %>%
  mutate(`1` = as.numeric(str_remove(`1`, "^.+="))) %>%
  pivot_longer(cols = everything(), names_to = "p2") %>%
  mutate(p2 = as.numeric(p2),
         p1 = sort(rep(1:25, 25)))

met %>%
  ggplot(aes(x = p1, y = value, color = factor(p2))) +
  geom_line() +
  geom_point(size = 0.75)
