library(tidyverse)
library(hrbrthemes)
library(viridis)
devtools::load_all()

met <- get_met()

pwin3 <- 5/6*1/6*5/6 + 5/6*5/6*5/6*1/6

loseone <- met %>% mutate(y = y + 1) %>% rename(loseone = mwc)
winone <- met %>% mutate(x = x + 1) %>% rename(winone = mwc)
takelose <- met %>% mutate(y = y + 2) %>% rename(takelose = mwc)
takewin <- met %>% mutate(x = if_else(y > 2, x + 2, x + 4)) %>% rename(takewin = mwc)

roll3 <- met %>%
  left_join(loseone, by = c("x", "y")) %>%
  left_join(winone, by = c("x", "y")) %>%
  left_join(takelose, by = c("x", "y")) %>%
  left_join(takewin, by = c("x", "y")) %>%
  replace_na(list(loseone = 0, takelose = 0, winone = 1, takewin = 1)) %>%
  mutate(takemwc = pwin3 * takewin - pwin3 * takelose + takelose,
         takegain = takemwc - loseone,
         score = paste0("-", x, ",-", y),
         score = fct_reorder(score, takegain)) %>%
  arrange(takegain)

# Dot plot
roll3 %>%
  filter(x > 1, x < 10, y > 1, y < 10) %>%
  ggplot(aes(y = score, x = takegain, color = takegain > 0)) +
  geom_point(show.legend = F)

# Heatmap
roll3 %>%
  filter(x > 2, y > 2) %>%
  ggplot(aes(x, y, fill = takegain)) +
  geom_tile(color = "white") +
  scale_x_continuous(breaks = 2:25) +
  scale_y_continuous(breaks = 2:25) +
  scale_fill_viridis(discrete=FALSE) +
  theme_minimal()
