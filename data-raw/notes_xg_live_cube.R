library(tidyverse)
devtools::load_all()
met <- get_met()

# https://www.facebook.com/groups/backgammonstrategy/posts/3532552207013426/?comment_id=3631176307151015&notif_id=1718828064640279&notif_t=group_comment
# [I was asking about the edge case of -3,-3 when Yellow had a gammon frequency of 100%,
# all other frequencies zero], got this informative answer:

# Minh Nguyen
# XG will calculate Live Cube take point like this
# "Live Cube Take Point" = "Dead Cube Take Point" x "Cash Point Cube on the next level"
# In the first step calculating "Dead Cube Take Point", he will always take 0% gammon rate for the Taker.
# Justification is that in the perfect model, Taker doesn't win any G as for all his wins he always
# reaches Cash Point and cubes opponent out. This first step approximation is questionable.
# In your case, no matter how you change Yellow's gammon rate, due to first step, XG will always give
# same Live Cube Take Point:
# 22.65% = 30.17% x 75.08%

# Note: This needs to be done recursively for longer matches

# Checking the calculation, looks okay:
tp(3, 3, 1, met) * (1 - tp(3, 3, 2, met))

# Another example: 3-point match, opening style gammons:
# Brown:  G: 26,43%, BG: 1,30%
# Yellow: G: 24,80%, BG: 1,09%

# Outcome probabilities including gammons
probs <- c((1 - 0.2480 - 0.0109) * 50, 0.2480 * 50, 0.0109 * 50,
           (1 - 0.2643 - 0.0130) * 50, 0.2643 * 50, 0.0130 * 50)

# Outcome probabilities, no gammons won for taker (since he cashes):
probs2 <- c(50, 0, 0, probs[4:6])

# Take point if taker can't win gammons:
tp_gammons(3, 3, probs2, 1, met)

# Cash point for the recube
1 - tp_gammons(3, 3, probs, 2, met)

# XG live cube take point
tp_gammons(3, 3, probs2, 1, met) * (1 - tp_gammons(3, 3, probs, 2, met))

# So: 0.2900 = 0.3862 * 0.7508
# Checks out with cube information window in XG

# Implement a function for this
tp_xg(3, 3, probs, 1, met)


# A more complex example, where the re-cube is not dead
id <- "XGID=aBB-BBB---B---A-db-dbB-b--:0:0:1:00:4:6:0:17:10"

# Analyzed in XG Roller++
#   Player Winning Chances:   71,66% (G:20,20% B:0,10%)
#   Opponent Winning Chances: 28,34% (G:5,25% B:0,34%)

# no-double and double-take probabilities:
xg_probs_nd <- c(28.34, 5.25, 0.34, 71.66, 20.20, 0.10)
xg_probs_dt <- c(72.43, 21.87, 0.15, 27.57, 5.98, 0.49)

# It is the no-double probs that are used in XGs Cube Information
probs <- outcome_probs(xg_probs_nd)

# Agrees with the Cube Information window:
# Dead Cube: 32,16%, Live Cube: 27,59%
tp_xg(11, 13, probs, 1, met)

# Try for other scores:
tps <- 2:17 |>
  map(tp_xg, y = 17, probs = probs, cube = 1, met = met) |>
  bind_rows() |>
  mutate(x = 2:17, y = 17) |>
  pivot_longer(cols = c(tp_dead, tp_live))

# Plots
ggboard(id)

tps |>
  ggplot(aes(x = x, y = value, color = name, group = name)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2:17) +
  scale_y_continuous(breaks = seq(0.25, 0.35, 0.01)) +
  scale_color_discrete(labels = c("Dead Cube", "Live Cube")) +
  labs(title = "Take Points by score and cube liveliness",
       subtitle = "Opponent needs 17 point to win.\n'Dead' and 'Live' cube calculations like XG does",
       color = NULL, y = "Take Point", x = "Points player needs")
