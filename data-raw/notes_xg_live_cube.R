devtools::load_all()
met <- get_met()

# Minh Nguyen
# XG will calculate Live Cube take point like this
# "Live Cube Take Point" = "Dead Cube Take Point" x "Cash Point Cube on the next level"
# In the first step calculating "Dead Cube Take Point", he will always take 0% gammon rate for the Taker. Justification is that in the perfect model, Taker doesn't win any G as for all his wins he always reaches Cash Point and cubes opponent out. This first step approximation is questionable.
# In your case, no matter how you change Yellow's gammon rate, due to first step, XG will always give same Live Cube Take Point:
#   22.65% = 30.17% x 75.08%

id <- "XGID=aBB-BBB---B---A-db-dbB-b--:0:0:1:00:4:6:0:17:10"
ggboard(id)

# Analyzed in XG Roller++
#   Player Winning Chances:   71,66% (G:20,20% B:0,10%)
#   Opponent Winning Chances: 28,34% (G:5,25% B:0,34%)

xg_probs_nd <- c(71.66, 20.20, 0.10, 28.34, 5.25, 0.34)
xg_probs_dt <- c(72.43, 21.87, 0.15, 27.57, 5.98, 0.49)

probs_nd <- outcome_probs(xg_probs_nd)
probs_dt <- outcome_probs(xg_probs_dt)

# XGs cube information window seem to take information from
# the nd-probs; the below agree with the reported gammon and
# backgammon frequencies (within rounding error):

# White G freq:
(xg_probs_nd[2] - xg_probs_nd[3]) / xg_probs_nd[1]

# White BG freq:
xg_probs_nd[3] / xg_probs_nd[1]

# Black G freq:
(xg_probs_nd[5] - xg_probs_nd[6]) / xg_probs_nd[4]

# Black BG freq:
xg_probs_nd[6] / xg_probs_nd[4]

probs_nd_fliped <- c(probs_nd[4:6], probs_nd[1:3])

# Blacks take points
tp_black <- tp_info(11, 13, probs_nd_fliped, 1, met)
tp_black["tp_dead"] # agrees with cube information window, so no typos

# Whites takepoint for the recube, gammon less
tp4_white <- tp_info(13, 11, c(0.5, 0, 0, 0.5, 0, 0), 2, met)
tp4_white["tp_dead"] # also agrees with cube information window

tp_live_test <- tp_black * (1 - tp4_white)

0.3216 * 75.27

probs_nd_fliped

34.70

0.3862 * 0.7508

0.3595 * 0.7249

# 3-point match, no gammons
# "Live Cube Take Point" = "Dead Cube Take Point" x "Cash Point Cube on the next level"
# 22.65% = 30.17% x 75.08%

# 30.17: Dead cube gammon less tp
# 75.08: Cash point, implied no cube, no gammons

# 3-point match, opening style gammons:
Brown:  G: 26,43%, BG: 1,30%
Yellow: G: 24,80%, BG: 1,09%

0.2900 = 0.3862 * 0.7508

0.3862: Dead cube gammon less tp
0.7508: Dead cube, cash point including gammons.

