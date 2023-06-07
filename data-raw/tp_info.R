# Try a slightly different angle for implementing
# calcualtion of live take points w/gammons

library(tidyverse)
devtools::load_all()
#source("data-raw/tp_info.R")
met <- get_met()[1:5, 1:5]

# The no-gammon take points agree with the bots:
tp_info(5,3,0,0,1,met)

# ... but the gammonish take points do not:
tp_info(5,3,0.1,0.1,1,met)

# ... if gammons are certainties, we see the same discrepancy
# but much larger: tp_live = 0.171 vs 0.256 (GNU)
tp_info(5,3,1,1,1,met)

# How does this situation look from the opponent's perspective?
# ... he can pass for 0.5 or take for the match, so clearly any tp = 0.5
tp_info(3,5,1,1,2,met)

# So, in this special case of certain gammons, can you take at (-5, -3)
# with just 0.1708, rather than 0.3415, because you can cash at 0.5000?

# Wheeler: p(E) = a / (a + b) = 1708 / (1708 + 3292) = 0.3416
# i.e. the probability that our 0.17 goes to 0.50 before it drops to
# zero is 0.3416 -- just what we need to take.

# Of course, this is a bit artificial; we would double automatically,
# since we lose the match if we lose the game (and a certain gammon).
# In that case: tp = 0.2564; what we have if we pass.

# There's no way we can do better by holding off. How is GNU taking into
# account this kind of desperation double?



tp_info <- function(x, y, xgf, ygf, cube, met) {

  D <- mwc(x, y - cube, met)

  W <- (1 - xgf) * mwc(x - 2 * cube, y, met) +
            xgf  * mwc(x - 4 * cube, y, met)

  L <- (1 - ygf) * mwc(x, y - 2 * cube, met) +
            ygf  * mwc(x, y - 4 * cube, met)

  risk <- D - L
  gain <- W - D

  tp_dead <- risk / (risk + gain)

  if (tp_dead == 0) {
    tp_live <- 0
    } else {
    tp_live <- tp_dead * (1 - tp_info(y, x, ygf, xgf, 2 * cube, met)["tp_live"])
  }

  info <-  c("D" = D,
             "W" = W,
             "L" = L,
             "risk" = risk,
             "gain" = gain,
             "tp_dead" = tp_dead,
             "tp_live" = unname(tp_live))

  return(info)
}
