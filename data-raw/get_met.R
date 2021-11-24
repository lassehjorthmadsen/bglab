library(tidyverse)
library(readr)
library(gramEvol)
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


# Symbolic reg

p1 <- met$p1
p2 <- met$p2
y  <- met$value

ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos, log, sqrt),
                op = grule('+', '-', '*', '/'),
                var = grule(p1, p2))

grammarDef <- CreateGrammar(ruleDef)
grammarDef

SymRegFitFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (mean(log(1 + abs(y - result))))
}

set.seed(314)
ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.1, iterations = 250, max.depth = 3)
ge

best_expr <- ge$best$expression

data.frame(p1, p2, me = y, ge = eval(best_expr))
