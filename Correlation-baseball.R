library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#correlation coefficient between number of runs per game and number of at bats per game

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%  summarize(r = cor(AB_per_game, R_per_game))


library(Lahman)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
cor(Teams_small$R/Teams_small$G, Teams_small$AB/Teams_small$G)

#orrelation coefficient between win rate (number of wins per game) and number of errors per game

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G, E_per_game = E / G) %>%  summarize(r = cor(win_rate, E_per_game))

cor(Teams_small$W/Teams_small$G, Teams_small$E/Teams_small$G)

#correlation coefficient between doubles (X2B) per game and triples (X3B) per game

cor(Teams_small$X2B/Teams_small$G, Teams_small$X3B/Teams_small$G)