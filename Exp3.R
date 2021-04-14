#Load the Lahman library and filter the Teams data frame to the years 1961-2001. 
#Run a linear model in R predicting the number of runs per game based on both the
#number of bases on balls per game and the number of home runs per game.

library(Lahman)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G)


pr1 <- lm(R_per_game ~ BB_per_game+HR_strata, data = dat)
pr1$coef


library(Lahman)
library(broom)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))

