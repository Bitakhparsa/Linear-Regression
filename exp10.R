library(tidyverse)
library(HistData)
library(broom)
library(dplyr)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton
 #-----------------------
 gfd <- galton %>% group_by(pair) %>% filter(pair== "father_daughter"         )
nrow(gfd)

gms <- galton %>% group_by(pair) %>% filter(pair== "mother_son"         )
nrow(gms)

gmd <- galton %>% group_by(pair) %>% filter(pair== "mother_daughter"         )


gfs <- galton %>% group_by(pair) %>% filter(pair== "father_son"         )

galton %>%
  group_by(pair) %>%
  summarize(n = n())


#------------------------
get_slope <- function(data){
  fit <- lm(childHeight  ~ parentHeight, data = gfd)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
gfd %>%  
  group_by(pair) %>%
  do(get_slope(.))




get_slope <- function(data){
  fit <- lm(childHeight  ~ parentHeight, data = gms)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
gms %>%  
  group_by(pair) %>%
  do(get_slope(.))


get_slope <- function(data){
  fit <- lm(childHeight  ~ parentHeight, data = gmd)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
gmd %>%  
  group_by(pair) %>%
  do(get_slope(.))




get_slope <- function(data){
  fit <- lm(childHeight  ~ parentHeight, data = gfs)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
gfs %>%  
  group_by(pair) %>%
  do(get_slope(.))
#-----------------------------
fit <- lm(childHeight  ~ parentHeight,dat=gfd)
tidy(fit)
glance(fit)


fit <- lm(childHeight  ~ parentHeight,dat=gfs)
tidy(fit)
glance(fit)


fit <- lm(childHeight  ~ parentHeight,dat=gmd)
tidy(fit)
glance(fit)


fit <- lm(parentHeight  ~childHeight ,dat=gms)
tidy(fit)
glance(fit)



library(broom)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter") %>%
  pull(estimate)


galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)


library(broom)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_son") %>%
  pull(estimate)





galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)



galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_daughter") %>%
  pull(estimate)





#-------------------------------

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  select(p.value, estimate, conf.low, conf.high) %>%
  ggplot(aes(p.value, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()


galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  pull(estimate)
  
  
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)

