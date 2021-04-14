#set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
library(dplyr)
library(ggplot2)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)



# plot predictions and confidence intervals
female_heights %>% ggplot(aes(mother, daughter)) +
  geom_point() +
  geom_smooth(method = "lm")


# predict Y directly
fit <- female_heights%>% lm( mother~ daughter, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
summary(fit)

# plot best fit line
female_heights %>%
  mutate(Y_hat = predict(lm(daughter ~ mother, data=.))) %>%
  ggplot(aes(mother, Y_hat))+
  geom_line()

y <- 44.18+.31*69