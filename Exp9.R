


library(Lahman)
data("Batting") 
library(plyr)

  dat <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, 
         singles = (H - X2B - X3B - HR) / pa, bb = BB / pa) %>%
  filter(pa >= 100)%>% 
    group_by(playerID) 
  
  
  avg <- Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, avg_singles = (H - X2B - X3B - HR) / pa, avg_bb = BB / pa) %>% 
    filter(pa >= 100) %>% 
    group_by(playerID)
  
  d <- ddply(avg, .(playerID), summarize,  avg_singles=mean(avg_singles), avg_bb=mean(avg_bb))

  
  
  
  dat1 <- inner_join(dat, d, by = "playerID")
  
    dat1 %>% summarise(singles_r = cor(singles,avg_singles ), bb_r = cor(bb, avg_bb ))

 
    
    dat1 %>%
      ggplot(aes(singles, avg_singles)) +
      geom_point()
  
    
    dat1 %>%
      ggplot(aes(bb, avg_bb)) +
      geom_point() 

    
    fit <- lm(singles~avg_singles, data=dat1)
    
    fit <- lm(bb~avg_bb, data=dat1)
    
    
    