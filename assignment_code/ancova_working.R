#test ANCOVA
library(tidyverse)

data("msleep")


pairs(~ sleep_total + sleep_rem + sleep_cycle + awake + brainwt + bodywt, data = msleep)


###Sleep total and Sleep REM between carnivora and rodents

d1 <- msleep %>%
  filter(order == "Carnivora" | order == "Rodentia")

p1 <- 
  ggplot(d1, aes(sleep_total, sleep_rem, color=order))+
  geom_point(size=5) +
  geom_smooth(method = 'lm', se=FALSE) +
  theme_bw()
p1

m1 <- lm(sleep_rem~sleep_total*order, data=d1)
anova(m1)
summary(m1)


####Body Wt vs Sleep Total between (mid sized) carnivora and Primates
d2 <- msleep %>%
  filter(order == "Carnivora" | order == "Primates") %>% 
  filter(bodywt<60)

p2 <- 
  ggplot(d2, aes(bodywt, sleep_total, color=order))+ 
  geom_point(size=5) +
  geom_smooth(method = 'lm', se=FALSE) +
  theme_bw()
p2

m2 <- lm(sleep_total~bodywt*order, data=d2)
anova(m2)
summary(m2)

##sticky example because everything is NON SIG.
