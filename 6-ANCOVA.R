#C.M. Gienger
#Ch6 ANCOVA

library(HH)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(emmeans)

#import and view data
limp <- read.csv("limpet.csv")
limp$SEASON <- as.factor(limp$SEASON) #force SEASON to be considered as a factor

View(limp)
glimpse(limp)

#as always, we make a plot to visualize data; no fit lines yet
ggplot(limp, aes(x = DENSITY, y = EGGS, colour = SEASON)) +
  geom_point() +
  scale_color_manual(values = c(spring="green", summer="red")) +
  theme_bw()

#technical note about fitting group-specific regression lines
#https://stackoverflow.com/questions/16830947/ancova-plot-in-ggplot

#HH package has slick plotting function
ancovaplot(EGGS ~ DENSITY + SEASON, data=limp) #equal slopes plot
ancovaplot(EGGS ~ DENSITY * SEASON, data=limp) #separate slopes plot

#specify the ANCOVA model
limp.mod <- lm(EGGS ~ DENSITY * SEASON, data = limp)

#details about the model
names(limp.mod)

#check model assumptions
autoplot(limp.mod, smooth.colour = "green")

#interpret the model
anova(limp.mod)
summary(limp.mod)

#fit model WITHOUT interaction (since we remove N.S. interaction terms)
limp.mod.nointeraction <- lm(EGGS ~ DENSITY + SEASON, data = limp)
anova(limp.mod.nointeraction)
summary(limp.mod.nointeraction)

#doesn't change much because slope and seasons were already significant
#we are just re-assigning the '0.0118' variation from interaction back
#to the other model terms.

#compare the two models
anova(limp.mod, limp.mod.nointeraction)
#they have statistically similar explanatory power

#emmeans to compare adjusted tail lengths
emm_limp.mod.nointeraction <- emmeans(limp.mod.nointeraction, "SEASON")
emm_limp.mod.nointeraction

#raw means
means <- limp %>%
  group_by(SEASON) %>%
  summarise(mean=mean(EGGS))
means

#in this case raw and adjusted means are similar (b/c fake data?)
