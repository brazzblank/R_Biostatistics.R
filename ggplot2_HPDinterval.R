install.packages("lme4")
install.packages("ggplot2")
install.packages("xtable")
install.packages("gmodels")
install.packages("plyr")

library(lme4)
library(ggplot2)
library(xtable)
library(gmodels)
library(plyr)

#Examining the yield of har within a block of land of three different alfalfa varieties as well as different harvest days 
alf <- read.csv("alf.csv")
alf$Block <- as.factor(alf$BLock)
alf$Datef <- factor(alf$Date, levels = c("b", "c", "d", "a"), labels = c("Sep1", "Sep20", "Oct7", "None"))

#Alfalfa yield model interaction plot does not show strong visual evidence of Variety differences
mtm <- ddply(alf, .(Datef, Variety), summarise, mn = mean(Yield))
p <- ggplot(data = mtm, aes(x = Datef, y = mn, group = Variety, colour = Variety))
p + geom_point(size = 3) + 
  geom_line() + 
  scale_colour_hue("Alfalfa \nVariety", breaks = c("cossack", "ladak", "ranger"), labels = c("Cossack", "Ladak", "Ranger")) + 
  xlab("Date of Third Cut") + 
  ylab("Mean Yield")

#Analysis of Variance based on variety does not show evidence of an effect based on variety (p-value = 0.8563)
alf.aov <- aov(Yield ~ Variety * Datef + Error(Block/Variety), data = alf)
summary(alf.aov)

#There is no evidence of an interaction of variety and datef (p-value = 0.187), there is evidence that datef (date of third cut) is significant (p-value = 2.56e^-05)
alflmerint <- lmer(Yield ~ Variety * Datef + (1 | Block/Variety), data = alf)
summary(alfmerint)
alfmcmcint <- mcmcsamp(alflmerint, n = 3000)
HPDinterval(alfmcmcint, prob = 0.95)$fixef

#The Sep1 and Sep20 dates show significant reductions in yield compared to no third cut at those dates. Oct7 date is not significantly different from no third cut meaning no evidence the next years harvest yield differes between no third cut and a third cut on Oct7.
alflmer <- lmer(Yield ~ Variety + Datef + (1 | Block/Variety), data = alf)
summary(alflmer)

alfmcmc <- mcmcsamp(alflmer, n = 3000)
HPDinterval(alfmcmc, prob = 0.95)$fixef

alflmerv <- lmer(Yield ~ Datef + (1 | Block/Variety), data = alf)
anova(alflmerv, alflmer)

estimable(alflmer, c(0, 0, 0, 0, -1, 1), C(0), conf.int = TRUE, show.beta0 = TRUE, sim.lmer = TRUE, n.sim = 1500)

estimable(alflmer, c(0, 0, 0, -1, 0, 1), C(0), conf.int = TRUE, show.beta0 = TRUE, sim.lmer = TRUE, n.sim = 2000)

estimable(alflmer, c(0, 0, 0, -1, 1, 0), C(0), conf.int = TRUE, show.beta0 = TRUE, sim.lmer = TRUE, n.sim = 2000)

estimable(alflmer, c(0, 1, 11, 0, 0, 0), C(0), conf.int = TRUE, show.beta0 = TRUE, sim.lmer = TRUE, n.sim = 1000)

#Alfalfa yield model residual plot
resids <- resid(alflmer)
fittedvals <- fitted(alflmer)
alflmerdframe <- data.frame(alf, resids, fittedvals)

p <- ggplot(data = alflmerdframe, aes(x = fittedvals, y = resids))
p + geom_point(aes(colour = Variety), size = 3) +
  stat_smooth(se = TRUE) +
  geom_hline(yintercept = 0) +
  scale_colour_hue("Alfalfa /nVariety", breaks = c("cossack", "ladak", "ranger"), labels = c("Cossack", "Ladak", "Ranger")) +
  xlab("Predicted Value") +
  ylab("Residuals")

#Alfalfa yield residual plot 2
p <- ggplot(data = alflmerdframe, aes(x = fittedvals, y = resids))
p+ geom_point(aes(colour = Variety), size = 3) +
  geom_hline(yintercept = 0) +
  facet_wrap(Block) +
  scale_colour_hue("Alfalfa /nVariety", breaks = c("cossack", "ladak", "ranger"), labels = c("Cossack", "Ladak", "Ranger")) +
  xlab("Predicted Value") +
  ylab("Residuals")







