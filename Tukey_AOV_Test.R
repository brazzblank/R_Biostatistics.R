install.packages("MASS")
install.packages("xtable")
install.packages("nlme")
install.packages("lattice")

library(MASS)
library(xtable)
library(nlme)
library(lattice)

#Determining which variety grows best on a particular farm
beans <- read.csv("beans.csv")
beans$BEAN <- factor(beans$BEAN)
beans$BLOCK <- factor(beans$BLOCK)

#boxplot displaying the bean variety and yield 
with(beans, boxplot(YIELD ~ BEAN, xlab = "Beans Variety"))

#Analysis of Variance test suggests evidence for differences in yield across bean varieties based on a statistically significant p-value
beanaov <- aov(YIELD ~ BLOCK + BEAN, data = beans)
summary(beanaov)

#Tukey analysis shoes that groups one and two have the highest yields, but are not significantly different from each other
TukeyHSD(beanaov, "BEAN", ordered = TRUE)

#Bean variety yield model assessment graphs
with(beans, interaction.plot(BEAN, BLOCK, YIELD))
plot(predict(beanaov), resid(beanaov), ylab = "Residuals", xlab = "Predicted Values")
abline(h = 0)



#Research trial on swine weight gain based on group housing arrangements
gains <- read.csv("wtgain.csv")
gains$Block <- factor(gains$Block)

#interaction plots between the two conditions of uniform pen sizes and familiar groups and weight gain
coplot(wetgain2 ~ familiar | uniform, data = gains, panel = panel.smooth, xlab = "Weight Gain by Familiarity, given Pen Diversity")
with(gains, interaction.plot(familiar, uniform, wtgain2, ylab = "Mean Weight Gain", xlab = "Familiarity", trace.label = "Uniformity"))

#The analysis of variance shows no evidence of an interacting effect between familiarity and uniformity, but a significant effect for familiarity (p-value = 0.01)
#Evidence that familiar pigs have higher weight gains.
gmint1 <- aov(wtgain2 ~ familiar * uniform, data = gains)
summary(gmint1)

#Tukey test shows about a 2 pound increase in weight gain for familiar vs unfamiliar pigs
TukeyHSD(gmint1, "familiar", ordered = TRUE)

#Residual plot shows no series problems with non constant variance or skewness
plot(predict(gmint1), resid(gmint1), ylab = "Residuals", xlab = "Predicted Values")

#Addition of a term to capture the blok variation or spatial variation in the barn does not seem to alter conclusion that familiarity is the significant factor in swine weight gain
gblk2 <- aov(wtgain2 ~ Block + familiar * uniform, data = gains)
summary(gblk2)



#Analyzing infant birthweight and smoking status of the mother during the first trimester
obstet <- read.csv("obstet.csv")

#Birthweight boxplot based on smoking groups (NON = nonsmoker, EX = smoker before pregnancy but not during, CURL1 = smokes less than 1 pack/day, CURG1 = smokes more than 1 pack/day)
with(obstet, boxplot(bwt ~ group, ylab = "Birthweight", xlab = "Smoking Group"))

#There is evidence that the birthweight differs between smoking groups (p-value = 0.01371)
obaov <- aov(bwt ~ group, data = obstet)
summary(obaov)

#residual plot versus predicted value shows no evidence of non constant variance or outliers
plot(predict(obaov), resid(obaov), ylab = "Residuals", xlab = "Predicted Birthweight")

#The Tukey analysis only shows significantly higher birthweights between the nonsmoker and CURG1 groups (p-value = 0.01786)
TukeyHSD(obaov, ordered = TRUE)




