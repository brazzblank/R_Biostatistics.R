#Biostatistics homework for Regression Models

install.packages("lattice")
install.packages("MASS")
install.packages("alr3")
install.packages("car")
install.packages("Hmisc")

library(lattice)
library(MASS)
library(alr3)
library(car)
library(Hmisc)



#Describing the relationship between Forced Expiratory Volume (FEV) and  Height
fev <- read.csv("fev.csv")

#Graph suggests as Height increases FEV increases
plot(FEV ~ Height, xlab = 'Height(cm)', data=fev)
lines(lowess(fev$Height, fev$FEV), lty=1, lwd=0.75)

#Statistical evidence that Simple Linear Regression model slope is positive 
#T-test is very significant and the parameter estimate is positive suggesting more height is associated with more FEV
#R squared = 0.9742 which means about 97% of the variation in FEV can be explained by height
fevmod <- lm(FEV ~ Height, data = fev)
summary(fevmod)

#Confidence interval does not contain zero so there is evidence that the slope is something other than zero
confint(fevmod)

#Prediction of a boy with a height of 200cm with an observed FEV of 4.5
newboy <- data.frame(Height=200)

#The observed FEV is outside of what would be expected (FEV = 4.539 to 5.368), which suggests lung-related difficulty
predict(fevmod, newboy, interval="prediction")


#Examining the relationship between birthweight and number of cigarettes smoked by mothers
chds <- read.csv("chds.csv")

chdsmod <- lm(bwt ~ mnocig, data=chds)

#R squared is 0.0322, only about 3% of variation in birthweight can be explained by number of cigarettes smoked
summary(chdsmod)

#Predicting the birthweight of a child with a mom that smokes 50 cigarettes per day
newmom <- data.frame(mnocig=50)

#Predicted birthweight is 6.7767 pounds and a 95% confidence interval for the birthweight to be between 4.6412 and 8.9115 lbs 
predict(chdsmod, newmom, interval="prediction")

#Plotting the relationship between smoking cigarettes and birthweight
plot(bwt ~ mnocig, xlab = "Mother Cigarettes Per Day", ylab = "Birthweight", data=chds)
abline(chdsmod)

#Jitter returns a numeric of the same length, but with noise added in order to break ties
plot(bwt ~ jitter(mnocig), xlab = "Mother Cigarettes Per Day", ylab = "Birthweight", data=chds)
abline(chdsmod)

#Graph shows evidence of decreasing relationship between birthweight and cigarette consumption, but is weak with lots of variation
#Confidence interval for the slope is negative (-0.017386) and interval does not contain zero, suggesting the slope is something other than zero.
#The significant p-value indicates the number of cigarettes is statistically significant factor, but the low R squared indicates the relationship is not strong.
confint(chdsmod)


#FEV study from Rosnver statistics book measuring the respiratory function including factors such as smoking, gender, age, and height
FEVRosner <- read.csv("FEV.csv")
summary(FEVRosner)

#Basic model with no interaction for reference
fev.mod1 <- lm(fev ~ age + height, data=FEVRosner)
summary(fev.mod1)

#Regression of FEV on age, height, and interaction between age and height
fev.modint <- lm(fev ~ age * height, data=FEVRosner)

#Both age and height main effects are statistically significant. 
#The height has a positive value suggests more height = more FEV, but the individual age coefficient being negative is a confusing result and may suggest higher-order interaction.
#Looking at the interaction the increasing effect of height on FEV seems to get larger for increasing age.
summary(fev.modint)
confint(fev.modint)

#Interaction graph showing the height effect becoming greater as age increases. Consistent with interaction Coefficient
print(plot(effect("age*height", fev.modint, xlevels=list(height=50:70, age=seq(2,20,6))), multiline=TRUE, ylab = "FEV"))

#SHows in general as age increases, FEV increases for most height levels.
print(plot(effect("age*height", fev.modint, xlevels=list(age=2:12, height=seq(45,70,6))), multiline=TRUE, ylab = "FEV"))

#Hypothesis testing to determine whether sex and smoking status are significant factors after adjusting for age and height
fev.mod2 <- lm(fev ~ height * height + sex + smoke, data=FEVRosner)

#Test statistic F = 8.9931 and the P-value is significant (0.0001405), the data are unlikely to occur under the null hypothesis
#There is evidence to support the alternative hypothesis that sex and smoking help explain FEV after adjusting for age and height.
#After adjusting for all other effects, males have a larger FEV than females (0.998 liters more) and non-smokers have a significant higher FEV
summary(fev.mod2)
anova(fev.modint, fev.mod2)

#Residual plot points are evenly distributed around zero, but suggest the simple linear model assumption of constant error is violoted due to the increasing spread around zero as the predicted FEV increases.
residualPlot(fev.modint)




#Exploring if there is evidence lead exposure has an effect on finger taps are adjusting for age and sex
getHdata("lead")

#Intercept is 27.467, the t statistic (7.763) and p-value (9.99e^-12) show clear evidence the parameter is positive, no matter what sex or lead group the initial tapping is about 27.5 taps
#Age is 2.679, as age increases by 1 year estimated taps will increase by that amount. P-value (1.26e^-12) suggests strong evidence that after adjusting for sex and lead exposure taps tend to increase as age increases
#Sex is -1.868 suggesting slightly fewer taps for women, but the p-value (0.350) is not significant, suggesting no evidence that tapping relates to sex after adjusting for lead exposure and age
#Lead group 73 p-value (0.003) is significant and -7.648 suggests a decrease in taps. The 72 group however p-value (0.526) suggests no evidence mental function is affected.
mod2 <- lm(maxfwt ~ age + sex + group, data=lead)
summary(mod2)

#No apparent problems with the model based on the residual plot
residualPlot(mod2)

#With a test statistic of F = 4.6431 and p-value = 0.01194, there is evidence to reject the null hypothesis and support the alternative hypothesis that lead exposure levels explain cognitive functioning.
modnone <- lm(maxfwt ~ age + sex, data=lead)
anova(modnone, mod2)

#Looking at the anova of the full model and only sex/age there is significant evidence (p-value 0.01952) that blood lead concentrations explain significant variation in taps after adjusting for age and sex.
modfull <- lm(maxfwt ~ age + sex + ld72 + ld73, data=lead)
summary(modfull)
anova(modnone, modfull)









