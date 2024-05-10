########### Regular Regression Example ##############
library(readxl)
LungCapData <- read_excel("LungCapData.xls")
View(LungCapData)
plot(LungCapData)#Visualization

### check variables
names(LungCapData)
summary(LungCapData)

### Fitting lm model
model1<- lm(LungCap~Age+Height, data=LungCapData, na.action=na.exclude)
summary(model1)

### Confidence Interval
confint(model1, conf.level=0.95)

### Fitting th model for al X´s
model2 = lm(LungCap ~ Age + Height + Smoke + Gender + Caesarean,data=LungCapData, na.action=na.exclude)
summary(model2)

par(mfrow=c(2,2)) #Break the grid into 2x2
plot(model2)


##### Multiple Regression with interaction ############
library(readxl)
LungCapData <- read_excel("LungCapData.xls")
View(LungCapData)
plot(LungCapData)
class(LungCapData$Age)
class(LungCapData$Smoke) # 1 for "yes" 0 for "no"
library(ggplot2)

#Plot the data, using different colors for smoke (red) / non smoke (blue)
par(mfrow=c(1,1)) #Break the grid into 1x1
plot(LungCapData$Age[LungCapData$Smoke=="no"], 
     LungCapData$LungCap[LungCapData$Smoke=="no"], 
     col="blue", ylim=c(0,15),xlim=c(0,20), xlab="Age", ylab="LungCap", main="LungCap vs Age, Smoke")

points(LungCapData$Age[LungCapData$Smoke=="yes"], 
       LungCapData$LungCap[LungCapData$Smoke=="yes"], 
       col="red", pch=16)
legend(1,15,legend=c("NonSmoker","Smoker"), col=c("blue","red"),pch=c(1,16),bty="n")
### No interaction is shown, parallel lines are drawn for both sets of data, no age would change it

### Now, how to fit a model with interaction

model3=lm(LungCap~Age*Smoke, data = LungCapData)
coef(model3)# just the coefficients

model3=lm(LungCap~Age+Smoke+Age*Smoke, data = LungCapData) # adding interaction
summary(model3) # now just write the regression models for both
abline(a=1.052, b=0.558, col="blue",lwd=3)
abline(a=1.278, b=0.498, col="red",lwd=3)
### Does this new model make better sense than the previous one (conceptually & statistically (p value)?
### we can compare with the following:
model4=lm(LungCap~Age+Smoke, data = LungCapData)
summary(model4)
 
### Polynomial Regression #############
library(readxl)
LungCapData2 <- read_excel("LungCapData2.xlsx")#Different data base - check your file at Canvas/Comunidad ITAM
View(LungCapData2)
summary(LungCapData2)
with(LungCapData2, plot(Height, LungCap, main="Polynomial Regression", las=1))

#plot(Height, LungCap, main="Polynomial Regression", las=1)

#fitting the model
model1 <- lm(LungCap ~ Height, data = LungCapData2)
summary(model1)

# adding the line to compare tendency
abline(model1, lwd=3, col="red")

## propose a polynomial representation, watch out how it is written in the code
# hay que poner la I por que es la idempotencia y si no te toma el cuadrado en general
model2 <- lm(LungCap ~ Height + I(Height^2), data = LungCapData2) ## model2 <- lm(LungCap ~ poly(Height, degree=2, raw=T)) alternative
summary(model2)

# hasta que punto dejo de agregar variables
# Backward elimination 
# foward selection 

# add this blue line to the plot and compare
lines(smooth.spline(LungCapData2$Height, predict(model2)), col="blue", lwd=3)
# para comprobar cual ajusta mejor hacemos una prueba F o un P-value 

# test if the model including Height^2 i signif. better than one without
# using the partial F-test
# Null Hy: there is no significant difference between the two models
anova(model1, model2) ### small p-value, we  reject the Null Hyp, so new model provides a statistically significant better fit

#Let´s try another one
model3 <- lm(LungCap ~ Height + I(Height^2) + I(Height^3), data = LungCapData2)
summary(model3)

##add it to the plot
with(LungCapData2, lines(smooth.spline(Height, predict(model3)), col="green", lwd=3, lty=3))
legend(46, 15, legend = c("model1: linear", "model2: poly x^2", "model3: poly x^2 + x^3"), 
       col=c("red", "blue", "green"), lty=c(1,1,3), lwd=3, bty="n", cex=0.9)

##compare once again
anova(model2, model3) ## cubic does not improve the model
# con aquí sale 0.6582 y en el el anterior teníamos e-14 (mucho más chiquito)
# mientras más chiquito mejor, nivel de significancia


