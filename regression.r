
library(readr) #load library to read the file (may do it with RStudio predefined functions)
Salary_Data <- read_csv("Salary_Data.csv") #read the file (may do it with RStudio predefined functions)
View(Salary_Data)#View the dataframe
plot(Salary_Data)#Visualization
model=lm(YearsExperience~Salary, data=Salary_Data, na.action=na.exclude) #fit the linear model
summary(model) #parameter output
anova(model) #Analysis of Variance

#Plotting the result
library(ggplot2)# Add the library
# building the graph, piecewise elements
ggplot(Salary_Data, aes(x = Salary, y = YearsExperience)) +
  geom_point() +  # Add the scatter plot of the data points
  geom_smooth(method = "lm", se = TRUE) +  # Add the fitted regression line WITH CONFIDENCE INTERVALS for the mean response
  labs(x = "Salary", y = "Years of Experience") +  # Add axis labels
  ggtitle("Linear Regression of Years of Experience vs. Salary")  # Add plot title

#Prediction
new_salary=70000 #New observation into the model
new_data=data.frame(Salary=new_salary)
prediction=predict(model, newdata=new_data)
print(prediction)

# Diagnostic plots for linear regression assumptions
par(mfrow=c(2,2)) #Break the grid into 2x2
plot(model)

#Reference
#1x1: Check for linearity. The points should be randomly scattered around the horizontal line, with no clear pattern.
#1x2: Assumption of normality (QQ Plot). The points should fall approximately along the diagonal line.
#2x1: Assumption of homoscedasticity. The points should be randomly scattered around the horizontal line, with no clear pattern.
#2x2: Influential outliers. Points outside the dashed lines may have a large influence on the regression model.

#Nevertheless, the observations and conclusions depend on every analyst experience
