
statedata = read.csv("statedata.csv")
str(statedata)

#Problem 1.1 - Data Exploration
plot(statedata$x, statedata$y)

#Problem 1.2 - Data Exploration
tapply(statedata$HS.Grad, statedata$state.region, mean)
#North Central     Northeast         South          West 
#     54.51667      53.96667      44.34375      62.00000 

#Problem 1.3 - Data Exploration    Which region has the highest median murder rate?
boxplot(statedata$Murder ~ statedata$state.region, xlab= "State", ylab="Murder")
#South has the highest median Murder rate

#Problem 1.4 - Data Exploration
North_east<- subset(statedata, state.region == "Northeast" & Murder >= 10)
North_east[c("Murder","state.name","state.region")]

#Problem 2.1 - Predicting Life Expectancy - An Initial Model
# We would like to build a model to predict life expectancy by state using the state statistics we have in our dataset.
# Build the model with all potential variables included (Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area). Note that you should use the variable "Area" in your model, NOT the variable "state.area".
# What is the coefficient for "Income" in your linear regression model?

fit1<- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(fit1)
fit1$coefficients[3]

# Problem 2.2 - Predicting Life Expectancy - An Initial Model
# Call the coefficient for income x (the answer to Problem 2.1). What is the interpretation of the coefficient x?

#ANSWER:  For a one unit increase in income, predicted life expectancy decreases by |x|

#Problem 2.3 - Predicting Life Expectancy - An Initial Model
#Now plot a graph of life expectancy vs. income using the command:
  #plot(statedata$Income, statedata$Life.Exp)
#Visually observe the plot. What appears to be the relationship?

plot(statedata$Income, statedata$Life.Exp)

#Problem 2.4 - Predicting Life Expectancy - An Initial Model
#The model we built does not display the relationship we saw from the plot of life expectancy vs. income. Which of the following explanations seems the most reasonable?
    #A: Multicollinearity 

#Problem 3.1 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions
# Recall that we discussed the principle of simplicity: that is, a model with fewer variables is preferable to a model with many unnnecessary variables. Experiment with removing independent variables from the original model. Remember to use the significance of the coefficients to decide which variables to remove (remove the one with the largest "p-value" first, or the one with the "t value" closest to zero), and to remove them one at a time (this is called "backwards variable selection"). This is important due to multicollinearity issues - removing one insignificant variable may make another previously insignificant variable become significant.
# You should be able to find a good model with only 4 independent variables, instead of the original 7. Which variables does this model contain?
fit1<- lm(Life.Exp ~ Population  + Murder + HS.Grad + Frost , data = statedata)

#Problem 3.2 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions

#Removing insignificant variables changes the Multiple R-squared value of the model. By looking at the summary output for both the initial model (all independent variables) and the simplified model (only 4 independent variables) and using what you learned in class, which of the following correctly explains the change in the Multiple R-squared value?
#ANSWER: We expect the "Multiple R-squared" value of the simplified model to be slightly worse than that of the initial model. It can't be better than the "Multiple R-squared" value of the initial model.

#Problem 3.3 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions
# Using the simplified 4 variable model that we created, we'll now take a look at how our predictions compare to the actual values.
# Take a look at the vector of predictions by using the predict function (since we are just looking at predictions on the training set, you don't need to pass a "newdata" argument to the predict function).
#  Which state do we predict to have the lowest life expectancy? (Hint: use the sort function)

statedata$pred<- predict(fit1)
head(statedata[order(statedata$pred),],1)
#Alabama

head(statedata[order(statedata$Life.Exp),],1)
statedata[which.min(statedata$Life.Exp),"state.name"]

#Problem 3.4 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions
statedata[which.max(statedata$pred),"state.name"]

statedata[which.max(statedata$Life.Exp),"state.name"]

"which.max(statedata$Life.Exp)"

#Problem 3.5 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions
statedata$res<-fit1$residuals^2
statedata[which.min(statedata$res),"state.name"]

statedata[which.max(statedata$res),"state.name"]
