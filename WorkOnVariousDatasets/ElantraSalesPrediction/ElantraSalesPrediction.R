
#Problem 1 - Loading the Data
# Load the data set. Split the data set into training and testing sets as follows: place all observations for 2012 and earlier in the training set, and all observations for 2013 and 2014 into the testing set.
# How many observations are in the training set?

elantra = read.csv("elantra.csv")
str(elantra)

training<- subset(elantra, Year <=2012)
testing<- subset(elantra,Year > 2012)

#Problem 2.1 - A Linear Regression Model
# Build a linear regression model to predict monthly Elantra sales using Unemployment, CPI_all, CPI_energy and Queries as the independent variables. Use all of the training set data to do this.
# What is the model R-squared? Note: In this problem, we will always be asking for the "Multiple R-Squared" of the model.

fit1<- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries , data = training)
summary(fit1)
#0.4282

# Problem 2.2 - Significant Variables
# How many variables are significant, or have levels that are significant? 
# Use 0.10 as your p-value cutoff.
# Answer 0

# Problem 2.3 - Coefficients
# What is the coefficient of the Unemployment variable?
fit1$coefficients[2]

#Problem 3.1 - Modeling Seasonality
fit2<- lm(ElantraSales ~ Unemployment + Month + CPI_all + CPI_energy + Queries , data = training)
summary(fit2)

#Problem 3.2 - Effect of Adding a New Variable
# The model is not better because the adjusted R-squared has gone down and none of the variables (including the new one) are very significant.

#Problem 3.3 - Understanding the Model
abs(fit2$coefficients[3]*3 - fit2$coefficients[3]*1)

abs(fit2$coefficients[3]*5 - fit2$coefficients[3]*1)


# Problem 3.4 - Numeric vs. Factors
# You may be experiencing an uneasy feeling that there is something not quite right in how we have modeled the effect of the calendar month on the monthly sales of Elantras. If so, you are right. In particular, we added Month as a variable, but Month is an ordinary numeric variable. In fact, we must convert Month to a factor variable before adding it to the model.
# 
# What is the best explanation for why we must do this?
# 
# [1] By modeling Month as a factor variable, the effect of each calendar month is not restricted to be linear in the numerical coding of the month. 

# Problem 4.1 - A New Model
fit3<- lm(ElantraSales ~ Unemployment + as.factor(Month) + CPI_all + CPI_energy + Queries , data = training)
summary(fit3)
#0.8193

#Problem 4.2 - Significant Variables
#Which are significant?
# Month, CPI_all, CPI_energy, Unemployment

#Problem 5.1 - Multicolinearity
cor(training)
cor(training[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
#Unemployment, Queries, CPI_all

#Problem 5.2 - Correlations
cor(training)
#Unemployment, CPI_energy, CPI_all

#Problem 6.1 - A Reduced Model
#Remove Queries
fit4<- lm(ElantraSales ~ Unemployment + as.factor(Month) + CPI_all + CPI_energy , data = training)
summary(fit4)

#Problem 6.2 - Test Set Predictions
pred<-predict(fit4, newdata = testing)
SSE = sum((pred - testing$ElantraSales)^2) #if we didnt have the values of the testing, we would be using mean.
SSE

#Problem 6.3 - Comparing to a Baseline
mean(training$ElantraSales)

#Problem 6.4 - Test Set R-Squared
SST = sum((mean(training$ElantraSales) - testing$ElantraSales)^2)
Rsquared = 1 - (SSE/SST)
Rsquared

#Problem 6.5 - Absolute Errors
max(abs(pred - testing$ElantraSales))

#Problem6.6 - Month of Largest Error
testing[which.max(abs(pred - testing$ElantraSales)),c("Month","Year")]
