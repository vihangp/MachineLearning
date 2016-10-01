
wine = read.csv("wine.csv")
str(wine)
summary(wine)
model1 = lm(Price~AGST, data=wine)
summary(model1)
#Multiple R squared == This will never decrease when adding variables 
#Adjusted R-squared == Adjusted R-squared will decrease if there is a variable not needed

#Lets compute SSE
model1$residuals
SSE = sum(model1$residuals^2)
SSE
#[1] 5.734875
model2 = lm(Price~AGST + HarvestRain, data=wine)
summary(model2)
## Both Multiple R squared and Adjusted R squared increased (probably better model)

SSE= sum(model2$residuals^2)
SSE
#[1] 2.970373  Is better,, lower = better

model3= lm(Price~AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine) 
summary(model3)
SSE= sum(model3$residuals^2)
SSE
#[1] 1.732113 It appears to be better. Both Multiple R squared and Adjusted R squared increased

##QUICKQUESTION!
modelQ = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelQ)
#Multiple Rsquared =  .3177
interceptCoef = modelQ$coefficients[1]  
HarvestRainCoef = modelQ$coefficients[2]  #Significant!
WinterRainCoef = modelQ$coefficients[3] #NotSignificant

########
model4= lm(Price~AGST + HarvestRain + WinterRain + Age, data = wine) 
summary(model4)
SSE= sum(model4$residuals^2)
SSE

#Correlations
cor(wine$WinterRain, wine$Price)
#[1] 0.1366505

cor(wine$Age, wine$FrancePop)
#[1] -0.9944851

cor(wine)
#Multicoliniarity = highly correlation between two independent variables
# Correlation between independent and dependent variable is a good thing due to prediction purposes.

model5 = lm(Price~ AGST + HarvestRain + WinterRain , data=wine)
summary(model5)

cor(wine$HarvestRain, wine$WinterRain)


######## Using Testing Data ##############
wineTest = read.csv("wine_test.csv")
str(wineTest)
predictTest = predict(model4, newdata = wineTest)
predictTest
# 1        2 
# 6.768925 6.684910    Are they good? Lets see the Rsquared

SSE= sum((wineTest$Price - predictTest)^2)
SST = sum( (wineTest$Price - mean(wine$Price))^2)
1-SSE/SST
##[1] 0.7944278  It is good, but we shouldnt forget our test set is small
