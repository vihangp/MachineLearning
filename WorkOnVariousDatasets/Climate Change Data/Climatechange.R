
#ClimateChange

climate = read.csv("climate_change.csv")
str(climate)

#Problem 1.1 - Creating Our First Model
training = subset(climate, Year<= 2006)
testing = subset(climate, Year>2006)

fit1 = lm(Temp ~MEI+ CO2+ CH4+ N2O+ CFC.11+ CFC.12+ TSI+ Aerosols, data = training)
summary(fit1)
#Multiple R-squared:  0.7509

#Problem 1.2 - Creating Our First Model
#Significant .. all but CH4, and N20.

#Problem 2.1 - Understanding the Model
#All of the gas concentration variables reflect human development - N2O and CFC.11 are correlated with other variables in the data set.

#Problem 2.2 - Understanding the Model
cor(training)
#C02, CH4, CFC.12
#CH4, CFC.12

#Problem 3 - Simplifying the Model
fit2 = lm(Temp ~ MEI+N2O+ TSI+ Aerosols, data = training)
summary(fit2)
# coefficient 2.532e-02
#Mult Rsquared 0.7261

#Problem 4 - Automatically Building the Model
fit3= step(fit1)
summary(fit3)
# Rsquared 0.7508
#CH4

#Problem 5 - Testing on Unseen Data
predictStep = predict(fit3,testing)
summary(predictStep)
SSE = sum((testing$Temp - predictStep)^2)
SST = sum((mean(training$Temp) - testing$Temp)^2) 
Rsquared = 1 - (SSE/SST)
Rsquared

#############

##PISA Dataset 

pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

#Problem 1.1 - Dataset size
nrow(pisaTrain)

#Problem 1.2 - Summarizing the dataset
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#Problem 1.3 - Locating missing values
summary(pisaTrain)
colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]

#Problem 1.4 - Removing missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)

#Problem 2.1 - Factor variables
head(pisaTrain$raceeth)
head(pisaTrain$grade)
#raceeth
#grade

#Problem 2.2 - Unordered factors in regression models
fit1=lm(readingScore ~ as.factor(raceeth),pisaTrain) #This should be releveled
ind=model.matrix(~ factor(pisaTrain$raceeth) - 1)
#All but white

#Problem 2.3 - Example unordered factors
#Answer: Not asian, and All

#Problem 3.1 - Building a model
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ .,pisaTrain)
summary(lmScore) #Multiple R-squared:  0.3251

#Problem 3.2 - Computing the root-mean squared error of the model
summary(lmScore)$sigma

#Problem 3.3 - Comparing predictions for similar students
lmScore$coefficients[2]*2 

#Problem 3.4 - Interpreting model coefficients
summary(lmScore)

#Problem 4.1 - Predicting on unseen data
pred = predict(lmScore, newdata = pisaTest)
max(pred)-min(pred)

#Problem 4.2 - Test set SSE and RMSE
SSE = sum((pisaTest$readingScore - pred)^2)
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2) 
RMSE = sqrt(SSE/nrow(pisaTest))
# or RMSE = sqrt(mean((predTest-pisaTest$readingScore)^2))

#Problem 4.3 - Baseline prediction and test-set SSE
Baseline = mean(pisaTrain$readingScore)
Baseline

SST = sum((Baseline - pisaTest$readingScore)^2) 
SST

#Problem 4.4 - Test-set R-squared
Rsquared = 1-SSE/SST
Rsquared

##########################################################
##########################################################

#### FLU EPIDEMICS

flu = read.csv("FluTrain.csv")

#Problem 1.1 - Understanding the Data
flu[which.max(flu$ILI),]
flu[which.max(flu$Queries),]

#Problem 1.2 - Understanding the Data
hist(flu$ILI) #Right skew

#Problem 1.3 - Understanding the Data
plot(log(flu$ILI),flu$Queries) #Positive linear relationship

#Problem 2.1 - Linear Regression Model
# log(ILI) = intercept + coefficient x Queries, where the coefficient is positive log(ILI) = intercept + coefficient x Queries, where the coefficient is positive 

#Problem 2.2 - Linear Regression Model
FluTrend1 = lm(log(ILI)~Queries, data = flu)
summary(FluTrend1)[8]

#Problem 2.3 - Linear Regression Model
cor(flu$Queries,log(flu$ILI))^2

#Problem 3.1 - Performance on the Test Set
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

FluTest$prediction = PredTest1
FluTest
FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),4]

#Problem 3.2 - Performance on the Test Set
(FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),2]-FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),4])/FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),2]

#Problem 3.3 - Performance on the Test Set

RMSE = sqrt(mean((PredTest1-FluTest$ILI)^2))
RMSE

#Problem 4.1 - Training a Time Series Model
install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(flu$ILI), -2, na.pad=TRUE)
flu$ILILag2 = coredata(ILILag2)
head(flu)
summary(flu)

#Problem 4.2 - Training a Time Series Model
plot(log(flu$ILILag2), log(flu$ILI)) #Positive relationship

#Problem 4.3 - Training a Time Series Model
FluTrend2 = lm(log(ILI)~Queries + log(ILILag2), data = flu)
summary(FluTrend2)
summary(FluTrend2)[8]

#Problem 4.4 - Training a Time Series Model
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest) #2

#Problem 5.2 - Evaluating the Time Series Model in the Test Set
#Second to last observation
#last observation

#Problem 5.3 - Evaluating the Time Series Model in the Test Set
FluTest$ILILag2[1:2]= flu$ILI[416:417]
FluTest$ILILag2[1:2]

#Problem 5.4 - Evaluating the Time Series Model in the Test Set
PredTest2 = exp(predict(FluTrend2, FluTest))

RMSE = sqrt(mean((PredTest2-FluTest$ILI)^2))
RMSE #Lower is better

#Problem 5.5 - Evaluating the Time Series Model in the Test Set

