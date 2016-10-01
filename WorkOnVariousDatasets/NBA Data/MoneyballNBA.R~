#Moneyball statistics applied to NBA
setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week2/Unit2/02MoneyballNBA")

NBA=read.csv("NBA_train.csv")
str(NBA)
#'data.frame':	835 obs. of  20 variables:
#Playoffs: Team made it to the playofs
#W: Number of wins for the team
#PTS: points
#oppPTS:opponent points

table(NBA$W, NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff , NBA$W)
WinsReg = lm(W~PTSdiff, data=NBA)
summary(WinsReg)

#Predict Point Score ..PTS(dependent variable)
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg) #Rsquared .8992

#Lets see the residuals
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)    #[1] 28394314
RMSE = sqrt(SSE/nrow(NBA))          #[1] 184.4049 The error on average
mean(NBA$PTS)                       #[1] 8370.24  average of points in the season
summary(PointsReg)                  #To see which variable to remove first.

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2)      #Multiple R-squared:  0.8991

PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsReg3)     #Multiple R-squared:  0.8991

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)     #Multiple R-squared:  0.8991

#Calculate the new Error
SSE_4 = sum(PointsReg4$residuals^2)    #[1] 28421465
RMSE_4 = sqrt(SSE_4/nrow(NBA))         #[1] 184.493


###Predictions
NBA_test = read.csv("NBA_test.csv")

PointsPredictions = predict(PointsReg4, newdata=NBA_test)
#Calculate the error for the out of sample values
SSE= sum((PointsPredictions - NBA_test$PTS)^2)
SST= sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 -SSE/SST                   #[1] 0.8127142
R2
RMSE = sqrt(SSE/nrow(NBA_test))  #[1] 196.3723
