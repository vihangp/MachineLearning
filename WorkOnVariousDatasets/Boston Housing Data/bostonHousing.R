
library(caTools)

#Housing data in Boston

boston = read.csv("boston.csv")
str(boston)

#Chas river, and MIT
plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS== 1], boston$LAT[boston$CHAS==1], col="blue", pch = 19)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531], col="red",pch=19)

#Polution
summary(boston$NOX)
points(boston$LON[boston$NOX>=.55],boston$LAT[boston$NOX>=.55], col="green", pch=19)

#Restart plot
plot(boston$LON,boston$LAT)

#Prices
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

plot(boston$LAT,boston$MEDV)
plot(boston$LON,boston$MEDV)

#Regression Model
latlonlm = lm(MEDV~ LAT + LON, data=boston)
summary(latlonlm)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)
points(boston$LON[latlonlm$fitted.values>=21.2],
       boston$LAT[latlonlm$fitted.values>=21.2],
       col="blue",
       pch="$"
       )

######## REGRESSION TREES ##############
library(rpart)
library(rpart.plot)

#FIRST MODEL
latlontree = rpart(MEDV~LAT + LON, data=boston)
prp(latlontree)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)
fitted.values = predict(latlontree)
points(boston$LON[fitted.values>=21.2],
       boston$LAT[fitted.values>=21.2],
       col="blue",
       pch="$"
)
#SECOND MODEL
latlontree = rpart(MEDV~LAT + LON, data=boston, minbucket=50)
plot(latlontree)
text(latlontree)
plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

########### 

library(caTools)
set.seed(123)
split =sample.split(boston$MEDV,SplitRatio = 0.7)
train = subset(boston,split==T)
test = subset(boston,split==F)
str(train)
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN +INDUS + CHAS + NOX +
              RM + AGE + DIS +RAD + TAX +PTRATIO,train)
summary(linreg)
linreg.pred = predict(linreg,test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
linreg.sse  #[1] 3037.088

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN +INDUS + CHAS + NOX +
              RM + AGE + DIS +RAD + TAX +PTRATIO,train)
prp(tree)
tree.predict = predict(tree,test)
tree.sse= sum((tree.predict - test$MEDV)^2)
tree.sse  #[1] 4328.988

##########CROSS VALIDATION AND Complexity Parameter (CP) ############

library(caret)
library(e1071)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = (0:10)*0.001) #Values for the Cross validation
tr = train(MEDV~ LAT + LON + CRIM + ZN + INDUS + CHAS+ NOX + 
             RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", 
           trControl= tr.control, tuneGrid =cp.grid) 

tr

best.tree= tr$finalModel
prp(best.tree)
best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)  #[1] 3679.337
best.tree.sse

