# Part1

gerber = read.csv("gerber.csv")
mean(gerber$voting)
mean(gerber[gerber$hawthorne == 1,]$voting)
mean(gerber[gerber$civicduty== 1,]$voting)
mean(gerber[gerber$self == 1,]$voting)
mean(gerber[gerber$neighbors == 1,]$voting)

model = glm(voting ~ hawthorne + civicduty + self + neighbors, data = gerber)
summary(model)

predictions = predict(model)
table(gerber$voting, predictions > 0.3)
accuracy = (134513 + 51966) / nrow(gerber)
table(gerber$voting, predictions > 0.5)
235388 / nrow(gerber)

library(ROCR)
auc = performance(prediction(predictions, gerber$voting),"auc")
auc <- as.numeric(auc@y.values)

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel2)
CARTmodel3 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel4 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel3, digits=6)
prp(CARTmodel4, digits=6)

model = glm(voting ~ control + sex, data = gerber)
summary(model)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
