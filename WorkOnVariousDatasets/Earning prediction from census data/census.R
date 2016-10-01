census = read.csv("census.csv")

set.seed(2000)
library(caTools)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split == TRUE)
test = subset(census, split == FALSE)

model = glm(over50k ~ ., data = train, family='binomial')
summary(model)

predictions = predict(model, type = "response", newdata = test)
table(test$over50k, predictions >= 0.5)
(9051+1888)/nrow(test)
nrow(test[test$over50k == " <=50K",])/nrow(test)

library(ROCR)
auc = performance(prediction(predictions, test$over50k),"auc")
auc <- as.numeric(auc@y.values)

library(rpart)
cart = rpart(over50k ~ ., method = "class", data = train)
prp(cart)

predictions = predict(cart, type = "class", newdata = test)
table(test$over50k, predictions)
(9243 + 1596)/nrow(test)

library(ROCR)
predictions = predict(cart, newdata = test)
auc = performance(prediction(predictions[,2], test$over50k),"auc")
auc <- as.numeric(auc@y.values)

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
RF = randomForest(over50k ~ ., data=trainSmall)
predictions = predict(RF, newdata = test)
table(test$over50k, predictions)
(9614+1050)/nrow(test)

vu = varUsed(RF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(RF$forest$xlevels[vusorted$ix]))
varImpPlot(RF)
