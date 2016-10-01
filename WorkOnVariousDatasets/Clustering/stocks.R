library(caTools)
library(caret)

stocks = read.csv("StocksCluster.csv")
mean(stocks$PositiveDec)
summary(stocks)

set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(formula = PositiveDec ~ ., family = "binomial", data = stocksTrain)
predictions = predict(StocksModel)
confusion_matrix = table(stocksTrain$PositiveDec, predictions >= 0.5)
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / nrow(stocksTrain)

predictions = predict(StocksModel, newdata = stocksTest)
confusion_matrix = table(stocksTest$PositiveDec, predictions > 0.5)
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / nrow(stocksTest)

table(stocksTest$PositiveDec, 1)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

set.seed(144)
km = kmeans(x = normTrain, centers = 3)
nrow(limitedTrain[km$cluster == 1,])
nrow(limitedTrain[km$cluster == 2,])
nrow(limitedTrain[km$cluster == 3,])

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
length(clusterTest[clusterTest == 2])
