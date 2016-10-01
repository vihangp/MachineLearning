loans_dataset = read.csv("loans.csv")

na_data = loans_dataset[rowSums(is.na(loans_dataset)) > 0,]
sum(na_data$not.fully.paid)/nrow(na_data)

loans_dataset = read.csv("loans_imputed.csv")
set.seed(144)
library(caTools)
split = sample.split(loans_dataset$not.fully.paid, SplitRatio = 0.7)
train = subset(loans_dataset, split == TRUE)
test = subset(loans_dataset, split == FALSE)

model = glm(not.fully.paid ~ . , data=train, family=binomial)
summary(model)

predictions = predict(model, newdata = test, type = "response")
confusion_matrix = table(test$not.fully.paid, predictions >= 0.5)
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / nrow(test)
sensitivity = confusion_matrix[2,2] / sum(confusion_matrix[2,])
specificity = confusion_matrix[1,1] / sum(confusion_matrix[1,])
baseline_accuracy = (nrow(test) - sum(test$not.fully.paid)) / nrow(test)

test$predicted.risk = predictions

library(ROCR)
auc = performance(prediction(predictions, test$not.fully.paid),"auc")
auc <- as.numeric(auc@y.values)

model2 = glm(not.fully.paid ~ int.rate , data=train, family=binomial)
summary(model2)

predictions = predict(model2, newdata = test, type = "response")
max(predictions)
confusion_matrix = table(test$not.fully.paid, predictions >= 0.5)
auc = performance(prediction(predictions, test$not.fully.paid),"auc")
auc <- as.numeric(auc@y.values)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)*10
high_interest = test[test$int.rate >= 0.15,]
mean(high_interest$profit)

cutoff = sort(high_interest$predicted.risk, decreasing=FALSE)[100]
selected_loans = high_interest[high_interest$predicted.risk <= cutoff,]
sum(selected_loans$not.fully.paid)
