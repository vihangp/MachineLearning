letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
library(caTools)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

# predict not B

baseline_accuracy = nrow(test[test$isB == "FALSE",])/nrow(test)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predictions = predict(CARTb, newdata=test, type = "class")
table(predictions, test$isB)
accuracy = (1118 + 340) / nrow(test)

library(randomForest)
set.seed(1000)
RFb = randomForest(isB ~ . - letter, data = train)
predictions = predict(RFb, newdata=test, type = "class")
table(predictions, test$isB)
accuracy = (1165 + 374) / nrow(test)

letters = read.csv("letters_ABPR.csv")
letters$letter = as.factor( letters$letter )
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)
summary(train)
baseline_accuracy = nrow(test[test$letter == "P",])/nrow(test)

CART = rpart(letter ~ . , data=train, method="class")
predictions = predict(CART, newdata=test, type = "class")
table(test$letter, predictions)
(348 + 318 + 363 + 340) / nrow(test)

RF = randomForest(letter ~ . , data = train)
predictions = predict(RF, newdata=test, type = "class")
table(test$letter, predictions)
accuracy = (391 + 381 + 393 + 366) / nrow(test)
