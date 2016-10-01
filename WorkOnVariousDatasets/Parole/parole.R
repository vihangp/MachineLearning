parole_dataset = read.csv("parole.csv")
sum(parole_dataset$violator)

parole_dataset$crime = as.factor(parole_dataset$crime)
parole_dataset$state = as.factor(parole_dataset$state)

set.seed(144)
library(caTools)
split = sample.split(parole_dataset$violator, SplitRatio = 0.7)
train = subset(parole_dataset, split == TRUE)
test = subset(parole_dataset, split == FALSE)

model = glm(violator ~ . , data=train, family=binomial)
summary(model)

predictions = predict(model, newdata = test, type = "response")
max(predictions)
confusion_matrix = table(test$violator, predictions >= 0.5)
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / nrow(test)
sensitivity = confusion_matrix[2,2] / sum(confusion_matrix[2,])
specificity = confusion_matrix[1,1] / sum(confusion_matrix[1,])
baseline_accuracy = (nrow(test) - sum(test$violator)) / nrow(test)

library(ROCR)
auc = performance(prediction(predictions, test$violator),"auc")
auc <- as.numeric(auc@y.values)

#onsider a parolee who is male, of white race, aged 50 years at prison release, 
#from the state of Maryland, served 3 months, had a maximum sentence of 12 months, 
#did not commit multiple offenses, and committed a larceny
male = 1
race = 1
age = 50
state2 = 0
state3 = 0
state4 = 0
time.Served = 3
max.sentence = 12
multiple.offenses = 0
crime2 = 1
crime3 = 0
crime4 = 0
odds = exp(model$coefficients[1] + male*model$coefficients['male']+ race*model$coefficients['race']+ age*model$coefficients['age']+ state2*model$coefficients['state2']+ state3*model$coefficients['state3']+ state4*model$coefficients['state4']+ time.Served*model$coefficients['time.served']+ max.sentence*model$coefficients['max.sentence']+ multiple.offenses*model$coefficients['multiple.offenses']+ crime2*model$coefficients['crime2']+ crime3*model$coefficients['crime3']+ crime4*model$coefficients['crime4'])
prob = odds / (1 + odds)
