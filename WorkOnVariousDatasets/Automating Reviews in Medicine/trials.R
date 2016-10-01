#1.1
trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
max(nchar(trials$abstract))

#1.2
nrow(trials[nchar(trials$abstract) == 0,])

#1.3
trials[which.min(nchar(trials$title)),'title']

#2.1
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitleFrame = as.data.frame(as.matrix(dtmTitle))

corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTAbstract = DocumentTermMatrix(corpusAbstract)
dtmTAbstract = removeSparseTerms(dtmTAbstract, 0.95)
dtmAbstractFrame = as.data.frame(as.matrix(dtmTAbstract))

#2.3 
sort(colSums(dtmAbstractFrame))

#3.1
colnames(dtmTitleFrame) = paste0("T", colnames(dtmTitleFrame))
colnames(dtmAbstractFrame) = paste0("A", colnames(dtmAbstractFrame))

#3.2
dtm = cbind(dtmTitleFrame, dtmAbstractFrame)
dtm$trial = trials$trial

#3.3
library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)
table(dtm$trial)
baseline_accuracy = 1043/nrow(dtm)

#3.4
library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)

#3.5
predictions = predict(trialCART)
max(predictions[,2])

#3.7
predictions = predict(trialCART, type = "class")
table(train$trial,predictions)
accuracy = (631+441)/nrow(train)
sensitivity = 441/(441+131)
specificity = 631/(631+99)

#4.1
predictions = predict(trialCART, newdata = test, type = "class")
table(test$trial,predictions)
accuracy = (261+162)/nrow(test)

#4.2
library(ROCR)
predictions = predict(trialCART, newdata = test, type = "response")
auc = performance(prediction(predictions[,2], labels = test$trial), measure = 'auc')
auc <- as.numeric(auc@y.values)
