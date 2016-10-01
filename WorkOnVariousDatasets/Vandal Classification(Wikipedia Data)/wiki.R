# 1.1
wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
nrow(wiki[wiki$Vandal == "1",])

#1.2
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

#1.3
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

#1.4
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#1.5
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, split==TRUE)
test = subset(wikiWords, split==FALSE)

table(test$Vandal)
baseline_accuracy = 618/(618+545)

#1.6
library(rpart)
library(rpart.plot)

CART = rpart(Vandal ~ ., data=train, method="class")
predictions = predict(CART, newdata = test, type = "class")
table(test$Vandal,predictions)
accuracy = (618+12)/nrow(test)

#2.1
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)


#2.2
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
CART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
predictions2 = predict(CART2, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal,predictions2)
accuracy = (609+57)/nrow(test)

#2.3
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

#2.3
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
CART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
predictions2 = predict(CART2, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal,predictions2)
accuracy = (514+248)/nrow(test)

#3.1
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)
CART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
predictions3 = predict(CART3, newdata = wikiTest3, type = "class")
table(wikiTest3$Vandal,predictions3)
accuracy = (595+241)/nrow(test)
