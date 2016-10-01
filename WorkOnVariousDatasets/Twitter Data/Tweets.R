#Text Mining
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)

tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

#We need to create a corpus for the tweets
corpus = Corpus(VectorSource(tweets$Tweet))
corpus[[1]][1]


#######PREPROCESSING########
#To Lowercases
corpus = tm_map(corpus, tolower) 
#Remove Punctation
corpus = tm_map(corpus, removePunctuation)
#Remove Stopwords
corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))
#Stemming
corpus = tm_map(corpus, stemDocument)

#Create a Matrix
corpus = tm_map(corpus, PlainTextDocument)
frequencies = DocumentTermMatrix(corpus)
frequencies

inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies, lowfreq = 20)
sparse = removeSparseTerms(frequencies, 0.995) #Keep words that appear in .5% of our tweets
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative

#Train and Test
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split ==FALSE)

#QuickQuestion
# findFreqTerms(frequencies, lowfreq =80)
# [1] "itun"   "iphone" "new"   

#Model RPART
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative~., data=trainSparse, method="class")
prp(tweetCART)

predictCART = predict(tweetCART, newdata = testSparse, type="class")
cm = table(testSparse$Negative, predictCART) 
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #[1] 0.87

#BASELINE
table(testSparse$Negative)
300/355

#Model RF
library(randomForest)
set.seed(123)
tweetRF= randomForest(Negative~., data=trainSparse)
predictRF = predict(tweetRF, newdata= testSparse)
table(testSparse$Negative, predictRF)
cm = table(testSparse$Negative, predictRF) 
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #[1] 0.8676056338

#Model GLM
tweetLog = glm(Negative ~., data= trainSparse,family="binomial")
predictions = predict(tweetLog, newdata=testSparse, type="response")
cm = table(tweetLog$Negative, predictions) 
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #[1] 0.8253521127
