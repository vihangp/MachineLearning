#1.1
emails = read.csv("emails.csv", stringsAsFactors = FALSE)

#1.2
sum(emails$spam)

#1.3
emails[2,'text']

#1.5
max(nchar(emails$text))

#1.6
which.min(nchar(emails$text))

#2.1
library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

#2.2
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)

#2.3
dtmDF = as.data.frame(as.matrix(dtm))
colnames(dtmDF) = make.names(colnames(dtmDF))
which.max(colSums(dtmDF))

#2.4
dtmDF$spam = emails$spam
colSums(dtmDF[dtmDF$spam == 0, - dtmDF$spam])[colSums(dtmDF[dtmDF$spam == 0, - dtmDF$spam]) >= 5000]

#2.5
spamDTM = dtmDF[dtmDF$spam == 1,]
spamDTM = spamDTM[, -spamDTM$spam]
head(spamDTM[,colSums(spamDTM) >= 1000])
