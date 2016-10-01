songs = read.csv("songs.csv")

# Part 1 Understanding the data
nrow(songs[songs$year == 2010,])
nrow(songs[songs$artistname == "Michael Jackson",])
songs[songs$artistname == "Michael Jackson" & songs$Top10 == 1,]$songtitle
unique(songs$timesignature)
table(songs$timesignature)
songs[which.max(songs$tempo),"songtitle"]

# Part 2 Building the model
songs_train = songs[songs$year < 2010,]
songs_test = songs[songs$year == 2010,]

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songs_train = songs_train[ , !(names(songs_train) %in% nonvars) ]
songs_test = songs_test[ , !(names(songs_test) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=songs_train, family=binomial)
summary(SongsLog1)

# Part 3 MultiColinearity

cor(songs$loudness, songs$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=songs_train, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . - energy, data=songs_train, family=binomial)
summary(SongsLog3)

# Part 4 Model Validation

predictions = predict(SongsLog3, newdata = songs_test, type = "response")
summary(predictions)
confusion_matrix = table(songs_test$Top10, predictions > 0.45)
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / nrow(songs_test)
baseline_accuracy = (nrow(songs_test) - sum(songs_test$Top10)) / nrow(songs_test)
sensitivity = confusion_matrix[2,2] / sum(confusion_matrix[2,])
specificity = confusion_matrix[1,1] / sum(confusion_matrix[1,])

