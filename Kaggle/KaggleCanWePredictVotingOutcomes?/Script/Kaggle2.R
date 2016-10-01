test<-read.csv("test2016.csv")
test<-read.csv("test2016.csv",na.strings = c("","NA"))
train<-read.csv("train2016.csv",na.strings = c("","NA"))
train<-read.csv("train2016.csv")
train_o<-na.omit(train)
library(rpart)
library(rpart.plot)
#CARTmodel1 = rpart(Party ~ . -USER_ID, data=PartyTrain)
#prp(CARTmodel1)
CARTmodel1 = rpart(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel  , data=PartyTrain)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(49+64)/nrow(PartyTest)
#0.53
#2 - without gender
CARTmodel1 = rpart(Party ~ YOB + Income + HouseholdStatus + EducationLevel , data=PartyTrain)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(59+50)/nrow(PartyTest)
#0.51

#3 - Logistic Model
# baseline accuracy
table(train$Party)/nrow(train)

str(train)
summary(train)
Log_Mod1 = glm(Party ~ YOB + HouseholdStatus + Q121699 + Q121699 + Q121700 + Q120194 + Q118232 + Q116197 + Q115611 + Q114517 + Q113181 + Q113181 + Q112270 + Q109244 + Q108950 + Q109244 + Q101596 + Q101596 + Q100689 + Q98869 + Q98578 - USER_ID, data=train, family=binomial)
summary(Log_Mod1)

PredTest1 = predict(Log_Mod1, newdata=test, type="response")
threshold = 0.5

PredTestLabels1 = as.factor(ifelse(PredTest1<threshold, "Democrat", "Republican"))
table(PredTestLabels1)/nrow(test)

#4- with some imp questions
CARTmodel1 = rpart(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244, data=PartyTrain_o,minbucket= 25)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest_o, type="class")
table(PartyTest_o$Party, Predict1)
(80+56)/nrow(PartyTest_o)
#0.64 on omitted data set and 0.62 on actual data set
#depends on how data is splitted for 1000 seed te value is 0.66 while for 3000 it is 0.56
#100 - 0.65
library(ROCR)
predictRoc<-predict(CARTmodel1,newdata = PartyTest_o)
predictRoc
pred<-prediction(predictRoc[,2],PartyTest_o$Party)
perf<-performance(pred,"tpr","fpr")
plot(perf)
as.numeric(performance(pred,"auc")@y.values)



#random forest
set.seed(1200)
library(randomForest)
RF_model = randomForest(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244  , data=PartyTrain_o,nodesize=25,ntree=200)
Predict1 <- predict(RF_model, newdata = PartyTest_o, type="class")
table(PartyTest_o$Party, Predict1)
(50+77)/nrow(PartyTest_o) 


#splitting actual data
library(caTools)
spl <- sample.split(train$Party, SplitRatio = 0.7)
PartyTrain <- subset(train, spl == TRUE)
PartyTest <- subset(train, spl == FALSE)

#glm with everything
str(train)
summary(train)
Log_Mod1 = glm(Party ~ .- USER_ID, data=PartyTrain, family=binomial)
summary(Log_Mod1)

PredTest1 = predict(Log_Mod1, newdata=PartyTest, type="response")
threshold = 0.5

PredTestLabels1 = as.factor(ifelse(PredTest1<threshold, "Democrat", "Republican"))
table(PredTestLabels1)/nrow(test)


#cart with some more questions

CARTmodel1 = rpart(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244 + Q122769 + Q120650 + Q118237 + Q117186 + Q116797 + Q116601 + Q116441 + Q115390 + Q115195 + Q112512 + Q111220 + Q109367 + Q109244 + Q108617 + Q107491 + Q106997 + Q106272 + Q105840 + Q102089 + Q101162 + Q101163 + Q101596  , data=PartyTrain)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(633+405)/nrow(PartyTest)

#0.62 on actual set without omittion

#data with omition
set.seed(800)
spl <- sample.split(train_o$Party, SplitRatio = 0.7)
PartyTrain_o <- subset(train_o, spl == TRUE)
PartyTest_o <- subset(train_o, spl == FALSE)


CARTmodel1 = rpart(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244 + Q122769 + Q120650 + Q118237 + Q116441 + Q109244 + Q107491 + Q105840 + Q102089   , data=PartyTrain_o)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest_o, type="class")
table(PartyTest_o$Party, Predict1)
(56+71)/nrow(PartyTest_o)
#0.60

#Imputing data
train<-read.csv("train2016.csv",na.strings = c("","NA"))
test<-read.csv("test2016.csv",na.strings = c("","NA"))
train <- subset(train, train$YOB <= 2003) 
#remove party column from training data set
train<-train[,!(names(train) %in% "Party")]
imputeData<-NULL
#merge test and training data to create a new dataset
imputeData<-merge(test,train,all.x =  TRUE,all.y = TRUE)

library(mice)
impute<-NULL
impute<-complete(mice(imputeData))
testUserId<-test$USER_ID
trainUserId<-train$USER_ID
imputeTest<-subset(impute,impute$USER_ID==testUserId)

for(i in 1:NROW(testUserId))
{
  imputeTest[i,]<-impute[impute$USER_ID==testUserId[i],]
}

imputeTrain<-subset(impute,impute$USER_ID==trainUserId)

for(i in 1:NROW(trainUserId))
{
  imputeTrain[i,]<-impute[impute$USER_ID==trainUserId[i],]
}


trainComp<-read.csv("train2016.csv",na.strings = c("","NA"))

trainComp <- subset(trainComp, trainComp$YOB <= 2003) 
imputeTrain$Party<-trainComp$Party
library(rpart)
library(rpart.plot)

library(caTools)
spl <- sample.split(imputeTrain$Party, SplitRatio = 0.7)
PartyTrain <- subset(imputeTrain, spl == TRUE)
PartyTest <- subset(imputeTrain, spl == FALSE)

CARTmodel1 = rpart(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244  , data=PartyTrain,cp=0.03)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(568+446)/nrow(PartyTest) #0.645

CARTmodel1 = rpart(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244  , data=imputeTrain)
PredictCART <- predict(CARTmodel1, newdata = imputeTest, type="class")
table(PredictCART)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredictCART)
write.csv(MySubmission, "SubmissionCARTWithImputations.csv", row.names=FALSE)


CARTmodel1 = rpart(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel, data=PartyTrain)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(656+233)/nrow(PartyTest) #0.566


CARTmodel1 = rpart(Party ~ . -USER_ID, data=PartyTrain)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(569+437)/nrow(PartyTest) #0.640

#random forest
set.seed(1200)
library(randomForest)
RF_model = randomForest(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244  , data=PartyTrain,nodesize=25,ntree=200)
Predict1 <- predict(RF_model, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(583+425)/nrow(PartyTest) 

RF_model = randomForest(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244  , data=imputeTrain,nodesize=25,ntree=200)
PredictCART <- predict(RF_model, newdata = imputeTest, type="class")
table(PredictCART)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredictCART)
write.csv(MySubmission, "SubmissionRFWithImputations.csv", row.names=FALSE)
#second submission with RF gives 0.6128

set.seed(3000)
library(randomForest)
RF_model = randomForest(Party ~ YOB + Gender + Income + HouseholdStatus + EducationLevel + Q118232 + Q115611 + Q113181 + Q109244  , data=PartyTrain,nodesize=200,ntree=1000)
Predict1 <- predict(RF_model, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(589+431)/nrow(PartyTest) 
imputeCompleteCopy<-impute


#########################################################################################
#imputing data without queston variables
train<-read.csv("train2016.csv",na.strings = c("","NA"),stringsAsFactors = FALSE)
test<-read.csv("test2016.csv",na.strings = c("","NA"),stringsAsFactors = FALSE)
train <- subset(train, train$YOB <= 2003) 
#remove party column from training data set
train<-train[,!(names(train) %in% "Party")]
imputeData<-NULL
#merge test and training data to create a new dataset
imputeData<-merge(test,train,all.x =  TRUE,all.y = TRUE)
imputeData<-imputeData[1:6]

library(mice)
imputeNew<-NULL
imputeNew<-complete(mice(imputeData))
imputeNewCopy<-imputeNew
imputeNew[7:107]<-imputeData[7:107]
for(i in 3:107)
{
  imputeNew[i]<-as.factor(imputeNew[[i]])
}

testUserId<-test$USER_ID
trainUserId<-train$USER_ID
imputeTestNew<-subset(imputeNew,imputeNew$USER_ID==testUserId)


for(i in 1:NROW(testUserId))
{
  imputeTestNew[i,]<-imputeNew[imputeNew$USER_ID==testUserId[i],]
}

imputeTrainNew<-subset(imputeNew,imputeNew$USER_ID==trainUserId)

for(i in 1:NROW(trainUserId))
{
  imputeTrainNew[i,]<-imputeNew[imputeNew$USER_ID==trainUserId[i],]
}

trainComp<-read.csv("train2016.csv",na.strings = c("","NA"))

trainComp <- subset(trainComp, trainComp$YOB <= 2003) 
imputeTrainNew$Party<-trainComp$Party

#cart on imputed data for 6 variables only

library(rpart)
library(rpart.plot)

library(caTools)
set.seed(1000)
spl <- sample.split(imputeTrainNew$Party, SplitRatio = 0.7)
PartyTrain <- subset(imputeTrainNew, spl == TRUE)
PartyTest <- subset(imputeTrainNew, spl == FALSE)

CARTmodel1 = rpart(Party ~ .-USER_ID , data=PartyTrain)
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(641+340)/nrow(PartyTest) #0.645

set.seed(3000)
library(randomForest)
RF_model = randomForest(Party ~ .-USER_ID , data=PartyTrain,nodesize=200,ntree=1000)
Predict1 <- predict(RF_model, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(659+325)/nrow(PartyTest) 
imputeCompleteCopy<-impute

library(caret)
library(e1071)
#decide the number of folds
numFolds<-trainControl(method = "cv",number = 10)
cpGrid<-expand.grid(.cp=seq(0.01,0.5,0.01))
train( Party ~ .-USER_ID, data= PartyTrain,method="rpart", trControl= numFolds,tuneGrid=cpGrid)
CARTmodel1 = rpart(Party ~.-USER_ID, data=PartyTrain,method = "class")
prp(CARTmodel1)
summary(CARTmodel1)
Predict1 <- predict(CARTmodel1, newdata = PartyTest, type="class")
table(PartyTest$Party, Predict1)
(641+340)/nrow(PartyTest) #0.645

PredictCART <- predict(CARTmodel1, newdata = imputeTestNew, type="class")
table(PredictCART)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredictCART)
write.csv(MySubmission, "SubmissionCARTDemographicImputation.csv", row.names=FALSE)
#0.61 3rd submission

#####################################################################################












