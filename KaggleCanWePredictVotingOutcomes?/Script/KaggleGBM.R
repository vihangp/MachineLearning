#### GBM on complete data set
library(gbm)
library(dplyr)
library(caTools)

imputeData<-imputeCompleteCopy
imputeData<-select(imputeData,-USER_ID)

testUserId<-test$USER_ID
trainUserId<-train$USER_ID
imputeTest<-subset(imputeData,imputeData$USER_ID==testUserId)

for(i in 1:NROW(testUserId))
{
  imputeTest[i,]<-imputeData[imputeData$USER_ID==testUserId[i],]
}

imputeTrain<-subset(imputeData,imputeData$USER_ID==trainUserId)

for(i in 1:NROW(trainUserId))
{
  imputeTrain[i,]<-imputeData[imputeData$USER_ID==trainUserId[i],]
}
imputeTrain <- subset(imputeTrain, imputeTrain$YOB <= 2003) 

trainComp<-read.csv("train2016.csv",na.strings = c("","NA"))
library(stringi)
library(stringr)
trainComp <- subset(trainComp, trainComp$YOB <= 2003) 
imputeTrain$Party<-trainComp$Party
part<-c("Democrat"=0,"Republican"=1)
imputeTrainGBM<-imputeTrain %>% 
  mutate(Party=str_replace_all(Party,part))
imputeTrainGBM$Party<-as.factor(imputeTrainGBM$Party)
Party<-imputeTrainGBM$Party
imputeTrain<-select(imputeTrain,-Party)


#####Running GBM

imputeTrain$Party<-ifelse(imputeTrain$Party=="Democrat",TRUE,FALSE)
Party<-imputeTrain$Party
imputeTrain<-select(imputeTrain,-Party)


ntrees=5000
row_train<-nrow(imputeTrain)
KaggleGBM=gbm.fit(x=imputeTrain
              ,y=Party
              ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
              ,n.trees = ntrees             #select large value for tree and then later on prune it
              ,shrinkage = 0.01             #set smaller value for shrinkaggive better performance
              #but it may result in taking a longer time to execute
              ,interaction.depth = 3        #use cross validation to choose interaction depth
              ,n.minobsinnode = 10          #small value results in overfitting of data
              ,nTrain = round(row_train*0.8)      #use to select number of trees in the end
              #,var.monotone=c()
              #can help to smoothen the curves and help with overfitting            
              ,verbose=TRUE)                #print the output in the end


summary(KaggleGBM)
gbm.perf(KaggleGBM)

for(i in 1:length(KaggleGBM$var.names))
{
  plot(KaggleGBM,i.var=i
       ,ntree=gbm.perf(KaggleGBM,plot.it = FALSE),type="response")
}


#make predictionns

#test set predictions
TestPredictions<-predict(object = KaggleGBM,newdata = test
                         ,n.trees = gbm.perf(KaggleGBM,plot.it = FALSE),
                         type = "response")
#training set predictions
TrainPredictions<-predict(object = KaggleGBM,newdata = train
                          ,n.trees = gbm.perf(KaggleGBM,plot.it = FALSE),
                          type = "response")
TestPredictions<-round(TestPredictions)
TrainPredictions<-round(TrainPredictions)

#train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
TrainPredictions<-ifelse(TrainPredictions==1,"Democrat","Republican")
TrainPredictions<-as.factor(TrainPredictions)
table(trainComp$Party,TrainPredictions)
(2194+1235)/nrow(trainComp)

TestPredictions<-ifelse(TestPredictions==1,"Democrat","Republican")
TestPredictions<-as.factor(TestPredictions)
table(TestPredictions)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = TestPredictions)
write.csv(MySubmission, "SubmissionGBMwithCompleteImputed.csv", row.names=FALSE)
#result 0.61 



#gbm 2 GBM on complete training set

train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
Party<-train$Party
train<-select(train,-Party)

ntrees=5000
row_train<-nrow(train)
KaggleGBM1=gbm.fit(x=train
                  ,y=Party
                  ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
                  ,n.trees = ntrees             #select large value for tree and then later on prune it
                  ,shrinkage = 0.01             #set smaller value for shrinkaggive better performance
                  #but it may result in taking a longer time to execute
                  ,interaction.depth = 3        #use cross validation to choose interaction depth
                  ,n.minobsinnode = 10          #small value results in overfitting of data
                  ,nTrain = round(row_train*0.8)      #use to select number of trees in the end
                  #,var.monotone=c()
                  #can help to smoothen the curves and help with overfitting            
                  ,verbose=TRUE)                #print the output in the end

summary(KaggleGBM1)
gbm.perf(KaggleGBM1)

for(i in 1:length(KaggleGBM1$var.names))
{
  plot(KaggleGBM1,i.var=i
       ,ntree=gbm.perf(KaggleGBM1,plot.it = FALSE),type="response")
}

#make predictionns

#test set predictions
TestPredictions<-predict(object = KaggleGBM1,newdata = test
                         ,n.trees = gbm.perf(KaggleGBM1,plot.it = FALSE),
                         type = "response")
#training set predictions
TrainPredictions<-predict(object = KaggleGBM1,newdata = train
                          ,n.trees = gbm.perf(KaggleGBM1,plot.it = FALSE),
                          type = "response")
TestPredictions<-round(TestPredictions)
TrainPredictions<-round(TrainPredictions)

#train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
TrainPredictions<-ifelse(TrainPredictions==1,"Democrat","Republican")
TrainPredictions<-as.factor(TrainPredictions)
table(trainComp$Party,TrainPredictions)
(2067+1695)/nrow(trainComp)

TestPredictions<-ifelse(TestPredictions==1,"Democrat","Republican")
TestPredictions<-as.factor(TestPredictions)
table(TestPredictions)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = TestPredictions)
write.csv(MySubmission, "SubmissionGBMwithOriginalTraining.csv", row.names=FALSE)
#result 0.64 


################################################################################
#GBM 3rd version with imp questions---------------------------------------------

summary(KaggleGBM1)
gbm.perf(KaggleGBM1)

for(i in 1:length(KaggleGBM1$var.names))
{
  plot(KaggleGBM1,i.var=i
       ,ntree=gbm.perf(KaggleGBM1,plot.it = FALSE),type="response")
}

all<-select(train,Q109244,EducationLevel,Income,HouseholdStatus,YOB,Q115611,Q98197,Q113181,Q101163,Q118232,Q98869,Q110740,Gender,Q120379,Q112478,Q101596,Q123621,Q116953,Q120194,Q105655,Q108855,Q104996,Q106997,Q100689,Q119851,Q116881)
alltest<-select(test,Q109244,EducationLevel,Income,HouseholdStatus,YOB,Q115611,Q98197,Q113181,Q101163,Q118232,Q98869,Q110740,Gender,Q120379,Q112478,Q101596,Q123621,Q116953,Q120194,Q105655,Q108855,Q104996,Q106997,Q100689,Q119851,Q116881)
ntrees=5000
row_train<-nrow(all)
KaggleGBM2=gbm.fit(x=all
                   ,y=Party
                   ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
                   ,n.trees = ntrees             #select large value for tree and then later on prune it
                   ,shrinkage = 0.01             #set smaller value for shrinkag give better performance
                   #but it may result in taking a longer time to execute
                   ,interaction.depth = 3        #use cross validation to choose interaction depth
                   ,n.minobsinnode = 10          #small value results in overfitting of data
                   ,nTrain = round(row_train*0.8)      #use to select number of trees in the end
                   #,var.monotone=c()
                   #can help to smoothen the curves and help with overfitting            
                   ,verbose=TRUE)                #print the output in the end

summary(KaggleGBM2)
gbm.perf(KaggleGBM2)

for(i in 1:length(KaggleGBM2$var.names))
{
  plot(KaggleGBM2,i.var=i
       ,ntree=gbm.perf(KaggleGBM2,plot.it = FALSE),type="response")
}

#make prediction

#test set predictions
TestPredictions2<-predict(object = KaggleGBM2,newdata = alltest
                         ,n.trees = gbm.perf(KaggleGBM2,plot.it = FALSE),
                         type = "response")
#training set predictions
TrainPredictions2<-predict(object = KaggleGBM2,newdata = all
                          ,n.trees = gbm.perf(KaggleGBM2,plot.it = FALSE),
                          type = "response")
TestPredictions2<-round(TestPredictions2)
TrainPredictions2<-round(TrainPredictions2)

#train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
TrainPredictions2<-ifelse(TrainPredictions2==1,"Democrat","Republican")
TrainPredictions2<-as.factor(TrainPredictions2)
table(trainComp$Party,TrainPredictions2)
(2022+1482)/nrow(trainComp)

TestPredictions2<-ifelse(TestPredictions2==1,"Democrat","Republican")
TestPredictions2<-as.factor(TestPredictions2)
table(TestPredictions2)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = TestPredictions2)
write.csv(MySubmission, "SubmissionGBMwithOriginalTrainingAndLessVariables.csv", row.names=FALSE)






#Q109244,EducationLevel,Income,HouseholdStatus,YOB,Q115611,Q98197,Q113181,Q101163,Q118232,Q98869,Q110740,Gender,Q120379,Q112478,Q101596,Q123621,Q116953,Q120194,Q105655,Q108855,Q104996,Q106997,Q100689,Q119851,Q116881                 


############################################################################################
#gbm 4----------------------------------------- 
#GBM on complete training set with larger value of minobisnode to avoid overfitting

train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
Party<-train$Party
train<-select(train,-Party)
#minobsinnode value =20-0.6947 on training set,50-0.6866,100-0.69,30-0.696613,25-0.7012044,original entry value =10 
ntrees=5000
row_train<-nrow(train)
KaggleGBM3=gbm.fit(x=train
                   ,y=Party
                   ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
                   ,n.trees = ntrees             #select large value for tree and then later on prune it
                   ,shrinkage = 0.01             #set smaller value for shrinkaggive better performance
                   #but it may result in taking a longer time to execute
                   ,interaction.depth = 3        #use cross validation to choose interaction depth
                   ,n.minobsinnode = 25         #small value results in overfitting of data
                   ,nTrain = round(row_train*0.8)      #use to select number of trees in the end
                   #,var.monotone=c()
                   #can help to smoothen the curves and help with overfitting            
                   ,verbose=TRUE)                #print the output in the end

summary(KaggleGBM3)
gbm.perf(KaggleGBM3)
best.iter <- gbm.perf(KaggleGBM3,method="OOB")
print(best.iter)

for(i in 1:length(KaggleGBM3$var.names))
{
  plot(KaggleGBM3,i.var=i
       ,ntree=gbm.perf(KaggleGBM3,plot.it = FALSE),type="response")
}

#make predictionns

#test set predictions
TestPredictions<-predict(object = KaggleGBM3,newdata = test
                         ,n.trees = gbm.perf(KaggleGBM3,plot.it = FALSE),
                         type = "response")
#training set predictions
TrainPredictions<-predict(object = KaggleGBM3,newdata = train
                          ,n.trees = gbm.perf(KaggleGBM3,plot.it = FALSE),
                          type = "response")
TestPredictions<-round(TestPredictions)
TrainPredictions<-round(TrainPredictions)

#train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
TrainPredictions<-ifelse(TrainPredictions==1,"Democrat","Republican")
TrainPredictions<-as.factor(TrainPredictions)
table(trainComp$Party,TrainPredictions)
(2087+1673)/nrow(trainComp)

TestPredictions<-ifelse(TestPredictions==1,"Democrat","Republican")
TestPredictions<-as.factor(TestPredictions)
table(TestPredictions)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = TestPredictions)
write.csv(MySubmission, "SubmissionGBMwithOriginalTrainingAndUnderFitting2.csv", row.names=FALSE)
#result for minobsinnode value=30 -  0.65661 and 25-0.6545


############################################################################################
#gbm 5----------------------------------------- 
#GBM on complete training set without user ID

train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
Party<-train$Party
train<-select(train,-Party)
trainNew<-select(train,-USER_ID)
testNew<-select(test,-USER_ID)
#minobsinnode value =20-0.7 on training set,50-0.6866,100-0.69,30-0.69,15-original entry value =10 
#shrinkage value - original - 0.01 current 0.001 - 
ntrees=20000
row_train<-nrow(trainNew)
KaggleGBM4=gbm.fit(x=trainNew
                   ,y=Party
                   ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
                   ,n.trees = ntrees             #select large value for tree and then later on prune it
                   ,shrinkage = 0.001             #set smaller value for shrinkaggive better performance
                   #but it may result in taking a longer time to execute
                   ,interaction.depth = 3        #use cross validation to choose interaction depth
                   ,n.minobsinnode = 30   #small value results in overfitting of data
                   #,cv.folds=3
                   ,nTrain = round(row_train*0.8)      #use to select number of trees in the end
                   #,var.monotone=c(-1)
                   #can help to smoothen the curves and help with overfitting   
                  
                   ,verbose=TRUE)                #print the output in the end

summary(KaggleGBM4)
gbm.perf(KaggleGBM4)

for(i in 1:length(KaggleGBM4$var.names))
{
  plot(KaggleGBM3,i.var=i
       ,ntree=gbm.perf(KaggleGBM4,plot.it = FALSE),type="response")
}

#make predictionns

#test set predictions
TestPredictions<-predict(object = KaggleGBM4,newdata = testNew
                         ,n.trees = gbm.perf(KaggleGBM4,plot.it = FALSE),
                         type = "response")
#training set predictions
TrainPredictions<-predict(object = KaggleGBM4,newdata = trainNew
                          ,n.trees = gbm.perf(KaggleGBM4,plot.it = FALSE),
                          type = "response")
TestPredictions<-round(TestPredictions)
TrainPredictions<-round(TrainPredictions)

#train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
TrainPredictions<-ifelse(TrainPredictions==1,"Democrat","Republican")
TrainPredictions<-as.factor(TrainPredictions)
table(trainComp$Party,TrainPredictions)
(2102+1615)/nrow(trainComp)

TestPredictions<-ifelse(TestPredictions==1,"Democrat","Republican")
TestPredictions<-as.factor(TestPredictions)
table(TestPredictions)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = TestPredictions)
write.csv(MySubmission, "SubmissionGBMwithOriginalTrainingAndUnderFittingAndTREES.csv", row.names=FALSE)
#result for ntrees=20000, 0.66


##################################################################################################
#GBM 6--------------------------
#with selected variables without imputations


train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
Party<-train$Party
train<-select(train,-Party)
trainNew1<-select(trainNew,Q109244,Q115611,Q118232,Q110740,Q120379,Q118237,Q121011,Q123621,Q123464,Q115195,Q98197,Q120472,Q121011,Q118117,Q119334,Q111848,Q121699,Q101163,Q116881,Q116953,Q115899,Q122771,YOB,Gender,Income,HouseholdStatus,EducationLevel)
testNew1<-select(testNew,Q109244,Q115611,Q118232,Q110740,Q120379,Q118237,Q121011,Q123621,Q123464,Q115195,Q98197,Q120472,Q121011,Q118117,Q119334,Q111848,Q121699,Q101163,Q116881,Q116953,Q115899,Q122771,YOB,Gender,Income,HouseholdStatus,EducationLevel)

#minobsinnode value =20-0.7 on training set,50-0.6866,100-0.69,30-0.69,15-original entry value =10 
#shrinkage value - original - 0.01 current 0.001 - 
ntrees=5000
row_train<-nrow(trainNew1)
KaggleGBM5=gbm.fit(x=trainNew1
                   ,y=Party
                   ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
                   ,n.trees = ntrees             #select large value for tree and then later on prune it
                   ,shrinkage = 0.01             #set smaller value for shrinkaggive better performance
                   #but it may result in taking a longer time to execute
                   ,interaction.depth = 3        #use cross validation to choose interaction depth
                   ,n.minobsinnode = 25   #small value results in overfitting of data
                   #,cv.folds=3
                   ,nTrain = round(row_train*0.8)      #use to select number of trees in the end
                   #,var.monotone=c(-1)
                   #can help to smoothen the curves and help with overfitting   
                   
                   ,verbose=TRUE)                #print the output in the end

summary(KaggleGBM5)
gbm.perf(KaggleGBM5)

for(i in 1:length(KaggleGBM5$var.names))
{
  plot(KaggleGBM5,i.var=i
       ,ntree=gbm.perf(KaggleGBM5,plot.it = FALSE),type="response")
}

#make predictionns

#test set predictions
TestPredictions<-predict(object = KaggleGBM5,newdata = testNew1
                         ,n.trees = gbm.perf(KaggleGBM5,plot.it =  FALSE),
                         type = "response")
#training set predictions
TrainPredictions<-predict(object = KaggleGBM5,newdata = trainNew1
                          ,n.trees = gbm.perf(KaggleGBM5,plot.it = FALSE),
                          type = "response")
TestPredictions<-round(TestPredictions)
TrainPredictions<-round(TrainPredictions)

#train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
TrainPredictions<-ifelse(TrainPredictions==1,"Democrat","Republican")
TrainPredictions<-as.factor(TrainPredictions)
table(trainComp$Party,TrainPredictions)
(2013+1594)/nrow(trainComp)

TestPredictions<-ifelse(TestPredictions==1,"Democrat","Republican")
TestPredictions<-as.factor(TestPredictions)
table(TestPredictions)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = TestPredictions)
write.csv(MySubmission, "SubmissionGBMwithOriginalTrainingAndUnderFittingAndTREES.csv", row.names=FALSE)
#result for ntrees=20000, 0.66


############################################################################################
#gbm 7----------------------------------------- 
#GBM on complete training set with demography imputed

train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
Party<-train$Party
train<-select(train,-Party)
trainNew2<-select(train,-USER_ID)
testNew2<-select(test,-USER_ID)
trainNew2[,c(1:5)]<-imputeTrain[,c(2:6)]
testNew2[,c(1:5)]<-imputeTest[,c(2:6)]
trainNew2<-select(train,Gender,Q102089,Q106272,Q109244,Q113181,Q115611,Q115899,Q116881,Q116953,Q118232,Q121699,Q98869)
testNew2<-select(test,Gender,Q102089,Q106272,Q109244,Q113181,Q115611,Q115899,Q116881,Q116953,Q118232,Q121699,Q98869)
#minobsinnode value =20-0.7 on training set,50-0.6866,100-0.69,30-0.69,15-original entry value =10 
#shrinkage value - original - 0.01 current 0.001 - 
ntrees=20000
row_train<-nrow(trainNew2)
KaggleGBM4=gbm.fit(x=trainNew2
                   ,y=Party
                   ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
                   ,n.trees = ntrees             #select large value for tree and then later on prune it
                   ,shrinkage = 0.001             #set smaller value for shrinkaggive better performance
                   #but it may result in taking a longer time to execute
                   ,interaction.depth = 3        #use cross validation to choose interaction depth
                   ,n.minobsinnode = 30   #small value results in overfitting of data
                   #,cv.folds=3
                   ,nTrain = round(row_train*0.8)      #use to select number of trees in the end
                   #,var.monotone=c(-1)
                   #can help to smoothen the curves and help with overfitting   
                   
                   ,verbose=TRUE)                #print the output in the end

summary(KaggleGBM4)
gbm.perf(KaggleGBM4)

for(i in 1:length(KaggleGBM4$var.names))
{
  plot(KaggleGBM4,i.var=i
       ,ntree=gbm.perf(KaggleGBM4,plot.it = FALSE),type="response")
}

#make predictionns

#test set predictions
TestPredictions<-predict(object = KaggleGBM4,newdata = testNew2
                         ,n.trees = gbm.perf(KaggleGBM4,plot.it = FALSE),
                         type = "response")
#training set predictions
TrainPredictions<-predict(object = KaggleGBM4,newdata = trainNew2
                          ,n.trees = gbm.perf(KaggleGBM4,plot.it = FALSE),
                          type = "response")
TestPredictions<-round(TestPredictions)
TrainPredictions<-round(TrainPredictions)

#train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
TrainPredictions<-ifelse(TrainPredictions==1,"Democrat","Republican")
TrainPredictions<-as.factor(TrainPredictions)
table(trainComp$Party,TrainPredictions)
(2102+1615)/nrow(trainComp)

TestPredictions<-ifelse(TestPredictions==1,"Democrat","Republican")
TestPredictions<-as.factor(TestPredictions)
table(TestPredictions)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = TestPredictions)
write.csv(MySubmission, "SubmissionGBMwithOriginalTrainingAndUnderFittingAndTREES.csv", row.names=FALSE)
#result for ntrees=20000, 


############################################################################################
#gbm 8----------------------------------------- 
#GBM on complete training set with everything imputed

train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
Party<-train$Party
train<-select(train,-Party)
imputeTrain<-select(imputeTrain,-USER_ID)
imputeTest<-select(imputeTest,-USER_ID)
#trainNew2<-select(train,Gender,Q102089,Q106272,Q109244,Q113181,Q115611,Q115899,Q116881,Q116953,Q118232,Q121699,Q98869)
#testNew2<-select(test,Gender,Q102089,Q106272,Q109244,Q113181,Q115611,Q115899,Q116881,Q116953,Q118232,Q121699,Q98869)
#minobsinnode value =20-0.7 on training set,50-0.6866,100-0.69,30-0.69,15-original entry value =10 
#shrinkage value - original - 0.01 current 0.001 - 
ntrees=20000
row_train<-nrow(imputeTrain)
KaggleGBM4=gbm.fit(x=imputeTrain
                   ,y=Party
                   ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
                   ,n.trees = ntrees             #select large value for tree and then later on prune it
                   ,shrinkage = 0.001             #set smaller value for shrinkaggive better performance
                   #but it may result in taking a longer time to execute
                   ,interaction.depth = 3        #use cross validation to choose interaction depth
                   ,n.minobsinnode = 30   #small value results in overfitting of data
                   #,cv.folds=3
                   ,nTrain = round(row_train*0.8)      #use to select number of trees in the end
                   #,var.monotone=c(-1)
                   #can help to smoothen the curves and help with overfitting   
                   
                   ,verbose=TRUE)                #print the output in the end

summary(KaggleGBM4)
gbm.perf(KaggleGBM4)

for(i in 1:length(KaggleGBM4$var.names))
{
  plot(KaggleGBM4,i.var=i
       ,ntree=gbm.perf(KaggleGBM4,plot.it = FALSE),type="response")
}

#make predictionns

#test set predictions
TestPredictions<-predict(object = KaggleGBM4,newdata = imputeTest
                         ,n.trees = gbm.perf(KaggleGBM4,plot.it = FALSE),
                         type = "response")
#training set predictions
TrainPredictions<-predict(object = KaggleGBM4,newdata = imputeTrain
                          ,n.trees = gbm.perf(KaggleGBM4,plot.it = FALSE),
                          type = "response")
TestPredictions<-round(TestPredictions)
TrainPredictions<-round(TrainPredictions)

#train$Party<-ifelse(train$Party=="Democrat",TRUE,FALSE)
TrainPredictions<-ifelse(TrainPredictions==1,"Democrat","Republican")
TrainPredictions<-as.factor(TrainPredictions)
table(trainComp$Party,TrainPredictions)
(2102+1615)/nrow(trainComp)

TestPredictions<-ifelse(TestPredictions==1,"Democrat","Republican")
TestPredictions<-as.factor(TestPredictions)
table(TestPredictions)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = TestPredictions)
write.csv(MySubmission, "SubmissionGBMwithOriginalTrainingAndUnderFittingAndTREES.csv", row.names=FALSE)
#result for ntrees=20000, 0.66


