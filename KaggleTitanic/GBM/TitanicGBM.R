# Model using only Pclass,Sex,Age,SibSp,Parch,Fare,Embarked


library(gbm)
library(dplyr)
library(caTools)
train<-read.csv("train.csv")
test<-read.csv("test.csv")

#basic data manipulation
survived<-train$Survived
train<-select(train,-Survived)
end_trn<-nrow(train)
all<-rbind(train,test)
end<-nrow(all)
#survived<-survived[1:919]
set.seed(1000)
#spl <- sample.split(titanic$Survived, SplitRatio = 0.7)
#surviveTrain<- titanic[1:919,]
#surviveTest<- titanic[920:1313,]


library(datasets)
??datasets

all<-select(all,Pclass,Sex,Age,SibSp,Parch,Fare,Embarked)
head(all)
ntrees=5000

Model=gbm.fit(x=all[1:end_trn,]
              ,y=survived
            ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
            ,n.trees = ntrees             #select large value for tree and then later on prune it
            ,shrinkage = 0.001             #set smaller value for shrinkaggive better performance
                                          #but it may result in taking a longer time to execute
            ,interaction.depth = 3        #use cross validation to choose interaction depth
            ,n.minobsinnode = 50         #small value results in overfitting of data
            ,nTrain = round(end_trn*0.8)      #use to select number of trees in the end
            #,var.monotone=c()
            #can help to smoothen the curves and help with overfitting            
            ,verbose=TRUE)                #print the output in the end

summary(Model)
gbm.perf(Model)

for(i in 1:length(Model$var.names))
{
  plot(Model,i.var=i
       ,ntree=gbm.perf(Model,plot.it = FALSE),type="response")
}

#make predictionns
#test set predictions
TestPredictions<-predict(object = Model,newdata = all[(end_trn+1):end,]
                         ,n.trees = gbm.perf(Model,plot.it = FALSE),
                         type = "response")
#training set predictions
TrainPredictions<-predict(object = Model,newdata = all[1:end_trn,]
                         ,n.trees = gbm.perf(Model,plot.it = FALSE),
                         type = "response")

TestPredictions<-round(TestPredictions)
TrainPredictions<-round(TrainPredictions)
head(TrainPredictions,n=20)
head(survived,n=20)

accuracyOnTrain<-1-sum(abs(survived-TrainPredictions))/length(TrainPredictions)

submission<-data.frame(PassengerId=test$PassengerId,survived=TestPredictions)
write.csv(submission,file = "titanicSimpleGBM.csv",row.names = FALSE)









