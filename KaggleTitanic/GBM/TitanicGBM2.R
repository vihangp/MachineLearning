#GBMModel using Pclass,Sex,Age,Sibsp,Parch,Fare,Embarked,Title_new,Family_size,AgeClass,fairPerPerson


library(gbm)
library(dplyr)
library(caTools)


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
#GBMModel using Pclass,Sex,Age,Sibsp,Parch,Fare,Embarked,Title_new,Family_size,AgeClass,fairPerPerson
#all<-select(all,Pclass,Sex,Age,SibSp,Parch,Fare,Embarked,title_new,Family_size,AgeClass,fairPerPerson)
#AgeClass is used in gbm2-3-4
all<-select(all,Pclass,Sex,Age,SibSp,Parch,Fare,Embarked,title_new,Family_size,fairPerPerson)
all$title_new<-as.factor(all$title_new)
head(all)
ntrees=35000

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
write.csv(submission,file = "titanicSimpleGBM5.csv",row.names = FALSE)

#GBM2 Values- shrinkage-0.01 and ntrees=5000 nobisnode value=50 and testacc-0.876 submission-0.76
#GBM3 Values- shrinkage-0.001 and ntrees=35000 nobisnode value=25 and testacc-0.896 submission-0.75
#GBM4 Values- shrinkage-0.001 and ntrees=35000 nobisnode value=50 and testacc-0.87 submission- 0.75
#GBM5 Values- shrinkage-0.001 and ntrees=35000 nobisnode value=50 and testacc-0.88 submission- 0.76
#AgeClass removed in gbm5


