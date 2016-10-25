#datasets used after titanic gbm2
library(randomForest)
train$survived<-survived
train$survived<-as.factor(train$survived)
str(train$survived)
train$title_new<-as.factor(train$title_new)
#----------------Model1--------------------
#a model without age and age*class
RandomForestTitanic<-randomForest(survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+title_new+Family_size+fairPerPerson,data = train,nodesize=25,ntree=200)

test$title_new<-as.factor(test$title_new)
levels(test$Embarked)<-levels(train$Embarked)
levels(test$Pclass)<-levels(train$Pclass)
RFPredTitanic<-predict(RandomForestTitanic,newdata = test)

test$RFPredTitanic<-RFPredTitanic

submission<-data.frame(PassengerId=test$PassengerId,survived=test$RFPredTitanic)
write.csv(submission,file = "titanicRF1.csv",row.names = FALSE)
#0.78469
#----------------Model2--------------------
#converting to and from as a factor
#imputing age with NA in them
train$Pclass<-as.factor(train$Pclass)
train$survived<-NULL
test$RFPredTitanic<-NULL
all<-rbind(train,test)
library(mice)
allnew<-all[c("Age","title","title_new","Sex","Pclass","Family_size")]
summary(allnew)
str(allnew)
allnew<-complete(mice(allnew))
train$Age<-allnew$Age[1:891]
test$Age<-allnew$Age[892:1309]

train$survived<-survived
train$survived<-as.factor(train$survived)
str(train$survived)
RandomForestTitanic<-randomForest(survived~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked+title_new+Family_size+fairPerPerson,data = train,nodesize=25,ntree=200)
test$RFPredTitanic<-NULL
RFPredTitanic<-predict(RandomForestTitanic,newdata = test)

test$RFPredTitanic<-RFPredTitanic

submission<-data.frame(PassengerId=test$PassengerId,survived=test$RFPredTitanic)
write.csv(submission,file = "titanicRF2.csv",row.names = FALSE)
#0.78947















