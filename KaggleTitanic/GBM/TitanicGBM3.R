#Data is used same as TitanicRF2
library(gbm)
library(dplyr)
library(caTools)
library(datasets)
library(caret)


train_1<-select(train,Pclass , Sex , Age , SibSp , Parch , Fare ,Embarked ,title 
              ,Deck ,Child ,Mother ,family_size ,fare_per_person ,familyID,Cluster)

ntrees=5000

Model1=gbm.fit(x=train_1
              ,y=survived
              ,distribution = "bernoulli"   #bernoulli for classification and gaussian for regression/adaboost
              ,n.trees = ntrees             #select large value for tree and then later on prune it
              ,shrinkage = 0.001             #set smaller value for shrinkaggive better performance
              #but it may result in taking a longer time to execute
              ,interaction.depth = 3        #use cross validation to choose interaction depth
              ,n.minobsinnode = 20         #small value results in overfitting of data
              ,nTrain = round(end_trn*0.8)      #use to select number of trees in the end
              #,var.monotone=c()
              #can help to smoothen the curves and help with overfitting            
              ,verbose=TRUE)                #print the output in the end

tgbm<-train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title  + Child + Mother + family_size + fare_per_person + familyID,data = train,method="gbm",trControl=trainControl(method = "cv"))
my_prediction <- predict(tgbm, test, type = "prob")

my_prediction<-round(my_prediction)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test["PassengerId"], Survived = my_prediction[2])

write.csv(my_solution, file="gbm2.csv",  row.names = FALSE)
#result-0.76

