library(randomForest)
library(dplyr)
library(rpart)
library(rpart.plot)
library(party)

#data is same as TitanicRF2

titanicTree<-rpart(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title  + Deck + Child + Mother + family_size + fare_per_person + familyID,data = train,method = "class")
prp(titanicTree)
predictCart<-predict(titanicTree,newdata = test,type = "class")
my_solution <- data.frame(PassengerId = test["PassengerId"], Survived =predictCart)

write.csv(my_solution, file="Cart1.csv",  row.names = FALSE)
#-0.775

titanicTree<-rpart(as.factor(Survived) ~ Pclass + Sex + Age + Embarked +Fare + family_size,data = train,method = "class")
prp(titanicTree)
predictCart<-predict(titanicTree,newdata = test,type = "class")
my_solution <- data.frame(PassengerId = test["PassengerId"], Survived =predictCart)

write.csv(my_solution, file="Cart1.csv",  row.names = FALSE)
#-0.789

