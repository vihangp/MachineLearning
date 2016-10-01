library(randomForest)
library(dplyr)
library(rpart)
library(rpart.plot)
library(party)
train<-read.csv("train.csv")
test<-read.csv("test.csv")
shuffle<-train[sample(nrow(train)),]

all<-bind_rows(shuffle,test)
all$Embarked[c(62,830)]<-"C"
all$Embarked<-as.factor(all$Embarked)
all$Fare[1044]<-median(all[all$Pclass=='3' & all$Embarked=='S',]$Fare,na.rm = TRUE)
all$title<-gsub(".*\\ (.*)\\..*","\\1",all$Name)
#if(FALSE){
  all$title[all$title == 'L'] <- 'Other'
  all$title[all$title == 'Capt'] <- 'Other'
  all$title[all$title == 'Countess'] <- 'Other'
  all$title[all$title == 'Don'] <- 'Other'
  all$title[all$title == 'Dona'] <- 'Other'
  all$title[all$title == 'Mme'] <- 'Other'
  all$title[all$title == 'Major'] <- 'Other'
  all$title[all$title == 'Jonkheer'] <- 'Other'
  
  rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                  'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
  
  # Also reassign mlle, ms, and mme accordingly
  all$title[all$title == 'Mlle']        <- 'Miss' 
  all$title[all$title == 'Ms']          <- 'Miss'
  all$title[all$title == 'Mme']         <- 'Mrs' 
  all$title[all$title %in% rare_title]  <- 'Rare title'
#}
all$title<-as.factor(all$title)
all$family_size<-all$Parch + all$SibSp + 1
all$fare_per_person<-all$Fare/all$family_size
all$Surname<-sapply(all$Name,FUN=function(x){strsplit(x,split = '[,.]')[[1]][1]})
all$familyID <- paste(as.character(all$family_size), all$surname, sep="")
all$familyID[all$family_size < 2] <- 'Small'
famIDs <- data.frame(table(all$familyID))
famIDs <- famIDs[famIDs$Freq < 2,]
all$familyID[all$familyID %in% famIDs$Var1] <- 'Small'
length(unique(all$familyID))
all$familyID <- factor(all$familyID)


predicted_age <- cforest(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + title + family_size + fare_per_person + familyID,
                         data = all[!is.na(all$Age),],
                         controls=cforest_unbiased(ntree=200, mtry=3))

all$Age[is.na(all$Age)] <- predict(predicted_age, all[is.na(all$Age),], OOB=TRUE, type = "response")
all$Child[all$Age < 18] <- 'Child'
all$Child[all$Age >= 18] <- 'Adult'
all$Child <- factor(all$Child)
all$Mother <- 'Not Mother'
all$Mother[all$Sex == 'female' & all$Parch > 0 & all$Age > 18 & all$title != 'Miss'] <- "Mother"
all$Mother <- factor(all$Mother)
all$Deck<-substring(all$Cabin, 1, 1)
all$Deck <- factor(all$Deck)


predicted_deck <- cforest(Deck ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + Child + Mother + family_size + fare_per_person + familyID,
                          data = all[all$Deck != '',],
                          controls=cforest_unbiased(ntree=200, mtry=3))

all$Deck[all$Deck == ''] <- predict(predicted_deck, all[all$Deck == '',], OOB=TRUE, type = "response")
str(all)
train <- all[1:891,]
test <- all[892:1309,]

my_forest <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title 
                     + Deck + Child + Mother + family_size + fare_per_person + familyID, data = train,
                     controls=cforest_unbiased(ntree=500, mtry=3))

my_prediction <- predict(my_forest, test, OOB=TRUE, type = "response")
# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test["PassengerId"], Survived = my_prediction)

write.csv(my_solution, file="cforest1.csv",  row.names = FALSE)
#result-0.80/0.79


my_forest <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + family_size , data = train,
                     controls=cforest_unbiased(ntree=500, mtry=3))

my_prediction <- predict(my_forest, test, OOB=TRUE, type = "response")
# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test["PassengerId"], Survived = my_prediction)

write.csv(my_solution, file="cforest2.csv",  row.names = FALSE)
#result-0.76



#if(FALSE){
  clusterdata <- all[, c('Pclass', 'Age', 'SibSp', 'Parch', 'Fare')]
  clusterdata$Sex <- as.numeric(all$Sex)
  #wss <- (nrow(clusterdata)-1)*sum(apply(clusterdata,2,var))
  #  for (i in 2:15) wss[i] <- sum(kmeans(clusterdata, centers=i)$withinss)
  #plot(1:15, wss, type="b", xlab="Number of Clusters",
  #     ylab="Within groups sum of squares")
  error <- vector()
  error[1] <- my_forest$err.rate[2000, c('OOB')]
  for(i in 2:15){
    cluster <- kmeans(clusterdata, i)
    all$Cluster <- as.factor(cluster$cluster)
    
    train <- all[1:891,]
    test <- all[892:1309,]
    
    my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + Deck + Child + Mother + Cluster + family_size, data=train, importance = TRUE, ntree=2000)
    error[i] <- my_forest$err.rate[2000, c('OOB')]
  }
  plot(error, type="b")
  cluster <- kmeans(clusterdata, 8)
  all$Cluster <- as.factor(cluster$cluster)
  
  train <- all[1:891,]
  test <- all[892:1309,]
  
  my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + Deck + Child + Mother + Cluster + family_size, data=train, importance = TRUE, ntree=4000)
#}

  my_prediction <- predict(my_forest, test, OOB=TRUE, type = "response")
  # Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
  my_solution <- data.frame(PassengerId = test["PassengerId"], Survived = my_prediction)
  
  write.csv(my_solution, file="Rforest1.csv",  row.names = FALSE)
  #result-0.80-ntree-2000
  #0.77-500
  #0.76-1500


  my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title 
                            + Deck + Child + Mother + family_size + fare_per_person + familyID + Cluster, data=train, importance = TRUE, ntree=4000)
  #}
  
  my_prediction <- predict(my_forest, test, OOB=TRUE, type = "response")
  # Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
  my_solution <- data.frame(PassengerId = test["PassengerId"], Survived = my_prediction)
  
  write.csv(my_solution, file="Rforest1.csv",  row.names = FALSE)
  