library(stringi)
library(stringr)
#creating a column wiht only the titles
title_list <-c( 'Mrs.', 'Mr.', 'Master.', 'Miss.', 'Major.', 'Rev.',
              'Dr.', 'Ms.', 'Mlle.','Col.', 'Capt.', 'Mme.', 'Countess.',
              'Don.', 'Jonkheer.')
title_list2 <-c( 'Mrs', 'Mr', 'Master', 'Miss', 'Major', 'Rev',
                'Dr', 'Ms', 'Mlle','Col', 'Capt', 'Mme', 'Countess',
                'Don', 'Jonkheer')
#?stri_detect_fixed
stri_detect_fixed(train$Name[1],title_list[2])
title<-c('Mrs')
title[1:nrow(train)]<-c('Mrs')

for(i in 1:NROW(title_list))
{
  for(j in 1:nrow(train))
  {
  if(stri_detect_fixed(train$Name[j],title_list[i])==TRUE)
  {
    title[j]<-title_list2[i]
  }
  }
}
train$title<-title

#replacing titles to->Mr,Mrs,Miss,Master
#'Don', 'Major', 'Capt', 'Jonkheer', 'Rev', 'Col' >-Mr
#stri_detect_fixed(train$title,'Don')
train$title_new[stri_detect_fixed(train$title,'Don')==TRUE]<-'Mr'
train$title_new[stri_detect_fixed(train$title,'Major')==TRUE]<-'Mr'
train$title_new[stri_detect_fixed(train$title,'Capt')==TRUE]<-'Mr'
train$title_new[stri_detect_fixed(train$title,'Jonkheer')==TRUE]<-'Mr'
train$title_new[stri_detect_fixed(train$title,'Rev')==TRUE]<-'Mr'
train$title_new[stri_detect_fixed(train$title,'Col')==TRUE]<-'Mr'
train$title_new[stri_detect_fixed(train$title,'Mr')==TRUE]<-'Mr'

#'Countess', 'Mme'>-Mrs
train$title_new[stri_detect_fixed(train$title,'Countess')==TRUE]<-'Mrs'
train$title_new[stri_detect_fixed(train$title,'Mme')==TRUE]<-'Mrs'
train$title_new[stri_detect_fixed(train$title,'Mrs')==TRUE]<-'Mrs'

#'Mlle', 'Ms'>-Miss
train$title_new[stri_detect_fixed(train$title,'Ms')==TRUE]<-'Miss'
train$title_new[stri_detect_fixed(train$title,'Mlle')==TRUE]<-'Miss'
train$title_new[stri_detect_fixed(train$title,'Miss')==TRUE]<-'Miss'
train$title_new[stri_detect_fixed(train$title,'Master')==TRUE]<-'Master'

#'Dr'-female->Mrs else Mr
train$title_new[stri_detect_fixed(train$title,'Dr')==TRUE & train$Sex=='male']<-'Mr'
train$title_new[stri_detect_fixed(train$title,'Dr')==TRUE & train$Sex=='female']<-'Mrs'

#Family size
train$Family_size<-train$SibSp + train$Parch

#Age*Class
#This is an interaction term, since age and class are both numbers we can just multiply them.
train$AgeClass<-train$Age * train$Pclass

#Fare per Person
train$fairPerPerson<-train$Fare/(train$Family_size +1)

stri_detect_fixed(test$Name[1],title_list[2])
title<-c('Mrs')
title[1:nrow(test)]<-c('Mrs')

for(i in 1:NROW(title_list))
{
  for(j in 1:nrow(test))
  {
    if(stri_detect_fixed(test$Name[j],title_list[i])==TRUE)
    {
      title[j]<-title_list2[i]
    }
  }
}
test$title<-title

#replacing titles to->Mr,Mrs,Miss,Master
#'Don', 'Major', 'Capt', 'Jonkheer', 'Rev', 'Col' >-Mr
#stri_detect_fixed(test$title,'Don')
test$title_new[stri_detect_fixed(test$title,'Don')==TRUE]<-'Mr'
test$title_new[stri_detect_fixed(test$title,'Major')==TRUE]<-'Mr'
test$title_new[stri_detect_fixed(test$title,'Capt')==TRUE]<-'Mr'
test$title_new[stri_detect_fixed(test$title,'Jonkheer')==TRUE]<-'Mr'
test$title_new[stri_detect_fixed(test$title,'Rev')==TRUE]<-'Mr'
test$title_new[stri_detect_fixed(test$title,'Col')==TRUE]<-'Mr'
test$title_new[stri_detect_fixed(test$title,'Mr')==TRUE]<-'Mr'

#'Countess', 'Mme'>-Mrs
test$title_new[stri_detect_fixed(test$title,'Countess')==TRUE]<-'Mrs'
test$title_new[stri_detect_fixed(test$title,'Mme')==TRUE]<-'Mrs'
test$title_new[stri_detect_fixed(test$title,'Mrs')==TRUE]<-'Mrs'

#'Mlle', 'Ms'>-Miss
test$title_new[stri_detect_fixed(test$title,'Ms')==TRUE]<-'Miss'
test$title_new[stri_detect_fixed(test$title,'Mlle')==TRUE]<-'Miss'
test$title_new[stri_detect_fixed(test$title,'Miss')==TRUE]<-'Miss'
test$title_new[stri_detect_fixed(test$title,'Master')==TRUE]<-'Master'

#'Dr'-female->Mrs else Mr
test$title_new[stri_detect_fixed(test$title,'Dr')==TRUE & test$Sex=='male']<-'Mr'
test$title_new[stri_detect_fixed(test$title,'Dr')==TRUE & test$Sex=='female']<-'Mrs'

#Family size
test$Family_size<-test$SibSp + test$Parch

#Age*Class
#This is an interaction term, since age and class are both numbers we can just multiply them.
test$AgeClass<-test$Age * test$Pclass

#Fare per Person
test$fairPerPerson<-test$Fare/(test$Family_size +1)




