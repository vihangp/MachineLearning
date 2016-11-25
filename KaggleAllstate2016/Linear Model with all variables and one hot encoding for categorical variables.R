rm(list=ls())
dir<-"//falmumapp28/Unilever Singapore (15-AIM-1275)/Users/Vihang/Allstate"
setwd(dir)

library(data.table)
library(dplyr)
library(tidyr)
library(gtools)
#####################################################################################
##################One Hot Encoding ##################################################
test<-fread("test.csv")
train<-fread("train.csv")
loss<-train[,c(1,132),with=F]
train$loss<-NULL
train$set<-1
test$set<-0

master<-rbind(train,test)

master3<-master[,c(1),with=F]


for(i in 2:117)
{
    master2<-master[,c(1,i),with=F]
    master2<-master2 %>% 
      gather(key,value,2)
    
    master2$newcol<-paste(master2$key,master2$value,sep = "")
    master2$key<-master2$value<-NULL
    master2$flag<-1
    master2<-master2 %>%
      spread(newcol,flag)
    master2<-na.replace(master2,0)
    
    master3<-merge(master3,master2,all.x = TRUE,by="id")
}

rm(master2,i)

master<-master[,c(1,(118:132)),with=F]
master<-merge(master,master3,all = TRUE,by="id")
rm(test,train,master3)
train<-subset(master,master$set==1)
test<-subset(master,master$set==0)
train$set<-test$set<-NULL

train<-merge(train,loss,all.x = TRUE,by="id")
rm(loss)


testmaster<-test
rm(test)

library(caTools)
set.seed(50)
split<-sample.split(train$loss,SplitRatio = 0.7)
test<-subset(train,split==0)
train<-subset(train,split==1)
rm(split)



lm_all<-lm(loss ~.-id ,data = train)
lm_all_predict<-predict(lm_all,newdata = test)


SSE<-sum((lm_all_predict - test$loss)^2)
SST<-sum((test$loss - mean(test$loss))^2)
r2<-SSE/SST
r2

lm_all_predict<-predict(lm_all,newdata = testmaster)
lm_all_predict<-as.data.table(lm_all_predict)
lm_all_predict<-cbind(testmaster$id,lm_all_predict$lm_all_predict)
lm_all_predict<-as.data.table(lm_all_predict)
setnames(lm_all_predict,c("V1","V2"),c("id","loss"))
lm_all_predict$id<-as.integer(lm_all_predict$id)
lm_all_predict$loss<-as.integer(lm_all_predict$loss)


write.csv(lm_all_predict,"lm_pred2.csv",row.names = FALSE)

SSE<-sum((lm_all_predict - test$loss)^2)
SST<-sum((test$loss - mean(test$loss))^2)





