setwd("/home/fractaluser/R")
pisaTest<-read.csv("pisa2009test.csv")
pisaTrain<-read.csv("pisa2009train.csv")


tapply(pisaTrain$readingScore,pisaTrain$male,mean)


#give values of columns which dont have NA in its rows
a<-as.data.frame(character(1))
j=1
for(i in 1:24){
  if(is.na(table(is.na(pisaTrain[i]))[2])){
    a[j]<-names(pisaTrain[i])
    j=j+1
  }
}
names(a)m
names(pisaTrain)

#values in a will not have NA values i.e completely filled


pisaTrain<-na.omit(pisaTrain)
pisaTest<-na.omit(pisaTest)
nrow(pisaTest)
nrow(pisaTrain)

str(pisaTrain$raceeth)
pisaTrain$raceeth<-relevel(pisaTrain$raceeth,"White")
pisaTest$raceeth<-relevel(pisaTest$raceeth,"White")
str(pisaTrain$raceeth)
str(pisaTest$raceeth)
# 
# #raceasian
# for(i in 1:nrow(pisaTrain))
# {
#     if(pisaTrain$raceeth[i]=="Asian")
#     { 
#         pisaTrain$raceethAsian[i]<- 1
#     }
#   else
#   {
#         pisaTrain$raceethAsian[i]<-0
#   }
# }
# 
# #racehispanic
# for(i in 1:nrow(pisaTrain))
# {
#   if(pisaTrain$raceeth[i]=="Hispanic")
#   { 
#     pisaTrain$raceethHispanic[i]<- 1
#   }
#   else
#   {
#     pisaTrain$raceethHispanic[i]<-0
#   }
# }
# 
# 
# #raceAmerican Indian/Alaska Native
# for(i in 1:nrow(pisaTrain))
# {
#   if(pisaTrain$raceeth[i]=="American Indian/Alaska Native")
#   { 
#     pisaTrain$raceethAmericanIndian[i]<- 1
#   }
#   else
#   {
#     pisaTrain$raceethAmericanIndian[i]<-0
#   }
# }
# 
# #raceBlack
# for(i in 1:nrow(pisaTrain))
# {
#   if(pisaTrain$raceeth[i]=="Black")
#   { 
#     pisaTrain$raceethBlack[i]<- 1
#   }
#   else
#   {
#     pisaTrain$raceethBlack[i]<-0
#   }
# }
# 
# #raceMore than one race
# for(i in 1:nrow(pisaTrain))
# {
#   if(pisaTrain$raceeth[i]=="More than one race")
#   { 
#     pisaTrain$raceethMoreRace[i]<- 1
#   }
#   else
#   {
#     pisaTrain$raceethMoreRace[i]<-0
#   }
# }
# 
# 
# #raceNative Hawaiian/Other Pacific Islander
# for(i in 1:nrow(pisaTrain))
# {
#   if(pisaTrain$raceeth[i]=="Native Hawaiian/Other Pacific Islander")
#   { 
#     pisaTrain$raceethNativeHawaiian[i]<- 1
#   }
#   else
#   {
#     pisaTrain$raceethNativeHawaiian[i]<-0
#   }
# }

lmscore=lm(readingScore ~ ., data = pisaTrain)
summary(lmscore)
lmscore$residuals
SSE=sum(lmscore$residuals^2)
RMSE=sqrt(SSE/nrow(pisaTrain))
RMSE


# pisaTrain$raceethAsian<-NULL
# pisaTrain$raceethHispanic<-NULL
# pisaTrain$raceethAmericanIndian<-NULL
# pisaTrain$raceethBlack<-NULL
# pisaTrain$raceethMoreRace<-NULL
# pisaTrain$raceethNativeHawaiian<-NULL

predTest<-predict(lmscore,newdata = pisaTest)
summary(predTest)
summary(predTest)[1] - summary(predTest)[6]








