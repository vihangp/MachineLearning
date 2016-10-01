
baseball = read.csv("baseball.csv")
str(baseball)
moneyball= subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
plot( moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

#Question 1
#Scores 713 and allows 614 how many games do we expect to win?
713-614
99*.1057+80.8813
90.78
#####

str(moneyball)
RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)

#Question2
#OBP .311 and SLG .405
# -804.63 + 2737.77*.311 + 1584.91*.405
# [1] 688.705

RunsAllReg<-lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsAllReg)
sum(RunsAllReg$coefficients * c(1,.297,.370))

RunsReg = lm(RS ~ OBP + SLG, data = moneyball)
sum(RunsReg$coefficients * c(1,.338,.540))
sum(RunsReg$coefficients * c(1,.391,.450))
sum(RunsReg$coefficients * c(1,.369,.374))
sum(RunsReg$coefficients * c(1,.313,.447))
sum(RunsReg$coefficients * c(1,.361,.500))

Players=as.matrix(rbind(c(1,.338,.540),c(1,.391,.450),c(1,.369,.374),c(1,.313,.447),c(1,.361,.500)))
t(rowSums(Players %*% as.matrix(RunsReg$coefficients)))

###World Series

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012=c(94,88,95,88,93,94,98,97,93,94)
wins2013=c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank,wins2012)
#[1] 0.3477129
cor(teamRank,wins2013)
#[1] -0.6556945

