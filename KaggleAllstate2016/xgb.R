library(data.table)
#statistics
library(Metrics)
library(scales)
library(Hmisc)
library(forecast)
#data sciense
library(caret)
library(xgboost)
library(e1071)
#strings
library(stringr)

# ----------------------------------- tools & variables ----------------------------

# fair objective 2 for XGBoost

amo.fairobj2 <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  con <- 2
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2
  
  return(list(grad = grad, hess = hess))
  
}

# MAE Metric for XGBoost

amm_mae <- function(preds
                    , dtrain) {
  
  labels <- xgboost::getinfo(dtrain, "label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- mae(elab, epreds)
  
  return(list(metric = "amm_mae", value = err))
  
}

# shift applying to the dependent variable ('loss')

shft <- 200

# names of categorical features for feature engineering

new.cat.raw <- c("cat80","cat87","cat57","cat12","cat79","cat10","cat7","cat89","cat2","cat72",
             "cat81","cat11","cat1","cat13","cat9","cat3","cat16","cat90","cat23","cat36",
             "cat73","cat103","cat40","cat28","cat111","cat6","cat76","cat50","cat5",
             "cat4","cat14","cat38","cat24","cat82","cat25")

new.cat.raw <- merge(data.frame(f1 = new.cat.raw), data.frame(f2 = new.cat.raw))
new.cat.raw <- data.table(new.cat.raw)
new.cat.raw <- new.cat.raw[as.integer(str_extract_all(f1, "[0-9]+")) >
    as.integer(str_extract_all(f2, "[0-9]+"))]

new.cat.raw[, f1 := as.character(f1)]
new.cat.raw[, f2 := as.character(f2)]


#-------------------------------------- load data -----------------------------------------

# load data
tr <- fread("../input/train.csv", showProgress = TRUE)
ts <- fread("../input/test.csv", showProgress = TRUE)

# merge to single data set
ttl <- rbind(tr, ts, fill=TRUE)
remove(tr)
remove(ts)



#-------------------------------------- feature engineering -------------------------------

# new categorical features
# a bit straightforward approach so you can try to improve it

for (f in 1:nrow(new.cat.raw)) {
  
  f1 <- new.cat.raw[f, f1]
  f2 <- new.cat.raw[f, f2]
  
  ttl[, eval(as.name(paste(f1, f2, sep = "_"))) :=
        paste0(ttl[, eval(as.name(f1))], ttl[, eval(as.name(f2))])]
}

# if you have issues with lines 91-98, try to replace it with chunk of codes below (commented lines)
# great thanks to JohnM!

# for (f in 1:nrow(new.cat.raw)) {
#
#  f1 <- new.cat.raw[f, f1]
#  f2 <- new.cat.raw[f, f2]
#  vrb <- paste(f1, f2, sep = "_") # removed as.name
#  
#  ttl[, eval(vrb) := paste0(ttl[[f1]], ttl[[f2]])] # simplified with double brackets
#}

# categorical features to range ones
# was very slow - must be much faster and much R-approach now

utf.A <- utf8ToInt("A")

for (f in colnames(ttl)[colnames(ttl) %like% "^cat"]) {
  
  ttl[, eval(as.name(f)) := mapply(function(x, id) { 
    
    if (id == 1) print(f)
    
    x <- utf8ToInt(x)
    ln <- length(x)
    
    x <- (x - utf.A + 1) * 26 ^ (ln - 1:ln - 1)
    x <- sum(x)
    x
    
  }, eval(as.name(f)), .I)]
  
}

# remove skewness

for (f in colnames(ttl)[colnames(ttl) %like% "^cont"]) {
  
  tst <- e1071::skewness(ttl[, eval(as.name(f))])
  if (tst > .25) {
    if (is.na(ttl[, BoxCoxTrans(eval(as.name(f)))$lambda])) next
    ttl[, eval(as.name(f)) := BoxCox(eval(as.name(f)), BoxCoxTrans(eval(as.name(f)))$lambda)]
  }
}

# scale

for (f in colnames(ttl)[colnames(ttl) %like% "^cont"]) {
  ttl[, eval(as.name(f)) := scale(eval(as.name(f)))]
}

# save

save(ttl, file="data/ttl.pipe.Rda")



#-------------------------------------- convert to matrices -------------------------------

# data table to matrix
# you can do it different ways
# sometimes it is not neccessary though...

ttl.m <- model.matrix(object=~ ., data=model.frame(formula=~ ., data=ttl[, !c("id", "loss"), with=FALSE]
    , na.action="na.pass"))
ttl.m <- as.matrix(ttl.m)

tr.label <- ttl[!is.na(loss), loss]
ts.key <- ttl[is.na(loss), id]
tr.key <- ttl[!is.na(loss), id]

tr.m<-ttl.m[which(ttl[,loss] %in% tr.label),]
ts.m<-ttl.m[which(!(ttl[,loss] %in% tr.label)),]

# remove garbage

remove(ttl, ttl.m)

# sure we can (and should) save labels and keys as well
# however in this script I save only new data matrices

save(tr.m, file="data/tr.m.pipe.Rda")
save(ts.m, file="data/ts.m.pipe.Rda")



#------------------------------------------ prepare model ---------------------------------

# additional variables

n_folds <- 10
cv_sum <- 10
early_stopping <- 50
print.every <- 100

preds <- list()
# rnds <- list(0)

# split data

set.seed(1)
flds <- createFolds(1:nrow(tr.m), k = n_folds)

# parameters

xgb.params <- list(booster = "gbtree"
                   , objective = amo.fairobj2
                   , subsample = 0.7
                   , max_depth = 12
                   , colsample_bytree = 0.7
                   , eta = 0.03
                   , min_child_weight = 100)


# training function

xgb.train.am <- function (ds.x, ds.ev.x, ds.label, ev.label, ds.ts.x, params, it = 0
    , e.stop = 50, print.ev = 100) {
  
  tr.m.xgb <- xgb.DMatrix(ds.x, label=ds.label, missing=NA)
  tr.ev.m.xgb <- xgb.DMatrix(ds.ev.x, label=ev.label, missing=NA)
  ts.m.xgb <- xgb.DMatrix(ds.ts.x, missing=NA)

  print(paste("[", it, "] training xgboost begin ",sep=""," : ",Sys.time()))
  set.seed(1)
  xgb <- xgb.train(params = params
                   , data = tr.m.xgb
                   , nrounds = 10000
                   , verbose = 1
                   , print.every.n = print.ev
                   , feval = amm_mae
                   , watchlist = list(eval = tr.ev.m.xgb, train = tr.m.xgb)
                   , early.stop.round = e.stop
                   , maximize = FALSE)
  
  pred_tst <- predict(xgb, tr.ev.m.xgb)
  
  print(paste("[", it, "] training xgboost complete with score: ", mae(exp(ev.label) - shft
        , exp(pred_tst) - shft), sep="", " : ", Sys.time()))
  
  pred <- predict(xgb, ts.m.xgb)
  
  pred
}



#-------------------------------------------- run and get 1107.23181 ----------------------------------

for (i in 1:n_folds) {
  
  preds[[i]] <- xgb.train.am(ds.x = tr.m[-flds[i][[1]], ]
                          , ds.ev.x = tr.m[flds[i][[1]], ]
                          , ds.label = log(tr.label[-flds[i][[1]]] + shft)
                          , ev.label = log(tr.label[flds[i][[1]]] + shft)
                          , ds.ts.x = ts.m
                          , params = xgb.params
                          , it = i
                          , print.ev = print.every
                          , e.stop = early_stopping)
  
}

# average

preds.t <- as.data.table(preds)
preds.t[, loss := rowMeans(.SD)]

# return to normal condition and write

xgb.sub <- data.table(id = ts.key, loss = exp(preds.t[, loss]) - shft)
write.csv(xgb.sub, paste("../submissions/xgb.upload_",as.character(Sys.Date()),"_1.csv",sep="")
        , row.names=FALSE, quote=FALSE)
write.csv(preds.t, paste("../submissions/RAW.xgb.upload_",as.character(Sys.Date()),"_1.csv",sep="")
        , row.names=FALSE, quote=FALSE)

