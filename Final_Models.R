# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"


source("Machine-Learning-Click-Through-Rate-Project/Load_Data.R")
source("Machine-Learning-Click-Through-Rate-Project/ROCPlot.r")

vars <- colnames(trainingData)
BigFm <- paste(vars[2],"~",paste(vars[4:24],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

# Decision Trees ----------------
if(!require("rpart")) { install.packages("rpart"); require("rpart") }
if(!require("ROCR")) { install.packages("ROCR"); require("ROCR") }

rpc <- rpart.control(minsplit = 1, minbucket = 1, cp = 0, usesurrogate = 0, xval = 10)
dt <- rpart(BigFm, data = trainingData.splits$train, control=rpc)

plotcp(dt)

cps <- dt$cp
AUC.tree <- rep(NA, length(cps[, 1]))
auc.cnt <- 1
for(tempCp in cps[, 1]){
  out.temp <- prune(dt, cp = tempCp)
  pHat <- predict(out.temp, type = "prob", newdata = trainingData.splits$validate)
  pred <- prediction(pHat[, 2], trainingData.splits$validate[, click])
  auc.perf <- performance(pred, measure = "auc")
  AUC.tree[auc.cnt] <- unlist(auc.perf@y.values)
  auc.cnt <- auc.cnt + 1
}

plot(AUC.tree,type="l")
BestN <- which.max(AUC.tree)
abline(v=BestN, col = 2)
text(BestN+16,(max(AUC.tree,na.rm=TRUE) + min(AUC.tree, na.rm = TRUE))/2,
     paste("n=",BestN),srt=0.2,pos=3, col = 2)
max(AUC.tree,na.rm=TRUE)
AUC.tree[BestN]
bestCP <- cps[BestN, 1]

out1 <- prune(dt, cp = bestCP)
pHat <- predict(out1, type = "prob", newdata = trainingData.splits$validate)
pred <- prediction(pHat[, 2], trainingData.splits$validate[, click])
roc.perf <- performance(pred, "tpr", "fpr")
auc.perf <- performance(pred, measure = "auc")

plot(roc.perf,lwd=2,col="blue",
     main=paste("AUC :", auc.perf@y.values))
abline(0, 1, lty = 2)

AUC.dt <- auc.perf@y.values

#mtry defines bagging process, if it all variables then its bagging - Bagging
library(randomForest)
bagging <- randomForest(BigFm, data = trainingData.splits$train, 
                        mtry = (length(colnames(trainingData.splits$train)) - 4), ntree = 500)

pHatbagging <- predict(bagging,newdata = trainingData.splits$validate, type = "prob")
pHatbagging <- pHatbagging[, 2]
AUC.bagging <- ROCPlot(Pvec = pHatbagging, Cvec = trainingData.splits$validate[, click])$AUC

# Random Forest ----
rf <- randomForest(BigFm, data = trainingData.splits$train, ntree = 500, mtry = 5)

pHatrf <- predict(rf,newdata = trainingData.splits$validate, type = "prob")
pHatrf <- pHatrf[, 2]
AUC.rf <- ROCPlot(Pvec = pHatrf, Cvec = trainingData.splits$validate[, click])$AUC

paste(AUC.dt, AUC.bagging, AUC.rf)
