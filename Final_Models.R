# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

setwd("C:/ML/")

source("Machine-Learning-Click-Through-Rate-Project/Load_Data.R")
source("Machine-Learning-Click-Through-Rate-Project/ROCPlot.r")
source("Machine-Learning-Click-Through-Rate-Project/Log Loss Function.R")

vars <- colnames(trainingData)
BigFm <- paste(vars[2],"~",paste(vars[4:24],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

# Decision Trees ----------------
if(!require("rpart")) { install.packages("rpart"); require("rpart") }
if(!require("ROCR")) { install.packages("ROCR"); require("ROCR") }

rpc <- rpart.control(minsplit = 1, minbucket = 1, cp = 0, usesurrogate = 0, xval = 10)
dt <- rpart(BigFm, data = trainingData.splits$train, control=rpc)

plotcp(dt)

bestcp <- dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
out1 <- prune(dt, cp = bestcp)
pHatdt <- predict(out1, newdata = trainingData.splits$validate, type = "prob")
pHatdt <- pHatdt[, 2]
AUC.dt <- ROCPlot(Pvec = pHatdt, Cvec = trainingData.splits$validate[, click])$AUC
Logloss(pHatdt, trainingData.splits$validate[, click])

#mtry defines bagging process, if it all variables then its bagging - Bagging
library(randomForest)
bagging <- randomForest(BigFm, data = trainingData.splits$train, 
                        mtry = (length(colnames(trainingData.splits$train)) - 4), ntree = 500)

pHatbagging <- predict(bagging,newdata = trainingData.splits$validate, type = "prob")
pHatbagging <- pHatbagging[, 2]
AUC.bagging <- ROCPlot(Pvec = pHatbagging, Cvec = trainingData.splits$validate[, click])$AUC
Logloss(pHatbagging, trainingData.splits$validate[, click])

# Random Forest ----
rf <- randomForest(BigFm, data = trainingData.splits$train, ntree = 500, mtry = 5)

pHatrf <- predict(rf,newdata = trainingData.splits$validate, type = "prob")
pHatrf <- pHatrf[, 2]
AUC.rf <- ROCPlot(Pvec = pHatrf, Cvec = trainingData.splits$validate[, click])$AUC
Logloss(pHatrf, trainingData.splits$validate[, click])

paste(AUC.dt, AUC.bagging, AUC.rf)
