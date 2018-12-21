# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

setwd(code_files_location)
source("ROCPlot.r")
source("Log Loss Function.R")

vars <- colnames(trainingData)
cols_not_in_fm_right_side <- c("id", "click")
BigFm <- paste("click","~",paste(setdiff(vars, cols_not_in_fm_right_side),collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

sapply(trainingData,class)

# Decision Trees ----------------
if(!require("rpart")) { install.packages("rpart"); require("rpart") }

rpc <- rpart.control(minsplit = 1, minbucket = 1, cp = 0, usesurrogate = 0, xval = 10)
dt <- rpart(BigFm, data = trainingData.splits$train, control=rpc, method = "class")

plotcp(dt)

bestcp <- dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
out1 <- prune(dt, cp = bestcp)
pHatdt <- predict(out1, newdata = trainingData.splits$validate, type = "prob")
pHatdt <- pHatdt[, 2]
AUC.dt <- ROCPlot(Pvec = pHatdt, Cvec = trainingData.splits$validate[, click])$AUC
Logloss(pHatdt, trainingData.splits$validate[, click])


# mtry defines bagging process, if it all variables then its bagging - Bagging -------------
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



