# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

setwd(code_files_location)
source("ROCPlot.r")
source("Log Loss Function.R")

vars <- colnames(trainingData)
cols_not_in_fm_right_side <- c(not__to_factor)
independent_variables <- setdiff(vars, cols_not_in_fm_right_side)
BigFm <- paste("click","~",paste(independent_variables, collapse=" + "),sep=" ")
BigFm <- formula(BigFm)
print(BigFm)
sapply(trainingData,class)

#sapply(trainingData, table)
gc()
# Decision Trees ----------------
if(!require("rpart")) { install.packages("rpart"); require("rpart") }

rpc <- rpart.control(minsplit = 1, minbucket = 1, cp = 0, usesurrogate = 0, xval = 5)
system.time(dt <- rpart(BigFm, data = trainingData.splits$train, method = "class"))

plotcp(dt)

bestcp <- dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
out1 <- prune(dt, cp = bestcp)
pHatdt <- predict(out1, newdata = trainingData.splits$validate, type = "prob")
pHatdt <- pHatdt[, 2]
AUC.dt <- ROCPlot(Pvec = pHatdt, Cvec = trainingData.splits$validate[, click])$AUC
logloss.dt <- Logloss(pHatdt, trainingData.splits$validate[, click])

print(paste("Log Loss of DT =", logloss.dt))

gc()
# mtry defines bagging process, if it all variables then its bagging - Bagging -------------
library(randomForest)
system.time(bagging <- randomForest(BigFm, data = trainingData.splits$train, 
                        mtry = (length(colnames(trainingData.splits$train)) - 4), ntree = 500))

pHatbagging <- predict(bagging,newdata = trainingData.splits$validate, type = "prob")
pHatbagging <- pHatbagging[, 2]
AUC.bagging <- ROCPlot(Pvec = pHatbagging, Cvec = trainingData.splits$validate[, click])$AUC
Logloss(pHatbagging, trainingData.splits$validate[, click])

print(paste("Log Loss of DT =", logloss.dt))

# Random Forest ----
rf <- randomForest(BigFm, data = trainingData.splits$train, ntree = 500, mtry = 5)

pHatrf <- predict(rf,newdata = trainingData.splits$validate, type = "prob")
pHatrf <- pHatrf[, 2]
AUC.rf <- ROCPlot(Pvec = pHatrf, Cvec = trainingData.splits$validate[, click])$AUC
Logloss(pHatrf, trainingData.splits$validate[, click])

gc()
# GLM ----------------

#THIS IS INCOMPLETE
lr <- glm(click ~ hour + C1 + banner_pos, 
          data = trainingData.splits$train, 
          family = binomial(link="logit"))

pHatlr <- predict.glm(object = lr, newdata = trainingData.splits$validate)

AUC.lr <- ROCPlot(Pvec = pHatlr, Cvec = trainingData.splits$validate[, click])$AUC
Logloss(pHatlr, trainingData.splits$validate[, click])

paste(AUC.dt, AUC.bagging, AUC.rf)



