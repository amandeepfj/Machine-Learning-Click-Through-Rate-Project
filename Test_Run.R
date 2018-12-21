# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

rm(list = setdiff(ls(), c("trainingData.full.data", "testData.full.data", "code_files_location", "data_files_location")))

library(data.table)
library(lubridate)
library(dplyr)
if(!exists("trainingData.full.data")){
  trainingData.full.data <- fread(paste0(data_files_location, "ProjectTrainingData.csv"))
}
if(!exists("testData.full.data")){
  testData.full.data <- fread(paste0(data_files_location, "ProjectTestData.csv"))
}

set.seed(4)
trainSampleSize <- 10000
trainingData <- trainingData.full.data[sample(1:nrow(trainingData.full.data), trainSampleSize, replace=FALSE),]
#trainingData.summary <- summary(trainingData)

set.seed(4)
testSampleSize <- 10000
testData <- testData.full.data[sample(1:nrow(testData.full.data), testSampleSize, replace=FALSE),]
#trainingData.summary <- summary(trainingData)

trainingData <- bind_rows(trainingData, testData)

setwd(code_files_location)
source("transform_time_variables.R")
source("Shrink_Categories_and_Factor.R")

trainingData.splits.trainingData <- trainingData[1:trainSampleSize]
trainingData.splits.testData <- trainingData[(trainSampleSize + 1 ): nrow(trainingData), ]


vars <- colnames(trainingData)
cols_not_in_fm_right_side <- c("id", "click")
BigFm <- paste("click","~",paste(setdiff(vars, cols_not_in_fm_right_side),collapse=" + "),sep=" ")
BigFm <- formula(BigFm)


if(!require("rpart")) { install.packages("rpart"); require("rpart") }

trainingData.splits.trainingData[, click := factor(click)]
rpc <- rpart.control(minsplit = 1, minbucket = 1, cp = 0, usesurrogate = 0, xval = 10)
dt <- rpart(BigFm, data = trainingData.splits.trainingData, control=rpc, method = "class")

plotcp(dt)

bestcp <- dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
out1 <- prune(dt, cp = bestcp)
pHatdt <- predict(out1, newdata = trainingData.splits.testData, type = "prob")
pHatdt <- pHatdt[, 2]
AUC.dt <- ROCPlot(Pvec = pHatdt, Cvec = trainingData.splits$validate[, click])$AUC
#logloss.dt <- Logloss(pHatdt, trainingData.splits$validate[, click])

#print(paste("Log Loss of DT =", logloss.dt))
