# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

rm(list = setdiff(ls(), c("trainingData.full.data", "testData.full.data", "code_files_location", "data_files_location")))

library(data.table)
library(lubridate)
if(!exists("trainingData.full.data")){
  trainingData.full.data <- fread(paste0(data_files_location, "ProjectTrainingData.csv"))
}
if(!exists("testData.full.data")){
  testData.full.data <- fread(paste0(data_files_location, "ProjectTestData.csv"))
}

set.seed(4)
sampleSize <- nrow(trainingData.full.data)
#sampleSize <- 5000
if(sampleSize != nrow(trainingData.full.data)){
  trainingData <- trainingData.full.data[sample(1:nrow(trainingData.full.data), sampleSize, replace=FALSE),]
  testData <- testData.full.data[sample(1:nrow(testData.full.data), sampleSize, replace=FALSE),]
} else{
  trainingData <- trainingData.full.data
  testData <- testData.full.data
}

library(dplyr)
trainingData <- data.table(bind_rows(trainingData, testData))

setwd(code_files_location)
source("transform_time_variables.R")
source("Shrink_Categories_and_Factor.R")


trainingData.final <- trainingData[1: sampleSize]
testData.final <- trainingData[(sampleSize + 1) : nrow(trainingData)]


vars <- colnames(trainingData)
cols_not_in_fm_right_side <- c(not__to_factor)
independent_variables <- setdiff(vars, cols_not_in_fm_right_side)
BigFm <- paste("click","~",paste(independent_variables, collapse=" + "),sep=" ")
BigFm <- formula(BigFm)
print(BigFm)

# Create factor list for each feature
factor_list<-list()
for(i in 1:length(independent_variables)){
  factor_list[[i]]=unique(trainingData.final[[i]])
}

#match the levels
for(i in 1:length(independent_variables)){
  index<-which(!as.character(testData.final[[i]]) %in% as.character(factor_list[[i]]))
  testData.final[[i]][index]<-NA
}

# Decision Trees ----------------
if(!require("rpart")) { install.packages("rpart"); require("rpart") }

rpc <- rpart.control(minsplit = 1, minbucket = 1, cp = 0, usesurrogate = 0, xval = 5)
dt <- rpart(BigFm, data = trainingData.final, control=rpc, method = "class")

plotcp(dt)

bestcp <- dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
out1 <- prune(dt, cp = bestcp)
pHatdt_preds <- predict(out1, newdata = testData.final, type = "prob")
pHatdt_class <- predict(out1, newdata = testData.final, type = "class")

table(pHatdt_class)

fwrite(pHatdt_preds, "pHatdt_preds.csv")
fwrite(pHatdt_class, "pHatdt_class.csv")