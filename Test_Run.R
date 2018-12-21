# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

rm(list = setdiff(ls(), c("trainingData.full.data", "trainingData", "testData.full.data", "code_files_location", "data_files_location")))

if(!exists("testData.full.data")){
  testData.full.data <- fread(paste0(data_files_location, "ProjectTestData.csv"))
}

set.seed(4)
testSampleSize <- 1000
testData <- testData.full.data[sample(1:nrow(testData.full.data), testSampleSize, replace=FALSE),]

testData[, week_day := wday(ymd_h(as.character(hour)))]
testData[, is_week_day := 1]
testData[week_day %in% c(1,7), is_week_day := 0]
testData[, week_day := NULL]
#keeping only hour
testData[, hour := (hour %% 100)]

vars <- colnames(trainingData)
cols_not_in_fm_right_side <- c("id", "click")
BigFm <- paste("click","~",paste(setdiff(vars, cols_not_in_fm_right_side),collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

for(testColumn in setdiff(vars, cols_not_in_fm_right_side)){
  testData[!(get(testColumn) %in% trainingData[, get(testColumn)]), eval(testColumn) := "others"]
  trainingData[, eval(testColumn) := factor(get(testColumn))]
}

if(!require("rpart")) { install.packages("rpart"); require("rpart") }

rpc <- rpart.control(minsplit = 1, minbucket = 1, cp = 0, usesurrogate = 0, xval = 10)
dt <- rpart(BigFm, data = trainingData, control=rpc, method = "class")

plotcp(dt)

bestcp <- dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
out1 <- prune(dt, cp = bestcp)
pHatdt <- predict(out1, newdata = testData, type = "prob")
pHatdt <- pHatdt[, 2]
