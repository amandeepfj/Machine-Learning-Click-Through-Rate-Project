# @Authors Alex Su, Aman
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

# for sampling TestData
library(data.table)

setwd('C:/Users/alexs/Documents/Machine Learning/Final Group Project')
TestData.full.data <- fread('ProjectTestData.csv')

set.seed(4)
sampleSize <- 300
TestData <- TestData.full.data[sample(1:nrow(TestData.full.data), sampleSize, replace=FALSE),]
TestData.summary <- summary(TestData)

#keeping only hour
TestData[, week_day := wday(ymd_h(as.character(hour)))]
TestData[, hour := (hour %% 100)]

colnames(TestData)
categorical_variables <- setdiff(colnames(TestData), c("id", "hour", "week_day"))
TestData.distribution <- list()
bPlotBar = TRUE
for(column in categorical_variables){
  TestData.distribution[[column]] <- table(TestData[, get(column)])
  if(bPlotBar  == TRUE){
    barplot(TestData.distribution[[column]], xlab = column, ylab = "Frequency")
  }
  TestData[, eval(column) := as.numeric(factor(TestData[, get(column)]))]
}

# exclude 'id'
TestData <- TestData[,-1]

# seperate Valdata from trainingData
spec <- c(train = .7, validate = .3)

g <- sample(cut(
  seq(nrow(trainingData)), 
  nrow(trainingData)*cumsum(c(0,spec)),
  labels = names(spec)
))

trainingData.splits <-  split(trainingData, g)

# assign TrainData and ValData
TrainData <- trainingData.splits$train
ValData <- trainingData.splits$validate

# Exclude "ID" and "Click"
TrainData <- TrainData[,-1]
ValData <- ValData[,-1]

# TrainData <- TrainData[,-c(1:2)]
# ValData <- ValData[,-c(1:2)]


# Create factor list for each feature
factor_list<-list()
for(i in 1:ncol(TrainData)){
  factor_list[[i]]=unique(TrainData[[i]])
}

factor_list


#Match levels and treat unmatched levels as NA
for(i in 1:ncol(ValData)){
  print(i)
  index<-which(!as.character(ValData[[i]]) %in% as.character(factor_list[[i]]))
  ValData[[i]][index]<-NA
}

summary(ValData)


for(i in 1:ncol(TestData)){
  print(i)
  index<-which(!as.character(TestData[[i]]) %in% as.character(factor_list[[i]]))
  TestData[[i]][index]<-NA
}

# not sure if we need it or not!
# for the extra level in training data 
# Need to set the levels of the validation data to be the same as the levels of
# the training data in order for predict() to work.

for(i in 1:length(TrainData)) {
  if(is.factor(TrainData[[i]])) {
    levels(ValData[[i]]) <- levels(TrainData[[i]])
  }
}
