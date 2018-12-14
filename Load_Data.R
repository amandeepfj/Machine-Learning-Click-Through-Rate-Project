setwd("C:/ML/")
library(data.table)

trainingData.full.data <- fread("ProjectTrainingData.csv")

#sampleSize <- ceiling(nrow(trainingData)*0.05)
set.seed(4)
sampleSize <- 10000
trainingData <- trainingData.full.data[sample(1:nrow(trainingData.full.data), sampleSize, replace=FALSE),]

trainingData.summary <- summary(trainingData)

trainingData.unfactored <- data.table(trainingData)

colnames(trainingData)
categorical_variables <- setdiff(colnames(trainingData), c("id"))
trainingData.distribution <- list()
bPlotBar = TRUE
for(column in categorical_variables){
  trainingData.distribution[[column]] <- table(trainingData[, get(column)])
  if(bPlotBar  == TRUE){
    barplot(trainingData.distribution[[column]], xlab = column, ylab = "Frequency")
  }
  #tree supports factors max 32 factor levels
  if(nlevels(factor(trainingData[, get(column)])) <= 32){
    trainingData[, eval(column) := factor(trainingData[, get(column)])]
  } else{
    print(paste("Not factoring", column))
  }
}


spec <- c(train = .7, validate = .3)

g <- sample(cut(
  seq(nrow(trainingData)), 
  nrow(trainingData)*cumsum(c(0,spec)),
  labels = names(spec)
))

trainingData.splits <-  split(trainingData, g)
trainingData.unfactored.splits <- split(trainingData.unfactored, g)

