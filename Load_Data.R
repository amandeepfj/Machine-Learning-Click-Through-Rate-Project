# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

setwd("C:/ML/")

rm(list = setdiff(ls(), "trainingData.full.data"))

library(data.table)
if(!exists("trainingData.full.data")){
  trainingData.full.data <- fread("ProjectTrainingData.csv")
}

set.seed(4)
sampleSize <- 1000
trainingData <- trainingData.full.data[sample(1:nrow(trainingData.full.data), sampleSize, replace=FALSE),]
trainingData.summary <- summary(trainingData)

trainingData[, click := factor(trainingData[, click])]
#keeping only hour
trainingData[, week_day := wday(ymd_h(as.character(hour)))]
trainingData[, hour := (hour %% 100)]

colnames(trainingData)
categorical_variables <- setdiff(colnames(trainingData), c("id", "click", "hour", "week_day"))
trainingData.distribution <- list()
bPlotBar = TRUE
for(column in categorical_variables){
  trainingData.distribution[[column]] <- table(trainingData[, get(column)])
  if(bPlotBar  == TRUE){
    barplot(trainingData.distribution[[column]], xlab = column, ylab = "Frequency")
  }
  trainingData[, eval(column) := as.numeric(factor(trainingData[, get(column)]))]
}

spec <- c(train = .7, validate = .3)

g <- sample(cut(
  seq(nrow(trainingData)), 
  nrow(trainingData)*cumsum(c(0,spec)),
  labels = names(spec)
))

trainingData.splits <-  split(trainingData, g)

