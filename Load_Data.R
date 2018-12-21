# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

rm(list = setdiff(ls(), c("trainingData.full.data", "code_files_location", "data_files_location")))

library(data.table)
library(lubridate)
if(!exists("trainingData.full.data")){
  trainingData.full.data <- fread(paste0(data_files_location, "ProjectTrainingData.csv"))
}

set.seed(4)
sampleSize <- 10000
trainingData <- trainingData.full.data[sample(1:nrow(trainingData.full.data), sampleSize, replace=FALSE),]
trainingData.summary <- summary(trainingData)

setwd(code_files_location)
source("transform_time_variables.R")
source("Shrink_Categories_and_Factor.R")

spec <- c(train = .7, validate = .3)

g <- sample(cut(
  seq(nrow(trainingData)), 
  nrow(trainingData)*cumsum(c(0,spec)),
  labels = names(spec)
))

trainingData.splits <-  split(trainingData, g)

