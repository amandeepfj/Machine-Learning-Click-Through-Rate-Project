# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

#install multiple packages at once
#install.packages(c("rsample", "ranger", "caret", "h2o"))

library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

h2o.init(max_mem_size = "64g")

# create feature names
y <- "click"
x <- setdiff(names(trainingData.splits$train), y)


# turn training set into h2o object
train.h2o <- as.h2o(trainingData.splits$train)


# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(5, 24, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)


# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

#mtry defines bagging process, if it all variables then its bagging - Bagging
library(randomForest)
bagging <- randomForest(BigFm, data = trainingData.splits$train, 
                        mtry = (length(colnames(trainingData.splits$train)) - 4), ntree = 500)

pHatbagging <- predict(bagging,newdata = trainingData.splits$validate, type = "prob")
pHatbagging <- pHatbagging[, 2]
AUC.bagging <- ROCPlot(Pvec = pHatbagging, Cvec = trainingData.splits$validate[, click])$AUC

