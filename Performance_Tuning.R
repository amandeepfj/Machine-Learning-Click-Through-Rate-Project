# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

#install multiple packages at once
#install.packages(c("rsample", "ranger", "caret", "h2o"))

library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

h2o.init(max_mem_size = "5g")


# turn training set into h2o object
train.h2o <- as.h2o(trainingData.splits$train)
