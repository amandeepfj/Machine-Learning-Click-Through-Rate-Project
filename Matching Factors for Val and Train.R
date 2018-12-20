# @Authors Alex Su, Aman
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

TrainData <- trainingData.splits$train
ValData <- trainingData.splits$validate

# Exclude "ID" and "Click"
TrainData <- TrainData[,-c("click", "id")]

ValData <- ValData[,-c("click", "id")]


# Create factor list for each feature
factor_list<-list()
for(i in 1:ncol(TrainData)){
  factor_list[[i]]=unique(TrainData[[i]])
}

# Match levels and treat unmatched levels as 'Others'
for(i in 1:ncol(ValData)){
  print(i)
  index<-which(!as.character(ValData[[i]]) %in% as.character(factor_list[[i]]))
  ValData[[i]][index]<-'Other'
}

ValData <- droplevels(ValData)

# ValData[[2]]
# unique(ValData[[2]])
# unique(TrainData[[2]])

TrainData <- lapply(TrainData,as.factor)

ValData <- lapply(ValData,as.factor)

# not sure if we need it or not!
# for the extra level in training data 
# Need to set the levels of the validation data to be the same as the levels of
# the training data in order for predict() to work.

for(i in 1:length(TrainData)) {
  if(is.factor(TrainData[[i]])) {
    levels(ValData[[i]]) <- levels(TrainData[[i]])
  }
}
