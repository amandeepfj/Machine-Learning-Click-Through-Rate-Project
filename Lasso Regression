@ Alex Su
@ incomplete

# install.packages('ModelMetrics')
library(ModelMetrics)
# install.packages('MLmetrics')
library(MLmetrics)

Train_with_response <- TrainData
Val_with_response <- ValData

if (!require('glmnet')) {install.packages('glmnet'); require('glmnet')}

# create dummy variable for X
XTrain <-model.matrix(click ~.,Train_with_response)[,-1]
XVal <- model.matrix(click~., Val_with_response)[,-1]
YTrain <- Train_with_response$click
YVal <- Val_with_response$click

grid<-10^seq(3,-3,length = 1000)

glm_lasso<-glmnet(XTrain,YTrain,alpha=1,lambda=grid,thresh=1e-12,family = 'binomial')

YHat<-predict(glm_lasso,newx =XVal,type = 'response')

Logloss(YHat,YVal) # kept showing warnings
