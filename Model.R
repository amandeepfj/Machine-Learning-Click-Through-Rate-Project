library(tree)
vars <- colnames(trainingData)
BigFm <- paste(vars[2],"~",paste(vars[4:24],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

tempFM <- formula("click ~ C18 + C16 + C15 + device_conn_type + device_type + app_category + app_category + site_category + banner_pos + C1")

source("ROCPlot.r")

tc <- tree.control(nrow(trainingData.splits$train), minsize=2, mincut=1, mindev=0)
#dt <- tree(BigFm, data=trainingData.splits$train, control=tc)
dt <- tree(tempFM, data = trainingData.splits$train, control = tc)

NNodes <- summary(dt)$size
NNodes

# Find the best tree size
AUC.tree <- rep(NA,NNodes)
stepTreeSizeby <- 4
for(NNodes in seq(3, NNodes, stepTreeSizeby)) {
  out1 <- prune.tree(dt,best=NNodes)
  pHat <- predict(type = "vector", object = out1, newdata = trainingData.splits$validate)
  pHat <- pHat[, 2]
  AUC.tree[NNodes] <- ROCPlot(Pvec = pHat, Cvec = trainingData.splits$validate[, click], Plot = FALSE)$AUC
}

plot(AUC.tree,type="l")
BestN <- which.max(AUC.tree)
abline(v=BestN, col = 2)
text(BestN+16,(max(AUC.tree,na.rm=TRUE) + min(AUC.tree,na.rm=TRUE))/2,
     paste("n=",BestN),srt=0.2,pos=3, col = 2)
max(AUC.tree,na.rm=TRUE)
AUC.tree[BestN]

dtBest <- prune.tree(dt,best=BestN)
summary(dtBest)
pHatdt <- predict(type = "vector", object = dtBest, newdata = trainingData.splits$validate)
pHatdt <- pHatdt[, 2]
AUC.dt <- ROCPlot(Pvec = pHatdt, Cvec = trainingData.splits$validate[, click])$AUC


#mtry defines bagging process, if it all variables then its bagging - Bagging
library(randomForest)
bagging <- randomForest(tempFM, data = trainingData.splits$train, mtry=10,ntree=500)

pHatbagging <- predict(bagging,newdata = trainingData.splits$validate, type = "prob")
pHatbagging <- pHatbagging[, 2]
AUC.bagging <- ROCPlot(Pvec = pHatbagging, Cvec = trainingData.splits$validate[, click])$AUC


# Random Forest ----
rf <- randomForest(tempFM, data = trainingData.splits$train, ntree=500)

pHatrf <- predict(rf,newdata = trainingData.splits$validate, type = "prob")
pHatrf <- pHatrf[, 2]
AUC.rf <- ROCPlot(Pvec = pHatrf, Cvec = trainingData.splits$validate[, click])$AUC

