library(tree)
vars <- colnames(trainingData)
BigFm <- paste(vars[2],"~",paste(vars[4:24],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

source("ROCPlot.r")

tc <- tree.control(nrow(trainingData.splits$train), minsize=2, mincut=1, mindev=0)
#dt <- tree(BigFm, data=trainingData.splits$train, control=tc)
dt <- tree(click ~ C18 + C16 + C15 + device_conn_type + device_type + app_category + app_category + site_category + banner_pos + C1, 
           data=trainingData.splits$train, control=tc)

NNodes <- summary(dt)$size
NNodes

# Find the best tree size
AUC.tree <- rep(NA,NNodes)
for(NNodes in 3:NNodes) {
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
ROCPlot(Pvec = pHatdt, Cvec = trainingData.splits$validate[, click])
