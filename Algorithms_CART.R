#ALGORITHM FILE
#DECISION TREE

rm(list=ls())

dat <- read.csv("R:/577 Capstone/RStudio files/clean.dat.csv", stringsAsFactors = F, header = T)
dim(dat)
str(dat)
dat4 <- dat[,-1]
n = nrow(dat4)

# PARTITIONING
set.seed(1) 
id.train = sample(1:n, nrow(dat4)*.6) 
id.test = setdiff(1:n, id.train) 
dat.train = dat4[id.train,]
dat.test = dat4[id.test,]
nrow(dat.test)
nrow(dat.train)

## 3. Classificaiton Tree ##
library(rpart); library(rpart.plot)
id1 = which(colnames(dat4) == 'SMK_CIG')
fit = rpart(SMK_CIG~., method="class", data=dat.train, control=rpart.control(minsplit=30, cp=0.001))
par(mfrow=c(1,1))
rpart.plot(fit)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree 
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

prob.pred = predict(fit, newdata = dat.test[,-1])[,2]
yhat.test = rep(0, length(id.test))
yhat.test[prob.pred > .5] = 1
