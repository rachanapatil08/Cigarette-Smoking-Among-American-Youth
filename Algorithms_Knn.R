
#ALGORITHM FILE
#KNN

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

 ## 2. kNN classification ##
require(class)

 knn.bestK = function(train, test, y.train, y.test, k.max = 20) {
   k.grid = seq(1, k.max, 2)
   
   fun.tmp = function(x) {
     y.hat = knn(train, test, y.train, k = x, prob=F)
     return(sum(y.hat != y.test))
   }
   error = unlist(lapply(k.grid, fun.tmp))/length(y.test)
   out = list(k.optimal = k.grid[which.min(error)], error.min = min(error), error)
   return(out)
 }
 
 
 id1 = which(colnames(dat4) == 'SMK_CIG')
 pr <- knn.bestK(dat.train[,-id1], dat.test[,-id1], dat.train$SMK_CIG, dat.test$SMK_CIG)
 ##create confusion matrix
 tab <- table(pr,dat.test$SMK_CIG)
 
 accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
 accuracy(tab)

 