#ALGORITHM FILE
# LOGISTIC REGRESSION

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

# Logistic Regression
lr.train <- dat.train
lr.test <- dat.test

logistic <- glm(SMK_CIG~. , data=lr.train, family = "binomial")
summary(logistic)

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null-ll.proposed)/ll.null  # Pseudo R-squared = 0.78
1-pchisq(2*(ll.proposed-ll.null), df=(length(logistic$coefficients)-1))  #p-value = 0
# we can thus interpret that the pseudo R-sq value is not just due to luck

predicted.data<- data.frame(probability.of.smk= logistic$fitted.values, smk = lr.train$SMK_CIG)
#sort dataframe from low probab to high probab
predicted.data <- predicted.data[order(predicted.data$probability.of.smk, decreasing = FALSE),]
#add a new column that has the rank of each sample
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
usePackage('cowplot')
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.smk))+
  geom_point(aes(color=smk), alpha=1, shape=4, stroke=2)+
  xlab("Index")+
  ylab("Predicted Probability of Smoking a Cigarette")
ggsave("CigSmoking.png")






