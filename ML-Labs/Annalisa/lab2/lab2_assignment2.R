####Step 1####
data = read.csv("creditscoring.csv", header=TRUE, sep=";", dec=",")
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
training=data[id,]
temp=data[-id,]
n2=dim(temp)[1]
id2=sample(1:n2, floor(n2*0.5))
validation=temp[id2,]
test=temp[-id2,]

####Step 2####
library(tree)
fit_dev <- tree(good_bad~.,data=training, split="deviance")
fit_gin <- tree(good_bad~.,data=training, split="gini")

#retreive misclassification rates for training data
train_mcr_dev <- summary(fit_dev)$misclass[1]/ summary(fit_dev)$misclass[2]
train_mcr_gin <- summary(fit_gin)$misclass[1]/summary(fit_gin)$misclass[2]

print(list(DEV=train_mcr_dev, GINI=train_mcr_gin))

#make predictions based on the fitted tree model 
pred_dev_test <- predict(fit_dev, test, type="class")
pred_gin_test <- predict(fit_gin, test, type="class")
targets_test <- test$good_bad

#calculate misclassification rates for test data
tab_dev <- table(pred_dev_test,targets_test)
mcr_dev <- 1-sum(diag(tab_dev))/sum(tab_dev)

tab_gin <- table(pred_gin_test, targets_test)
mcr_gin <- 1-sum(diag(tab_gin))/sum(tab_dev)

print (list(DEV=mcr_dev,GINI=mcr_gin))

####Step 3####
trainScore=rep(0,9) #deviances for training data
validScore=rep(0,9) #deviances for validation data
nodes=seq(2,9,1) #number of nodes

for (i in 2:9){
  prunedTree <- prune.tree(fit_dev, best=i)
  pred <- predict(prunedTree, newdata=validation, type="tree")
  trainScore[i] = deviance(prunedTree)
  validScore[i] = deviance(pred)
}

#dividing deviances for training data by 2 since the dataset is of doubble size
plot(2:9, trainScore[2:9]/2, type="b", col="red", ylim=c(250,300))
points(2:9, validScore[2:9], type="b", col="blue")

minScore_valid <- which.min(validScore[2:9])
optimalLeaves <- nodes[minScore_valid]

#use the retreived optimal number of leaves to prune the final tree
finalTree <- prune.tree(fit_dev, best=optimalLeaves)
print(finalTree)
plot(finalTree)
text(finalTree, pretty=0)

#calculate misclassification rate for the testdata with the final tree
predict_test <- predict(finalTree, newdata = test, type="class")
tab2 <- table(predict_test, targets_test)
mcr2 <- 1-sum(diag(tab2))/sum(tab2)
print(list(MCR=mcr2))

####Step 4####
library(MASS)
library(e1071)
fit_nb <- naiveBayes(good_bad~., data = training)

Yfit_train <- predict(fit_nb, newdata = training)
tab_train <- table(Yfit_train, training$good_bad)
mcr_train <- 1-sum(diag(tab_train))/sum(tab_train)
print (list(CM=tab_train, MCR=mcr_train))

Yfit_test <- predict(fit_nb, newdata = test)
tab_test <- table(Yfit_test, test$good_bad)
mcr_test <- 1-sum(diag(tab_test))/sum(tab_test)
print (list(CM=tab_test, MCR=mcr_test))

####Step 5####
naive_pred_train <- predict(fit_nb, newdata = training, type="raw")
naive_pred_test <- predict(fit_nb, newdata = test, type="raw")

# set prediction bad to true if the probability that it is good is 
# 10 times bigger than the prediction that it is bad
naive_pred_train <- ifelse((naive_pred_train[,2]/naive_pred_train[,1]) >10, "good", "bad")
naive_pred_test <- ifelse((naive_pred_test[,2]/naive_pred_test[,1]) >10, "good", "bad")

#confusion matrices
cm_train <- table(naive_pred_train, True=training$good_bad)
cm_test <- table(naive_pred_test, True=test$good_bad)
print(list(TRAIN=cm_train, TEST=cm_test))

mcr_train_lossm <- 1-sum(diag(cm_train))/sum(cm_train)
mcr_test_lossm <- 1-sum(diag(cm_test))/sum(cm_test)
print(list(TRAIN=mcr_train_lossm, TEST=mcr_test_lossm))