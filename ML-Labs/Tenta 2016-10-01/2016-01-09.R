RNGversion('3.5.1')
data = read.csv('crx.csv')

#Assignment 1
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
train=data[id,]
test=data[-id,] 

#install.packages('tree')
#library(tree)
par(mfcol = c(1,1))
tree = tree(Class~., data = train)
plot(tree)
text(tree, pretty = 0)

train.subset = train[-2,]
tree.subset = tree(Class~., data = train.subset)
plot(tree.subset)
text(tree.subset, pretty = 0)

#sturcture does not change at all

#prune tree to optimal nr of leafs by using cross validation. Provide a cross validation plot and comment how 
# many leaves the optimal tree should have. Which variables were selected by the tree

set.seed(12345)
CVtree = cv.tree(tree)
plot(CVtree)
plot(CVtree$size, CVtree$dev)
#plot(log(CVtree$k), CVtree$dev)
# k is the penalizing paramter which determines how manyleafes should be cut
which.min(CVtree$size)
which.min(CVtree$dev)
opt.tree.length = CVtree$size[which.min(CVtree$dev)]

#min value is given by leafs = 5 and uses variables A4, A10, A6
CVtree$dev[3]
finalTree = prune.tree(tree, best = opt.tree.length)
plot(finalTree, type = "uniform")
text(finalTree, pretty = 0)
summary(finalTree)

#install.packages('glmnet')
#library(glmnet)

x_train = model.matrix( ~ .-1, train[,-16])
#x_train = model.matrix( ~ ., train[,-16], xlev = -1)
set.seed(12345)
class = as.factor(train$Class)
lasso.model = cv.glmnet(x_train, class, alpha = 1, family = "binomial")
lasso.model$lambda.min
plot(lasso.model)
coef(lasso.model, s = "lambda.min")
lasso.model$nzero[39]

#optimal lambda (penality paramter) is 0.0994
#The nr of componetns used are 22
# the mean sqare error is lower for a lower value of lambda this is natural since the penality factor increases the mse 
# The model using the optimal lambda has a smaller binomial deviance but it does not look statistically significatly
# better. It is hower better since it gives a more sparse solution since it uses less components hence # avoiding
#overfitting, reducing complexity. hence the variance goes down

y = test$Class
x = test[,1:15]
x_test = model.matrix( ~ .-1, test[,-16])
ynew.tree = predict(finalTree, newdata = test, type = "vector")
ynew.lasso  = predict(lasso.model, newx = x_test, type = "response")
sum(y*log(ynew.tree)+(1-y)*log(1-ynew.tree))
sum(y*log(ynew.lasso)+(1-y)*log(1-ynew.lasso))

#according to this creterium to tree model is better since it is less negative => closer predictionn
# This model could be suitable since it takes into account the probability of cclass being
#classsified. not only if it gets right clasification ( could have been a low probabilty that it did)



##ASSIGNEMTN 2
#install.packages('mboost')
#library(mboost)
#bf <- read.csv2("bodyfatregression.csv")
#set.seed(1234567890)
#m <- blackboost(Bodyfat_percent ~ Waist_cm+Weight_kg, data=bf)
#mstop(m)
#cvf <- cv(model.weights(m), type="kfold")
#cvm <- cvrisk(m, folds=cvf, grid=1:100)
#plot(cvm)
#mstop(cvm)
#blackboost implemetns gradient boosting by utilixing regression trees as base learner
# mstop(m) gives us that the model stops the boosting after 30 iterations
#we then apply cross validation on the model in order to find the best weights
# we then create cvrisk using the optimal weights found in cv to shoe the estimated risk of applying various iteration


#install.packages('kernlab')
#library(kernlab)
#install.packages('cvTools')
#library(cvTools)
data(spam)


n=dim(spam)[1]
set.seed(1234567890)
id=sample(1:n, floor(n*0.5))
tr1=spam[id,]
tr2 = spam[-id,]

set.seed(1234567890)
kernel1 = ksvm(type~., data = train, kernel = "rbfdot", C = 1, kpar = list(sigma = 0.01), cross = 2)
set.seed(1234567890)
kernel2 = ksvm(type~., data = train, kernel = "rbfdot", C = 1, kpar = list(sigma = 0.05), cross = 2)
set.seed(1234567890)
kernel3 = ksvm(type~., data = train, kernel = "rbfdot", C = 5, kpar = list(sigma = 0.01), cross = 2)
set.seed(1234567890)
kernel4 = ksvm(type~., data = train, kernel = "rbfdot", C = 5, kpar = list(sigma = 0.05), cross = 2)
set.seed(1234567890)
kernel5 = ksvm(type~., data = train, kernel = "vanilladot", C = 1, cross = 2)
set.seed(1234567890)
kernel6 = ksvm(type~., data = train, kernel = "vanilladot", C = 5, cross = 2)

pred.kernel1 = function(train, test){
  set.seed(1234567890)
  kernel = ksvm(type~., data = train, kernel = "rbfdot", C = 1, kpar = list(sigma = 0.01), cross = 2)
  prediction = predict(kernel, newdata = test[,-58])
  x = mean((prediction != test$type)^2)
  return(x)
}

pred.kernel2 = function(train, test){
  set.seed(1234567890)
  kernel = ksvm(type~., data = train, kernel = "rbfdot", C = 1, kpar = list(sigma = 0.05), cross = 2)
  prediction = predict(kernel, newdata = test[,-58])
  x = mean((prediction != test$type)^2)
  return(x)
}

pred.kernel3 = function(train, test){
  set.seed(1234567890)
  kernel = ksvm(type~., data = train, kernel = "rbfdot", C = 5, kpar = list(sigma = 0.01), cross = 2)
  prediction = predict(kernel, newdata = test[,-58])
  x = mean((prediction != test$type)^2)
  return(x)
}

pred.kernel4 = function(train, test){
  set.seed(1234567890)
  kernel = ksvm(type~., data = train, kernel = "rbfdot", C = 5, kpar = list(sigma = 0.05), cross = 2)
  prediction = predict(kernel, newdata = test[,-58])
  x = mean((prediction != test$type)^2)
  return(x)
}

pred.kernel5 = function(train, test){
  set.seed(1234567890)
  kernel = ksvm(type~., data = train, kernel = "vanilladot", C = 1, cross = 2)
  prediction = predict(kernel, newdata = test[,-58])
  x = mean((prediction != test$type)^2)
  return(x)
}

pred.kernel6 = function(train, test){
  set.seed(1234567890)
  kernel = ksvm(type~., data = train, kernel = "vanilladot", C = 5, cross = 2)
  prediction = predict(kernel, newdata = test[,-58])
  x = mean((prediction != test$type)^2)
  return(x)
}


mse1 = (pred.kernel1(tr1, tr2) + pred.kernel1(tr2, tr1))/2
mse2 = (pred.kernel2(tr1, tr2) + pred.kernel2(tr2, tr1))/2
mse3 = (pred.kernel3(tr1, tr2) + pred.kernel3(tr2, tr1))/2
mse4 = (pred.kernel4(tr1, tr2) + pred.kernel4(tr2, tr1))/2
mse5 = (pred.kernel5(tr1, tr2) + pred.kernel5(tr2, tr1))/2
mse6 = (pred.kernel6(tr1, tr2) + pred.kernel6(tr2, tr1))/2
cat("kernel1", mse1, "kernel2", mse2, "kernel3", mse3, "kernel4", mse4, "kernle5", mse5, "kernel6", mse6)


##NERUALNETWORK
