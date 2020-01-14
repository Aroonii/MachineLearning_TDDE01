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

#min value is given by leafs = 5 and uses variables A4, A10, A6
CVtree$dev[3]
finalTree = prune.tree(tree, best = 5)
plot(finalTree)
text(finalTree, pretty = 0)

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

#optimal lambda (penality paramter) is 0.0994
#The nr of componetns used are 23
# the mean sqare error is lower for a lower value of lambda this is natural since the penality factor increases the mse 
# The model using the optimal lambda has a smaller binomial deviance but it does not look statistically significatly
# better. It is hower better since it gives a more sparse solution since it uses less components hence # avoiding
#overfitting, reducing complexity. hence the variance goes down

y = test$Class
x = test[,1:15]
x_test = model.matrix( ~ .-1, train[,-16])
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
train=spam[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=spam[id2,]
id3=setdiff(id1,id2)
test=spam[id3,]

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

prediction1 = predict(kernel1, newdata = valid[,-58])
mean(prediction1 != valid$type )
prediction2 = predict(kernel2, newdata = valid[,-58])
mean(prediction2 != valid$type )
prediction3 = predict(kernel3, newdata = valid[,-58])
mean(prediction3 != valid$type )
prediction4 = predict(kernel4, newdata = valid[,-58])
mean(prediction4 != valid$type )
prediction5 = predict(kernel5, newdata = valid[,-58])
mean(prediction5 != valid$type )
prediction6 = predict(kernel6, newdata = valid[,-58])
mean(prediction6 != valid$type )




##NERUALNETWORK
