###Step 1###
library(tree)
crx = read.csv("crx.csv")
#View(crx)
n=dim(crx)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
training=crx[id,]
test=crx[-id,]
# id = n*0.8
# training=crx[1:id,]
# test=crx[id+1:n,]

training.modified = training
training.modified[,ncol(training.modified)] = as.factor(training.modified[,ncol(training.modified)])
fit.tree <- tree(Class~.,data=training.modified) 
plot(fit.tree, type=c("uniform"))
text(fit.tree, pretty=0)

training.no2=training.modified[-2,]
fit.tree.no2 <- tree(Class~.,data=training.no2)
plot(fit.tree.no2, type=c("uniform"))
text(fit.tree.no2, pretty = 0)

##Tree structure ändras ingenting???????

###Step 2###
cv.tree = cv.tree(fit.tree)
plot(cv.tree$size, cv.tree$dev, ylab="Deviance", type="b", main="cross-validation plot")
optimal.depth=cv.tree$size[which.min(cv.tree$dev)]
pruned.tree = prune.tree(fit.tree, best=optimal.depth)
plot(pruned.tree)
text(pruned.tree, pretty=0)

#Ans: The tree shuld have 6 leaves and the variables selected are A9, A3, A6, A15 and A11

###Step 3###
library(glmnet)
x_train = model.matrix( ~ .-1, training[,-16])
lasso.CV <- cv.glmnet(x_train, scale(training$Class), alpha=1, family="binomial")
coef(lasso.CV, s="lambda.min") #22st variabler
plot(lasso.CV, main="cross-validation plot")
optimal.penalty <- lasso.CV$lambda.min
lasso.CV$nzero[39] #22st variabler

# lambda > 0 is penalty factor.
# Optimal penalty parameter value is 0.01037
# Number of components selected by LASSO: 22
# For the smallest value of penalty parameter has a deviance around 0.12 higher.
# So the optimal model is better. But it is statistically more significant difference 
# for higher values on penalty parameter

###Step 4###
error = function(targets, predictions) {
  return (sum(targets * log(predictions) + (1-targets) * log(1-predictions)))
}
test.targets <- test$Class

tree.pred <- predict(pruned.tree, test)
tree.err <- error(test.targets, tree.pred[,2]) #-41.81758

x_test = model.matrix( ~ .-1, test[,-16])
lasso.pred <- predict(lasso.CV, x_test, s="lambda.min", type="response")
lasso.err <- error(test.targets, as.vector(lasso.pred)) #-41.83329

# According to this criterion the lasso model is better.
# This criterion might be more reasonable to use than the misclassification rate
# because it takes into consideration how likely it is that a prediction is correctly classified
# and not only if it is correct or not.