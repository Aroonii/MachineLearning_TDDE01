RNGversion('3.5.1')
data = read.csv('influenza.csv')
#View(data)


Y = data$Mortality


plot(data$Mortality)

lambda = seq(10,2918, 100)
#install.packages('gmp')
#install.packages('Math.bigq')

#library(Math.big)
library(gmp)
n = nrow(data)

log.like = function(lambda){
 # x = log(prod(exp(-lambda*lambda^Y)/yfac))
  x = (-n*lambda + log(lambda)*sum(Y) - sum(log(yfac)))
  return(-x)
}

#får rätt svar men tar ju inte factorial ???
log.like2 = function(lambda){
  # x = log(prod(exp(-lambda*lambda^Y)/yfac))
  x = (-n*lambda + log(lambda)*sum(Y) - fact)
  return(-x)
}
fact = 0
for ( cond in Y){
  fact = fact + (log(cond))
}

yfac = (gmp::factorialZ(Y))
v = sapply(lambda, log.like2)
plot(v)


optimize(f = log.like2, c(0,500000),maximum = FALSE)


Mortality=data$Mortality

like=function(y, lambda){
  n=length(y)
  return(lambda*n-log(lambda)*sum(y)+sum(log(factorial(y))))
}

#Find maximum likelihood value of theta
lambda_max = function(y){
  n=length(y)
  return(sum(y)/n)
}



lambda_max=lambda_max(Mortality)
lambda=seq(10,2910,100)
plot(like(Mortality, lambda), lambda, main="The minus loglike function of mortality depending on Lambda",
     xlim=c(10,2910))



## Scale all variables except mortality.. divede the data randomlöy 50/50
#and fit a LASSo regression  with mortality as poission distributed target and all other variables
#as features. 
##Select the optimal parameters in the lasso regression by cross validation and repoty the optimal lasso 
#penalization parameter and also the test MSE. 
##Is the MSE acutalaly the best way of easuring error rates
#in this case?
## REport also the optimal LASSo coefficients and report which variables seem to have biggest impact on the target
## CHeck the value of intercept alpha, compute exp(alpha) and compare it with the optimal lambda in
#step 1. Sre the quanitites similar, should they be?

#install.packages('glmnet')
#install.packages('cvTools')

library(glmnet)
library(cvTools)

#scale features
features = subset(data, select = -c(Mortality))
features = scale(features)
y = data$Mortality
set = cbind(features, y)

# split train test
n=dim(set)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=set[id,]
test=set[-id,] 

#Fit a Lasso model 
covariates = train[,-9]
response = train[,9]
#lasso.fit = glmnet(as.matrix(covariates), response, alpha = 1, family = "poisson" )
#plot(lasso.fit, xvar = "lambda", label = TRUE)



#Choose the best model through crossvalidation
y = test[,9]
x = test[,-9]
cv.model=cv.glmnet(as.matrix(covariates), response, alpha=1,family="poisson")
opt_lambda = cv.model$lambda.min
opt_lambda
plot(cv.model)
coef = coef(cv.model, s="lambda.min")
coef

responseprediction = predict(cv.model, newx = as.matrix(x), type = "response", s = "lambda.min")
MSE = mean((responseprediction-y)^2)
MSE
exp(opt_lambda)
intercept = coef[1]
exp(intercept)

##best lambda = 7.081622, lambda is the value for the penalizing factor
# looking at the coeficcients we see that  Influenza, Temperature decitit, and influenza_lag2 seems to have the biggest impact
#the computed value of intercept exp(intecept) is 1774.665  compared to the lambda from task 1 which was 
#1783. It is not a coincidence. The lasso model is a linear regression but since the poission distrubution is not 
#linear the lasso model puts log on it. This can be seen when plotting the cv.model (log lambda on the x axis)
# applying exp of the intercept cancels this and we get the actual intercept value. MSE is 11767

#3
#Fita a regression tree  with mortality as target aand all variabbles as features and select the optimal sixe of the 
#tree by cross validation.
#Report test MSE and compare with part 2
#Why is it not reasonable to do variable selection by applying Lasso penalization to the tree models

#install.packages('tree')
#library(tree)

tr = data.frame(train)
tree.fit = tree(y ~., data = tr)
plot(tree.fit)
text(tree.fit, pretty = 0)

cv.tree = cv.tree(tree.fit)
plot(cv.tree)
plot(cv.tree$size, cv.tree$dev)
plot(log(cv.tree$size), cv.tree$dev)
best.size = cv.tree$size[which.min(cv.tree$dev)]
best.tree = prune.tree(tree.fit, best = best.size)
plot(best.tree)
text(best.tree, pretty = 0)
te = data.frame(test)
tree.prediction = predict(best.tree, newdata = te, type = "vector")
MSE.tree = mean((tree.prediction-test[,9])^2)
MSE.tree

#MSE for tree is lower MSE = 10806,yhis is better than the value used for the lasso. This could be because 
# tree models are better att regression for complex models. Lasso cannot handle linear regressin as well. 
# it is not reasonable to do LASSO PENALIZATION  becase the tree model is discrete not continous


# Perform PCA analysis on  using all variables in training data except Morality and report how many principal components
# are needed to capture more than 90% of the variation in the data.
# USe the coordinates of the data in the PCA space as features and fit a LASSO regression with Mortality
#as a Poisson distrubutes target by cross validation. Check penalty factors lambda = 0, 0.1, 0.2....50
#Provide a plot that shows the dependence of the cross validation error on log(lambd)
#Does the comlexity of the model increase when lambda increases ?
#How many features are selected by the LASSO rergession?
# Report a probabiistic model corresponding to the optimal LASSO model

tr$y = c()
pca = prcomp(tr)
summary(pca)
#need 5 components to capture 
feature_coordinates = pca$x[,1:5]


lambdas = seq(0,50,0.1)
rep = data.frame(train)
set.seed(1234567890)
lasso.fit.cv = cv.glmnet(as.matrix(feature_coordinates), rep$y, alpha = 1, family = "poisson", lambda = lambdas)
lambda.min = lasso.fit.cv$lambda.min
plot(lasso.fit.cv)
responseprediction.lasso = predict(lasso.fit.cv, newx = as.matrix(x), type = "response", s = lambda.min)
MSE.lasso = mean((responseprediction.lasso-y)^2)
MSE.lasso
coef(lasso.fit.cv, s = "lambda.min")

## The model uses  3 features, PC1 PC2 and PC4. As lambda increases the complexity of the model goes down sinxe it increasingly penalizes the model
# the coefficients  give us the probabilistic model
# => P(exp(-0.029*Comp1 - 0.0124*Comp2 + 0.0046*Comp4))
#Tar exp då lasso modellen loggar (gör en transfomration) av poisson fördelningen



## Assignment 2

#install.packages('neuralnet')
#library(neuralnet)
set.seed(1234567890)
Var = runif(50,0,3)
tr = data.frame(Var, Sin = sin(Var))
Var = runif(50,3,9)
te = data.frame(Var, Sin = sin(Var))

weight = runif(10,-1,1)


nn = neuralnet(Sin~Var, data = tr, hidden = c(3), startweights = weight)
neural.predict = predict(nn, te)
plot(tr, col = "black", ylim = c(-2,1), xlim = c(0,10))
points(te, col = "blue")
points(te$Var, neural.predict, col = "red")
plot(nn)

##Kollar vi på neuralnet ser vi att när var blir stor (positiv ) kommer nod att aktiveras och
 # när den blir  stor negativ kommer den inte aktiveras. Vi kan då räkna output genom att se 
# vilken output de aktiverasd noderna spottar ut. De aktiverade nodernas output + bias ger Y output för
# regression.
#flr classification har även y output en aktiveringsfunktion



##Support vectors

#install.packages('kernlab')
#library(kernlab)

data(spam)
set.seed(1234567890)
index <- sample(1:4601)
tr <- spam[index[1:2500], ]
va <- spam[index[2501:3501], ]
te <- spam[index[3502:4601], ] 



mse = rep(0,50)
Cval = seq(0.2,10.1,0.2)
i = 1
for(c in Cval){
  kernel = ksvm(type~., data = tr, kernel = "rbfdot", C = c, kpar = list(sigma = 0.05))
  prediction = predict(kernel, newdata = va)
  mse[i] = mean(prediction!=va$type)
   i = i +1
}
which.min(mse)
best.c = Cval[which.min(mse)]

trva = spam[index[1:3501],]

## why c value = 0 gives error. Wen c = 0 you are still under the asumption that the data is linearly separable
# When no support vector is found the data is not linearly separable. When c is in th eqation even super small
# you still allow for massclassfications. When c = 0 this part dissapears and you don't alloow them
# based on this it is not super that the support vector can make the data linearly separable

best.kernel = ksvm(type~., data = trva, kernel = "rbfdot", C = best.c, kpar = list(sigma = 0.05))
best.predict = predict(best.kernel, newdata = te)
mean(best.predict!=te$type)
best.kernel[2,]
alpha = alphaindex(best.kernel)
coef =coef(best.kernel)
intecept = b(best.kernel)
