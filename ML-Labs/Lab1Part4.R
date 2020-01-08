my_data = tecator
plot(my_data$Protein, my_data$Moisture, xlab ="protein", ylab = "moisture")
#appears to be a linear model

#Mi~N(Sum(Wix),delta)
n=dim(my_data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=my_data[id,]
test=my_data[-id,] 

#skulle även kunna skriva ut de polynom som skapas ex x + I(x^2) + Ix^3 men desssa
#hade varit korrelerade termer.  poly skapar orthogonala polynomials

#modeller för mosisture expected value
model1 = lm(Moisture~poly(Protein, degree = 1), data = train)
model2 = lm(Moisture~poly(Protein, degree = 2), data = train)
model3 = lm(Moisture~poly(Protein, degree = 3), data = train)
model4 = lm(Moisture~poly(Protein, degree = 4), data = train)
model5 = lm(Moisture~poly(Protein, degree = 5), data = train)
model6 = lm(Moisture~poly(Protein, degree = 6), data = train)

get.MSE = function(train, test){
  MSE = matrix(nrow=6, ncol = 2)
  for(i in 1:6){
    model = lm(Moisture~poly(Protein, degree = i), data = train)
    MSE[i,1] = mean(model$residuals ^ 2)
    
    pred = predict(model,newdata = test)
    MSE[i,2] = mean((pred - test$Moisture) ^ 2)
  }
  return (MSE)
}
MSE = get.MSE(train,test)

#Residualerna blir större för de ja kör på testdata. Borde väl vara tvärt om

plot(MSE[,1], ylim = c(20,50), type = "b", ylab = "MSE", xlab = "degree")
#plot(MSE[,2], ylim = c(0,50), type = "b", ylab = "MSE", xlab = "degree")
points(MSE[,2], col = "red", type = 'b')


legend("bottomleft", col = c("black", "red"), lwd = c(2,2), legend = c("Training data", "testdata"))



getMSE = function(train, val){

MSE = matrix(nrow = 6, ncol = 2)
for (i in 1:6){
  model = lm(Moisture ~ poly(Protein, degree = i, raw = TRUE), data = train)
  predValues = predict(model, newdata = val, type = "response")
  MSE[i,1] = mean(model$residuals^2)
  MSE[i,2] = mean((predValues - val$Moisture)^2)
}
return(MSE)
}



library("MASS")
newdata = my_data[c(2:102)]
fat.model = lm(Fat ~., data = newdata)

step = stepAIC(fat.model, direction = "both")
length(coef(step)) #64 st => 95.548 # 1 coeff is intercept => 63 variabler väljs

step = stepAIC(fat.model, direction = "backward")
length(coef(step)) #same as above ? 

step = stepAIC(fat.model, direction = "forward")
length(coef(step)) #101st  => 100 variabler , AIC = 151


#4.5
library("glmnet")

lamda = 1
sum(fat.model$residuals ^2) + lamda*
install.packages("glmnet", dependencies = TRUE)
library("glmnet")

response.var = scale(my_data[102])
covariates = scale(my_data[c(2:101)])
lambdas <- 10^seq(3, -2, by = -.1)


fit = glmnet(covariates, response.var, alpha = 0, lambda = lambdas)
summary(fit)

#uses cross validation to find optimal lambda
cv_fit = cv.glmnet(covariates, response.var, lambda = lambdas, alpha = 0)

#lowest point in optimal
plot(cv_fit)
opt.lambda = cv_fit$lambda.min

#fit = glmnet(covariates, response.var, family = "gaussian", aplpha = 0)
#plot(fit, xvar ="lambda")
#fit$lambda.min

#extract all fitted models
#summary(fit) <==> cv_fit$glmnet.fit 
fitted.values = cv_fit$glmnet.fit

library("glmnet")
covariates=scale(my_data[,2:101])
response=scale(my_data[,102])
#alpha=0 => ridge, ridge is good when most variables are useful
modelRidge = glmnet(x = as.matrix(covariates), y = response, family = "gaussian", alpha = 0)
plot(modelRidge, xvar="lambda")

#y_predict = predict(fitted.values, s = opt.lambda, newx = response.var )



#4.6
#alpha = 1 => lasso, lasso is good as reducing the imprtance of insignificant varables
modelLasso = glmnet(x = as.matrix(covariates), y = response, family = "gaussian", alpha = 1)
plot(modelLasso, xvar="lambda")



#4.7
modelCV=cv.glmnet(x = as.matrix(covariates), y = response, family = "gaussian", alpha = 1)
minLambda = modelCV$lambda.min
plot(modelCV)





