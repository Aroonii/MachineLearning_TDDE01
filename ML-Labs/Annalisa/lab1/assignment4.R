####Step 1####
data = read.csv("tecator.csv", header=TRUE, sep=";", dec=",")
set.seed(12345)

plot(data$Moisture, data$Protein)

####Step 2####

####Step 3####
n=dim(data)[1]
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

MSE.train <- numeric(6)
MSE.test <- numeric(6)
for (i in 1:6) {
  fit.train <- lm(Moisture ~ poly(Protein, i, raw=TRUE), train)
  summary <- summary(fit.train)
  MSE.train[i] <-  mean(summary$residuals^2)
  prediction <- predict(fit.train, test)
  MSE.test[i] <- mean((prediction-test$Moisture)^2)
}

i=seq(1,6,1)
plot(i,MSE.train, type="l", col="red", ylim=c(30,40), ylab="MSE")
lines(i, MSE.test, col="blue")
legend(x = "topright", c("training data", "test data"), lty = c(1,1), lwd = c(1,1), col=c("Red", "Blue"))


####Step 4####
subset <- data[,2:101] #only channel1-channel100
data.lm <- lm(data$Fat ~ ., subset)
library(MASS)
stepAIC <- stepAIC(data.lm, trace=FALSE)
print(list(numberOfVars=length(stepAIC$coefficients)))

####Step 5####
library(glmnet)
ridgefit <- glmnet(as.matrix(subset), data$Fat, alpha=0, family="gaussian")
plot(ridgefit, xvar="lambda", label=TRUE)

####Step 6####
lassofit <- glmnet(as.matrix(subset), data$Fat, alpha=1, family="gaussian")
plot(lassofit, xvar="lambda", label=TRUE)

####Step 7####
lassoCV <- cv.glmnet(as.matrix(subset), data$Fat, alpha=1, family="gaussian", lambda=seq(0,7,0.1))
plot(lassoCV)

lambdamin <- lassoCV$lambda.min



