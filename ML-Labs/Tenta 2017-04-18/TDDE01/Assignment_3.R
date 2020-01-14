library(kernlab)
spam <- read.csv("spam.csv")
set.seed(1234567890)

###Step 1 - Support Vector Machines###
svm.1 <- ksvm(type~., data=spam, kernel="rbfdot", C=1, cross=2, kpar=list(sigma=0.05))
svm.10 <- ksvm(type~., data=spam, kernel="rbfdot", C=10, cross=2, kpar=list(sigma=0.05))
svm.100 <- ksvm(type~., data=spam, kernel="rbfdot", C=100, cross=2, kpar=list(sigma=0.05))

err.1 <- cross(svm.1)
err.10 <- cross(svm.10)
err.100 <- cross(svm.100)

print(list(err1=err.1,err10=err.10, err100=err.100))


###Step 2###
library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
tr <- data.frame(Var, Sin=sin(Var))
tr1 <- tr[1:25,] # Fold 1
tr2 <- tr[26:50,] # Fold 2

layers <- c(10) 
no_weights <- layers[1] + tail(layers, n=1) + sum(layers) + 1 + sum(c(0, layers) * c(
  layers,0))
winit <- runif(no_weights, -1, 1)
threshold <- 0.001
f <- as.formula("Sin ~ Var")
mse <- numeric(2)

NNmse = function(data1, data2) {
  nn <- neuralnet(f, data=data1, hidden=layers, threshold = threshold, startweights = winit)
  pred <- compute(nn, data2$Var)
  mse <- mean((pred$net.result - data2$Sin)^2)
  return (mse)
}

MSEs <- numeric(2)
MSEs[1] <- NNmse(tr1, tr2)
MSEs[2] <- NNmse(tr2, tr1)

errEstimate <- sum(MSEs)/2

print(errEstimate)

