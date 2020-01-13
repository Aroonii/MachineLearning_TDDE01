library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)
mse <- numeric(10)

# Calculate mean squared errors for different thresholds
  for(i in 1:10) {
    f <- Sin~Var
    nn <- neuralnet(f, data=tr, hidden=c(10), threshold=i/1000, startweights=winit)
    comp <- compute(nn, va$Var)$net.res
    mse[i] <- mean((comp - va$Sin)^2)
  }

# Plot of mean squared error for different values of threshold=i/1000
plot(mse, type="o", main="Mean Squared Error for thresholds=i/1000", xlab="i", ylab="MSE")

plot(nn <- neuralnet(f, data=tr, hidden=c(10), threshold=which.min(mse)/1000, startweights=winit))
  # Plot of the predictions (black dots) and the data (red dots)
  plot(prediction(nn)$rep1, main="Predictions and the data")
  points(trva, col = "red")
  legend(x = "bottomright", c("Predictions", "Data"), lty=c(1,1), col=c("black","red"))
  
  # Plot the final neural network with the chosen threshold
  plot(nn, main)
  