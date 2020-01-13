data = read.csv("State.csv", header=TRUE, sep=";", dec=",")
set.seed(12345)
####Step 1####
ordered_data<- data[order(data$MET),]
plot(ordered_data$MET, ordered_data$EX, main="State", xlab="MET", ylab="EX")

####Step 2####
library(tree)
nobs=dim(ordered_data)[1]
control <- tree.control(nobs=nobs, minsize=8)
fit <- tree(EX~MET, data=ordered_data, control=control)
plot(fit)
text(fit, pretty=0)

cv.tree <- cv.tree(fit)
plot(cv.tree$size, cv.tree$dev, type="b", col="red")
plot(log(cv.tree$k), cv.tree$dev, type="b", col="red")
pruned_tree <- prune.tree(fit, k=cv.tree$k[which.min(cv.tree$dev)],best=cv.tree$size[which.min(cv.tree$dev)])
plot(pruned_tree)
text(pruned_tree, pretty=0)

Yfit <- predict(pruned_tree, newdata = ordered_data)
plot(ordered_data$MET, ordered_data$EX, main="State", xlab="MET", ylab="EX")
points(ordered_data$MET, Yfit, col="red")

pruned_tree.sum <- summary(pruned_tree)
fit.sum <- summary(fit)
hist_pruned <- hist(pruned_tree.sum$residuals, breaks = 14)
hist_original <- hist(fit.sum$residuals, breaks = 14)
plot( hist_original, col=rgb(0,0,1,1/8), xlab="residuals", main="Histogram of residuals")
plot( hist_pruned, col=rgb(1,0,0,1/8), add=T)
legend(x="topright",c("original", "pruned"),lty=c(1,1),lwd=c(3,3), col=c(rgb(0,0,1,1/8),rgb(1,0,0,1/8)))

####Step 3####
library(boot)

#computing bootstrap samples
f = function(data, ind) {
  D <- data[ind,]
  res <- tree(EX~MET, data=D, control=control)
  pred <- predict(res, newdata=ordered_data)
  return(pred)
}
res <- boot(ordered_data, f, R=1000)

#compute confidence bands
e <- envelope(res)

plot(ordered_data$MET, ordered_data$EX, xlab="MET", ylab="EX")
lines(ordered_data$MET, Yfit, col="red")
lines(ordered_data$MET, e$point[2,], col="blue")
lines(ordered_data$MET, e$point[1,], col="green")

####Step 4####
data = ordered_data
data$EX = as.numeric(data$EX)
data$MET = as.numeric(data$MET)

set.seed(12345)
f1 = function(data1){
  tree = tree(EX~MET, data=data1, control = control) #fit tree model
  #predict values for EX from original data
  pred = predict(tree, newdata=data)
  return(pred)
}

f2 = function(data1){
  tree = tree(EX~MET, data=data1, control = control) #fit tree model
  #predict values for EX from original data
  pred = predict(tree, newdata=data)
  n = length(data$EX)
  predEX = rnorm(n, pred, sd(resid(mle)))
  return(predEX)
}

rng = function(data, mle){
  data1 = data.frame(EX = data$EX, MET = data$MET)
  n = length(data$EX)
  #generate new EX
  data1$EX = rnorm(n, predict(mle, newdata=data1), sd(resid(mle)))
  return(data1)
}

mle = pruned_tree #same as step 2

res1 = boot(data, statistic = f1, R = 1000, mle=mle, ran.gen = rng, sim="parametric")
e1 = envelope(res1)

plot(data$MET, ordered_data$EX, xlab="MET", ylab="EX", ylim=c(100,450))
lines(data$MET, e$point[2,], col="blue")
lines(data$MET, e$point[1,], col="blue")

res2 = boot(data, statistic = f2, R = 10000, mle=mle, ran.gen = rng, sim="parametric")
e2 = envelope(res2)

lines(data$MET, e2$point[2,], col="red")
lines(data$MET, e2$point[1,], col="red")
