####Step 1####
data = read.csv("machines.csv", header=TRUE, sep=";", dec=",")
set.seed(12345)

####Step 2####
expdist <- function(x, theta){
  return (theta * (exp(-theta*x)))
}

loglikelihood <- function(x, theta){
  p <- expdist(x, theta)
  logp <- log(p)
  return (sum(logp))
}

theta = seq(0, 10, by=0.01)

#Function computing the log-likelihoods for a given vector x and a given vector theta
loglikes <- function(x, theta) {
  loglikeli <- numeric(length(theta))
  for (i in 1:length(theta)) {
    loglikeli[i] = loglikelihood(x,theta[i])
  }
  return (loglikeli)
}

logLikelihood=loglikes(data, theta)

plot(theta, logLikelihood, ylim=c(-250,0), type="l",col="red")
max <- which.max(logLikelihood)
maxlikelihoodval <- theta[max]
print(maxlikelihoodval)

####Step 3####
data6first <- head(data)
vector2=loglikes(data6first, theta)#log-likelihood values
lines(theta, vector2, col="blue")
legend(x = "bottomright", c("all obs", "6 first"), lty = c(1,1), lwd = c(1,1), col=c("red", "blue"))

max <- which.max(vector2)
maxlikelihoodval2 <- theta[max]
print(maxlikelihoodval2)

####Step 4####
prior <- function(theta){
  lambda=10
  return (lambda*exp(-lambda*theta))
}

l <- function(x,theta){
  return (log(prod(expdist(x,theta))*prior(theta)))
}

lres <- numeric(length(theta))
for (i in 1:length(theta)){
  lres[i] <- l(data,theta[i])
}
max2 <- which.max(lres)
maxlikelihoodval3 <- theta[max2]
print(maxlikelihoodval3)

plot(theta,lres,ylab="l(theta)", type="l", col="blue")

####Step 5####
gen <-rexp(50,1.13)

p1 <- hist(data$Length, breaks=14)
p2 <- hist(gen, breaks=14)
plot( p1, col=rgb(0,0,1,1/8), xlim=c(0,6), ylim=c(0,30))
plot( p2, col=rgb(1,0,0,1/8), xlim=c(0,6), add=T)
legend(x="topright",c("old obs", "new obs"),lty=c(1,1),lwd=c(3,3),col=c(rgb(0,0,1,1/8),rgb(1,0,0,1/8)))