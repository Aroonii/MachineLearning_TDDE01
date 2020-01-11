RNGversion('3.5.1')
data = read.csv('influenza.csv')
View(data)


Y = data$Mortality


plot(data$Mortality)

lambda = seq(10,2918, 100)
install.packages('gmp')
install.packages('Math.bigq')

library(Math.big)
library(gmp)
n = nrow(data)

log.like = function(lambda){
 # x = log(prod(exp(-lambda*lambda^Y)/yfac))
  x = (-n*lambda + log(lambda)*sum(Y) - sum(log(yfac)))
  return(-x)
}

vec = rep(0,30)

for (i in 1:length(lambda)){
  vec[i] = log.like(lambda[i])
}


vec[1] = log.like(lambda[1])

yfac = (gmp::factorialZ(Y))

v = sapply(lambda, log.like)
plot(v)

#ln(n!)=n*ln(n)-n