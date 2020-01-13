###Step 1###
bank = read.csv("bank.csv", header=TRUE, sep=";", dec=",")

fit.glm <- glm(Visitors~Time, data=bank, family=poisson(link="log"))
summary(fit.glm)
pred <- predict(fit.glm, newdata=bank)
summary(pred)

##Ans: ??

###Step 2###
#mle=lm(EX~MET, data=data.order)
rng=function(data, mle){
  data1=data
  n=length(data$Visitors)
  lambda = mle #mle contains mean of original data (avg events per interval)
  data1$Visitors=rpois(n, lambda)
  return(data1)
}
f2=function(data1){
  #test=lm(EX~MET, data=data1)
  test=glm(Visitors~Time, data=data1, family=poisson(link="log"))
  
  return(predict(test, newdata=bank))
}

res=boot(data=bank, statistic=f2, R=1000, mle=mean(bank$Visitors), ran.gen=rng, sim="parametric")
env2=envelope(res)

plot(data.order$MET, data.order$EX, pch=21, bg="dodgerblue4")
lines(data.order$MET, env2$point[2,], col="chartreuse4")
lines(data.order$MET, env2$point[1,], col="chartreuse4")
lines(data.order$MET, env2$overall[2,], col="firebrick3")
lines(data.order$MET, env2$overall[1,], col="firebrick3")
