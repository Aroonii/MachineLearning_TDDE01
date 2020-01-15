RNGversion('3.5.1')
stat = read.csv2('state.csv')

data = stat[order(stat$MET),]
plot(data$MET, data$EX)

#Data looks to be normally distributed hence a maximum likelihood could be useful
#library(tree)
#library(cvTools)

tree.control(48, mincut = 8)
control = tree.control(48, mincut = 8)
tree.fit = tree(EX ~ MET, data = data, control = control)
plot(tree.fit)
text(tree.fit, prety = 0)
set.seed(1234567890)
cv.tree = cv.tree(tree.fit)
plot(cv.tree$size, cv.tree$dev)
min.leafs = cv.tree$size[which.min(cv.tree$dev)]
min.k = cv.tree$k[which.min(cv.tree$dev)]
min.leafs
final.tree = prune.tree(tree.fit, best = min.leafs)
summary(final.tree)
plot(final.tree)
text(final.tree, pretty = 0)

tree.predict = predict(final.tree, newdata = data, type = "vector")


plot(data$MET, data$EX)
points(data$MET, tree.predict, col = "red")

summary.final = summary(final.tree)
summary.fit = summary(tree.fit)

final.residuals = summary.final$residuals
tree.residuals = summary.fit$residuals

hist(final.residuals, breaks = 8)
hist(tree.residuals, breaks = 8)

##part 3

#library(boot)
# computingbootstrapsamples
set.seed(12345)
f=function(data, ind){
  data1=data[ind,]# extractbootstrapsample
  res= tree(EX ~ MET, data = data1, control = control)
  pruned.tree = prune.tree(res, best = 3)
  #predictvaluesfor all Area valuesfrom the original data såledess data inte data1
  tree.predict=predict(pruned.tree,newdata=data)
  return(tree.predict)
}

res=boot(data, f, R=1000) #make bootstrap
e = envelope(res)

plot(data$MET, data$EX, main = "non-parametric confidence bands")
lines(data$MET, tree.predict, col = "red")
points(data$MET, e$point[1,], col = "blue", type = "l")
points(data$MET, e$point[2,], col = "blue",type = "l")

#conficence bands are bumpy. This is beause that no distribution is asumeed for the data and the model
# tries to accustom as best as it can to the given data. The width is rather high which incicates that the model 
#is not that relaible. That we can almostdraw a straight line though the band firther indicates
#that the model is not reliable since it means that each EX value wpuld yield the same MET value again

##part 4

rng=function(data, mle  ) {
  data1=data.frame(EX= data$EX, MET=data$MET)
  n=length(data$EX)
  #generatenew Price
  data1$EX=rnorm(n,predict(mle, newdata=data1),sd(resid(mle)))
  return(data1)
}

#predictionband
f2=function(data1){
  #data1=data[ind,]# extractbootstrapsample
  res= tree(EX ~ MET, data = data1, control = control)
  pruned.tree = prune.tree(res, best = 3)
  #predictvaluesfor all Area valuesfrom the original data
  tree.predict=predict(pruned.tree,newdata=data)
  #Fixa predictionband genom att sampla n nya punkter som har väntevärde enligt prediction
  #dvs punkter som med 95% sannolikhet kommer att hamna där
  n = length(data$EX)
  predictionband = rnorm(n, tree.predict, sd(resid(mle)))
  return(predictionband)
}

#confidenceband
f1=function(data1){
  #data1=data[ind,]# extractbootstrapsample
  res= tree(EX ~ MET, data = data1, control = control)
  pruned.tree = prune.tree(res, best = 3)
  #predictvaluesfor all Area valuesfrom the original data
  tree.predict=predict(pruned.tree,newdata=data)
  return(tree.predict)
}

mle = final.tree
set.seed(12345)
res.pred=boot(data, statistic=f2, R=1000, mle =mle, ran.gen=rng , sim="parametric")
set.seed(12345)
res.conf=boot(data, statistic=f1, R=1000, mle =mle, ran.gen=rng , sim="parametric")

e.pred = envelope(res.pred)
e.conf = envelope(res.conf)

plot(data$MET, data$EX, ylim =c(100,450))
#lines(data$MET, tree.predict, col = "red")
points(data$MET, e.pred$point[1,], col = "blue", type = "l")
points(data$MET, e.pred$point[2,], col = "blue",type = "l")
points(data$MET, e.conf$point[1,], col = "green", type = "l")
points(data$MET, e.conf$point[2,], col = "green",type = "l")
