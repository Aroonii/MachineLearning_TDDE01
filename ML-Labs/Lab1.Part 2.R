data = machines_1_
View(data)
plot(data)
log.like = function(theta, data){
  length =length(data)
  x =0
  for(i in 1:length){
    logl= log(theta*exp(-theta*data[i,]))
    x = x + logl
  }
  return(x)
}

x = seq(from = 0, to = 10, by = 0.01)
length = length(x)
place=1
vector =  1:length(check)
for (i in x){
  vector[place] = log.like(i, data)
  place=place+1
}
#finns data dexp, 

#Uppg2
checktheta = function(data){
  check = seq(from = 0, to = 50, by = 0.1)
  length = length(check)
  place=1
  vector =  1:length(check)
  for (i in check){
    vector[place] = log.like(i, data)
    place=place+1
  }
  return (vector)
}
x = seq(from = 0, to = 10, by = 0.01)
length = length(x)
place=1
vector =  1:length(check)
for (i in x){
  vector[place] = log.like(i, data)
  place=place+1
}


plott1= plot(x,vector)

max_theta = function(data){
  n=length(data[,1])
  return(n/(sum(data)))
}
max_theta(data)


log.like(max_theta(data),data)


##sapply


trysap=function(theta){
  x=(theta*data)
  return(x)
}

 like = function(theta){
  x=log(prod((theta*exp(-theta*data))))
  return(x)
}

theta = seq(from = 0, to = 10, by = 0.01)
vec = sapply(theta,like)
plot(theta, sapply(theta, like), xlab ="theta", ylab ="y")


curve(dim(data)[1]*log(x) - x*sum(data), from = 0, to = 4)
curve(dim(machines_1_)[1]*log(x) - x*sum(machines_1_), from = 0, to = 10)

newlike = function(theta, data){
  n = length(data)
  x=n*log(theta) - theta*sum(data)
  return(x)
}

theta = seq(from = 0, to = 10, by = 0.01)
newvec = newlike(theta, data$Length)  #varör olika svar om bara data, är ju samma värden ?
plot(theta,sapply(theta, like), xlab = "theta", ylab="y")
lines(theta, newlike(theta, data$Length), col="red")
theta[which.max(newvec)] #max when theta = 1.13
theta[which.max(vec)] #max when theta = 1.13
optimize(f = like, c(0,9), maximum=TRUE)$maximum #1.126201
optimize(f = newlike, c(0,9), maximum=TRUE, data =data$Length)$maximum #-42

# cu
# 2.3 Only data[1:6] 
plot(theta,newlike(theta,data$Length), xlab ="theta", ylab="y", ylim=c(-60,0), xlim=c(0,8))
lines(theta,newlike(theta,data$Length[1:6]), col ="red" )


plot(theta,newlike(theta,data$Length), xlab ="theta", ylab="y", ylim=c(-60,0), xlim=c(0,8))
points(theta,newlike(theta,data$Length[1:6]), col ="red" )

#uppg2.4

  bay.like = function(theta){
  lamda=10
  x=log(prod((theta*exp(-theta*data)))*lamda*exp(-lamda*theta))
  return(x)
}

  bayesian = function(theta, data){
    lamda = 10
    n = length(data)
    x=n*log(theta) - theta*sum(data) + log(lamda) -lamda*theta
    return (x)
  }
  
#med wn bra prior så smalnar sannolikheten av. Får ett bättre resultat
bay = bayesian(theta,data$Length)
plot(theta,bay, xlab ="theta", ylab="y")
lines(theta, newlike(theta, data$Length), col="red")

theta[which.max(bay)] #max when theta = 0.91
optimize(f = bayesian, c(0,9), maximum=TRUE, data=data$Length)$maximum # last maximum to get the max

bay.vec = sapply(theta,bay.like)
plot(theta,bay.vec)
theta[which.max(bay.vec)]

#2.5 Using theta = 1.126201 generate 50 observationsfrom p(x|theta) = theta*exp(-thetax)
#create a histogram for old and new data

set.seed(12345)
x = rexp(50, 1.26201)
par(mfrow=c(2,1))
breaks = seq(0, 10, 0.5)
hist(x, main ="random", xlab ="Length", breaks = breaks)
hist(x, main ="random", xlab ="Length")

hist(data$Length, main="Given", xlab = "Length")
hist(x)  
