####Step 1####
data = read.csv("spambase.csv", header=TRUE, sep=";", dec=",")
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

####Step 2####
knearest=function(data,k,newdata) {
  
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  X=as.matrix(data[,-p])
  Xn=as.matrix(newdata[-p])
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  
#MISSING: implement steps ii)-iv)
  #ii)
  Xn=Xn/matrix(sqrt(rowSums(Xn^2)), nrow=n2, ncol=p-1)
  
  #iii)
  C=X%*%t(Xn)
  #iv)
  D=matrix(1,nrow(C),ncol(C)) - C

  #MISSING: use the computed distance matrix to find 
  #which observations are the nearest neighbors to case #i  
  for (i in 1:n2 ){
    # kn<-which.min(D[,i])
    # if (k>1){
    #   for (j in 2:k){
    #     kn<-c(kn,which.min(D[-kn,i]))
    #   }
    # }
    kn <- order(D[,i])[1:k]
    
    
#MISSING: derive probability value 'Prob[i]' by using the
    #target values of the nearest neighbors
    targets<-c(data[kn,p])
    Prob[i]=sum(targets)/k
  }
  return(Prob)
}
#debugonce(knearest)
dm <- knearest(train,5,test)#test run knearest

####Step 3####

step34func = function(data,k,newdata) {
  probvals <- knearest(data,k,newdata)
  classify <- round(probvals)
  ct <- table(classify,test[,ncol(test)]) #contingency table
  mcr <- 1 - sum(diag(ct))/sum(ct) #misclassification rate
  return(list(CT=ct,MCR=mcr))
}

step34func(train,5,test)

####Step 4####
step34func(train,1,test)

####Step 5####
library(kknn)

step5func=function(data,k,newdata) {
  data[,ncol(data)] = as.factor(data[,ncol(data)])
  newdata[,ncol(newdata)] = as.factor(newdata[,ncol(newdata)])
  predkknn <- kknn(Spam~.,data,newdata,k=k)
  probvals <- predkknn$prob[,2]
  classify <- round(probvals)
  ct <- table(classify,test[,ncol(test)]) #contingency table
  mcr <- 1 - sum(diag(ct))/sum(ct) #misclassification rate
  return(list(CT=ct,MCR=mcr))
}
#debugonce(step5func)
step5func(train,5,test)

####Step 6####
pi = seq(0.05, 0.95, 0.05)

probsknear <- knearest(train,5,test)

traindata <- train
testdata <- test
traindata[,ncol(traindata)] = as.factor(traindata[,ncol(traindata)])
testdata[,ncol(testdata)] = as.factor(testdata[,ncol(testdata)])
probskknn <- kknn(Spam~.,traindata,testdata,k=5)$prob[,2]

targets <- c(test[,ncol(test)])
#Y=vector with true info if spam/not spam
#Yfit=vector with probability values for the data
#p=classification threshold-vector, pi
ROC=function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    TP=t[2,2]
    Npos=sum(t[,2])
    FP=t[2,1]
    Nneg=sum(t[,1])
    TPR[i]=TP/Npos#formula for TPR
    FPR[i]=FP/Nneg#formula for FPR
  }
  return (list(TPR=TPR,FPR=FPR))
}


ROCknear <- ROC(targets,probsknear,pi)
ROCkknn <- ROC(targets,probskknn,pi)

plot(ROCknear$FPR, ROCknear$TPR, type="l", xlim=c(0,1), ylim=c(0,1), xlab="FPR", ylab="TPR", col="blue")
lines(ROCkknn$FPR, ROCkknn$TPR, col="red")
legend(x = "bottomright", c("knearest", "kknn"), lty = c(1,1), lwd = c(1,1), col=c("blue", "Red"))