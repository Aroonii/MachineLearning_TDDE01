RNGversion('3.5.1')
data = read.csv('australian-crabs.csv')

#install.packages('ggplot2')
#library(ggplot2)
ggplot(data, aes(x = CW, y = BD, col = species)) +
  geom_point()

#install.packages('MASS')
#install.packages('e1071')
#library(MASS)
#library(e1071)

set.seed(12345)
bayes.fit = naiveBayes(species ~ CW + BD, data = data)
prediction.bayes = predict(bayes.fit, newdata = data, type = "class")
table(acutal = data$species, predicted = prediction.bayes)
mean(prediction.bayes!= data$species)

# MSE is rel high 0.395. On the matrix we se that there are many that are predicted as blue that were actually oraange.
# Naive bayes uses the assumption that the åredictors are independent. Based on the plot we can see that this is not the
# case and tehrefor the bayesclassifier is not good

# make a lgositic model

logistic.model = glm(species ~ CW + BD, data = data, family = "binomial")
prediction.logistic = predict(logistic.model, type = "response")
prediction.logistic = ifelse(prediction.logistic >= 0.5, "orange", "blue")

features = data.frame(CW = data$CW, RW = data$BD)

intercept = (logistic.model$coefficients[1])/(-logistic.model$coefficients[3])
slope = (logistic.model$coefficients[2])/(-logistic.model$coefficients[3])
slopefunc = function(x){intercept+slope*x}

plot = ggplot(data = data, aes(x = CW, y = BD, col = prediction.logistic)) + 
  geom_point()
plot
plot = plot + stat_function(fun = slopefunc, colour = "green")
plot

species.frame = data.frame(species = prediction.logistic)
plot(data$CW, data$BD, col = prediction.logistic)
abline(intercept, slope, col = "black")

# the quality is good although is gives signs 
table(actual = data$species, prediction = prediction.logistic)


##PCA
data$CW = scale(data$CW)
data$BD = scale(data$BD)
features = subset(data, select = c(BD, CW))
y = data$species

pca = prcomp(features)
lambda = pca$sdev^2
lambda
sprintf("%2.3f", lambda/sum(lambda)*100)
summary(pca)
screeplot(pca)
bayes.features = pca$x
pca$rotation
#since the features are strongly correlated it is enough to use 1 of the components
#component coordinates in terms of original coordinates is gives by $rotation
# PC1 = 0-707BW + 0.707CW
#PC2 = -0.707BW - 0.707CW

#library(MASS)
#library(e1071)

newdata = cbind(bayes.features, y)
species = ifelse(newdata[,3] > 1, "Orange", "Blue")
mydata = cbind(species, bayes.features)
mydata = data.frame(mydata)
bayes.pca = naiveBayes(species ~., data = mydata)
bayesPCA.predict = predict(bayes.pca, newdata = mydata, type = "class")
table(actual = mydata$species, prediction = bayesPCA.predict)
mean(mydata$species!= bayesPCA.predict)
# The classification is now 100% correct. This is due to that the PC components are mutally independent of each other.
#Since this is  what the naivebayes classifier is assuming it results in a perfect fit

#ASSIGNMENT 2
bank = read.csv2('bank.csv')

glm = glm(Visitors ~ Time, data= bank, family = poisson(link="log"))
#plot(glm)
coefficients(glm)
glm$coefficients
# Time = => intercept + Time*0.4017
time = data.frame(Time = bank$Time)
control=exp(0.1742155+0.4017126*time)
print(control)

## since the response is possion distributed log is applied in order to fit a linear model. This has to be taken into
# account when creating the model and creating it's predictions

#install.packages('boot')
#library(boot)

#reorder (although already ordered) according to time
data2 = bank[order(bank$Time),]
#data1 = bank

rng=function(data, mle) {
  data1=data.frame(Visitors=data$Visitors, Time=data$Time)
  n=length(data$Visitors)
  #generate new data 
  #data1$Visitors=rnorm(n,predict(mle, newdata=data1),sd(mle$residuals))
  data1$Visitors = rpois(n, predict(mle, newdata = data1, type = "response"))
  return(data1)
}
f1=function(data1){
  res=glm(Visitors~., data=data1, family = poisson(link = "log")) #fit linear model
  #predict values for all Area values from the original data
  visitorsP=predict(res,newdata=data.frame(Time=seq(12,13,0.05)), type = "response")
  n = length(seq(12,13,0.05))
  predictedVisitors = rpois(n, visitorsP)
  return(predictedVisitors)
}

set.seed(12345)
res=boot(data2, statistic=f1, R=1000, mle=glm, ran.gen=rng, sim="parametric")
e = envelope(res)
plot(res)

#new.time = data.frame(Time = seq(12,13,0.05))

plot(bank$Time, bank$Visitors, main="Forecasting of visitors depending on time", xlab="Time",
     ylab="Visitors", xlim=c(9,13), ylim=c(30,300))
points(seq(12,13,0.05), (e$point[1,]), col = "red")
points(seq(12,13,0.05), (e$point[2,]), col = "green")
min.value_13 = exp(e$point[2,21])
max.value_13 = exp(e$point[1,21])
cat("the bank should expect between", min.value_13, " and ", max.value_13, "at 13.00" )


#SVM
#########################
# estimate the error for the three kernel support vectors with different C values. 
# Use cross validation with 2 folds to determine the best model. Error on the model is given by cross(model)

library(kernlab)
data(spam)
set.seed(1234567890)
kernel1 = ksvm(type~., data = spam, cross = 2, C = 1, kernel = "rbfdot", kpar = list(sigma = 10))
set.seed(1234567890)
kernel2 = ksvm(type~., data = spam, cross = 2, C = 10, kernel = "rbfdot", kpar = list(sigma = 0.05))
set.seed(1234567890)
kernel3 = ksvm(type~., data = spam, cross = 2, C = 1000, kernel = "rbfdot", kpar = list(sigma = 0.05))

cross(kernel1)
cross(kernel2)
cross(kernel3)
#Kernel 2 gives the best error estimate. Cross = 2 does cross validation automatically. Splits the incoming data 
# into 2 fold and returns the sum(mse)/folds

##NEuralnet

#install.packages('neuralnet')
#library(neuralnet)


#index = sample(50,50)
#tr = data[index[1:25]]
#test = data[index[26:50]]

#install.packages('neuralnet')
#library(neuralnet)

# same idea of cross validation nut have to do the split of the data manually

set.seed(1234567890)
Var <- runif(50, 0, 10)
tr <- data.frame(Var, Sin=sin(Var))
tr1 <- tr[1:25,] # Fold 1
tr2 <- tr[26:50,] # Fold 2


thresh.vector = rep(0,10)
thresh.error = rep(0,10)
MSE = rep(0,2)
#Random varaiable initialization of the weights in the interval [-1,1]
winit = runif(31, -1,1)

nn = neuralnet(Sin~ Var, threshold = 0.001, data = tr1, hidden =c(10), startweights = winit)
prediction = predict(nn, tr2)
MSE[1] = mean((prediction - tr2$Sin)^2)

nn = neuralnet(Sin~ Var, threshold = 0.001, data = tr2, hidden =c(10), startweights = winit)
prediction = predict(nn, tr1)
MSE[2] = mean((prediction-tr1$Sin)^2)

errEstimate = sum(MSE)/2
print(errEstimate)


