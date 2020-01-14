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
plot(glm)
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
data1 = bank

mle=lm(Visitors~Time, data=data2)

rng=function(data, mle) {
  data1=data.frame(Visitors=data$Visitors, Time=data$Time)
  n=length(data$Visitors)
  #generate new data 
  data1$Visitors=rnorm(n,predict(mle, newdata=data1),sd(mle$residuals))
  return(data1)
}
f1=function(data1){
  res=lm(Visitors~., data=data1) #fit linear model
  #predict values for all Area values from the original data
  visitorsP=predict(res,newdata=time)
  n = nrow(time)
  predictedVisitors = rnorm(n, visitorsP, sd = sd(glm$residuals))
  return(predictedVisitors)
}

res=boot(data2, statistic=f1, R=1000, mle=mle, ran.gen=rng, sim="parametric")
plot(res)

#res=boot(Dataframe, statistic=f1, R=1000, mle=linear_model, ran.gen=rng, sim="parametric")
e=envelope(res)
plot(bank$Time, bank$Visitors, main="Forecasting of visitors depending on time", xlab="Time",
     ylab="Visitors", xlim=c(9,13), ylim=c(30,500))
points(bank$Time, res$t0, col = "red")

points(time, exp(e$point[2,]), type="l", lty=21, col="gray")
points(seq(12,13,0.05), exp(e$point[1,]), type="l", lty=21, col="gray")

min_value_13=exp(e$point[2,21])
max_value_13=exp(e$point[1,21])
cat("The bank should expect between", min_value_13, "and", max_value_13, "customers", sep=" ")

