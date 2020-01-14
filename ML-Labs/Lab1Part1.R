my_data <- read.csv2('spambase.csv')
n=dim(my_data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=my_data[id,]
test=my_data[-id,] 

# Y är binomial variable 1/0, What is z value
model <- glm(Spam~., family = binomial(link='logit'), data =train)
summary(model)

test.predict <- predict(model, newdata = test, type ="response")
                        
train.predict <- predict(model, newdata = train, type = "response")

testdata = ifelse(test.predict >0.5, "1", "0")
traindata = ifelse(train.predict >0.5, "1", "0")

confusiontest = table(test[[49]],testdata)
confusiontrain = table(train[[49]], traindata)
print(confusiontest)
print(confusiontrain)

diagTest = sum(diag(confusiontest))
diagTrain = sum(diag(confusiontrain))
sum = sum(confusiontest)
rateTest = 1- diagTest/sum
rateTrain = 1- diagTrain/sum
print(rateTest)
print(rateTrain)



###3 # kollal så de är rätt
testdata2 = ifelse(test.predict >0.8, "1", "0")
traindata2 = ifelse(train.predict >0.8, "1", "0")


confusiontest2 = table(test[[49]],testdata2)
confusiontrain2 = table(train[[49]], traindata2)
print(confusiontest2)
print(confusiontrain2)

diagTest2 = sum(diag(confusiontest2))
diagTrain2 = sum(diag(confusiontrain2))
rateTest2 = 1- diagTest2/sum
rateTrain2 = 1-diagTrain2/sum
print(rateTest2)
print(rateTrain2)


#4
library(kknn)
#kknnmodel = kknn(Spam~.,train, test,k =30)
kknnmodel = kknn(as.factor(Spam)~., train, train,k =30)
kknnprediction = (fitted(kknnmodel))
#as factor gör detta jobbet
#kknnprediction = ifelse(kknnprediction > 0.5, "1", "0")
confusion = table(kknnprediction, train$Spam)
print(confusion)
kknnrate = 1 - sum(diag(confusion))/sum(confusion)
print(kknnrate)

library(kknn)
kknnmodel = kknn(as.factor(Spam)~., train, test,k =30)
kknnprediction = (fitted(kknnmodel))
confusion = table(kknnprediction, test$Spam)
print(confusion)
kknnrate = 1 - sum(diag(confusion))/sum(confusion)
print(kknnrate)


library(kknn)
kknnmodel = kknn(as.factor(Spam)~., train, train,k =30)
kknnprediction = (fitted(kknnmodel))
confusion = table(kknnprediction, train$Spam)
print(confusion)
kknnrate = 1 - sum(diag(confusion))/sum(confusion)
print(kknnrate)
#5
kknnmodel2 = kknn(as.factor(Spam)~., train, train,k =1)
kknnprediction2 = (fitted(kknnmodel2))
confusion2 = table(kknnprediction2, train$Spam)
kknnrate2 = 1 - sum(diag(confusion2))/sum(confusion2)
print(confusion2)
print(kknnrate2)

kknnmodel2 = kknn(as.factor(Spam)~., train, test,k =1)
kknnprediction2 = (fitted(kknnmodel2))
confusion2 = table(kknnprediction2, test$Spam)
kknnrate2 = 1 - sum(diag(confusion2))/sum(confusion2)
print(confusion2)
print(kknnrate2)

