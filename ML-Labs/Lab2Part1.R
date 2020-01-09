set.seed(12345)
library(MASS)
library(ggplot2)

1.1
crab.data =read.csv("australian-crabs.csv")
ggplot(crab.data, aes(x = CL, y = RW, color = sex)) +
  geom_point()


#1.2
missclassification = function(table, data){
  correct = sum(diag(confusion))
  total = nrow(data)
  return (1 -(correct/total))
}

confusion = function(data, prediction){
  return (table(actual = data, predicted = prediction))
}

lda.model = lda(sex~CL+RW, crab.data)
prediction1 = predict(lda.model)

confusion = table(actual= crab.data$sex, prediction = prediction1$class)
confusion
missclassification(confusion, crab.data)

ggplot(crab.data, aes(x = crab.data$CL, y = crab.data$RW, col = prediction1$class)) +
  geom_point()


#1.3
prediction2 = predict(lda.model, prior = c(0.9, 0.1))
confusion2 = table(actual= crab.data$sex, prediction = prediction2$class)
#missclassification(confusion2, crab.data)
confusion2
1 - sum(diag(confusion2))/nrow(crab.data)

ggplot(crab.data, aes(x = CL, y = RW, col = prediction2$class)) +
  geom_point()
#More are predicted to be males since prior is saying that there are more males. Missclass rate increases

#1.4

logistic.model = glm(sex~CL+RW, data = crab.data, family = binomial)
predictor.dataframe = data.frame(Cl = crab.data$CL, RW = crab.data$RW)
colnames(predictor.dataframe) = c("CL", "RW")
prediction3 = predict(logistic.model, data = predictor.dataframe, type = "response")

log.regression = ifelse(prediction3 > 0.5, "Male", "Female")
log.regression2 = ifelse(prediction3 > 0.1, "Male", "Female")




intercept = (logistic.model$coefficients)[1]/(-logistic.model$coefficients)[3]
slope = (logistic.model$coefficients)[2]/(-logistic.model$coefficients)[3]
slopefunc = function(x){intercept+slope*x}


plot = ggplot(crab.data, aes(x = CL, RW, color = log.regression)) +
  geom_point()
plot = plot + stat_function(fun = slopefunc, colour= "green")
plot



dataframe.sex = data.frame(sex = log.regression)
plot(crab.data$CL, crab.data$RW, col = dataframe.sex$sex, xlab = "CL", ylab = "RW")
abline(intercept, slope, col = "blue")

table = table(prediciton = log.regression, actual =crab.data$sex)
table
1 - sum(diag(table))/nrow(crab.data)










