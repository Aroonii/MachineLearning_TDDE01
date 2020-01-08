RNGversion('3.5.1')

set.seed(12345)
data =read.csv("creditscoring.xls", header = TRUE, sep = ";")
data = creditscoring
#Divide data into training/validation/test as 50,25,25
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

#OR
#spec <- c(train = .5, validation = .25, test = .25)
#size_csvfile <- dim(csvfile)[1]
#set.seed(12345)
#g <- sample(cut( seq( nrow(csvfile) ), nrow(csvfile)*cumsum(c(0,spec)), labels = names(spec) ))
#res <- split(csvfile, g)



#2.2 
#Decision tree is a form of supervised learning to solve regression and classification problems
#Classification tree used to predict a quallitative response (good_bad)
# response is predicted based on the most occuring ocurring class of training
library(tree)




#GINI
#Gini looks at the purity of a region. The quanity of much of the training data belongs to a singel class
train = as.data.frame(train)
label = ifelse(train$good_bad=="good", 1, 0)
n = dim(train)[1]

fit.gini = tree(as.factor(good_bad)~. , split = c("gini"), data = train)  
fit.deviance = tree(as.factor(good_bad)~. , split = c("deviance"), data = train)

plot(fit.gini)
plot(fit.deviance)

gini.predict.test = predict(fit.gini, newdata = test, type = "class")
gini.predict.train = predict(fit.gini, newdata = train, type = "class")
ginitable.test = table(actual = test$good_bad, prediction = gini.predict.test)
ginitable.train = table(actual = train$good_bad, prediction = gini.predict.train)
missclassification(ginitable.test, test)
missclassification(ginitable.train, train)

deviance.predict.test = predict(fit.deviance, newdata = test, type = "class")
deviance.predict.train = predict(fit.deviance, newdata = train, type = "class")
deviancetable.test = table(actual = test$good_bad, prediction = deviance.predict.test)
deviancetable.train = table(actual = train$good_bad, prediction = deviance.predict.train)
missclassification(deviancetable.test, test)
missclassification(deviancetable.train, train)

gini.test.missclass = mean(gini.predict.test != test$good_bad)
gini.train.missclass =mean(gini.predict.train != train$good_bad)
deviance.test.missclass =mean(deviance.predict.test != test$good_bad)
deviance.train.missclass =mean(deviance.predict.train != train$good_bad)


print(gini.test.missclass)
print(gini.train.missclass)
print(deviance.test.missclass)
print(deviance.train.missclass)


## Task 3
treecomplexity = cv.tree(fit.deviance)
plot(treecomplexity$size, treecomplexity$dev, type="b", col="red")

trainingscore = rep(0,15)
validationscore = rep(0,15)

for (i in 2:15){
  prunedtree = prune.tree(fit.deviance, best = i)
  prediction = predict(prunedtree, newdata = valid, type = "tree")
  trainingscore[i] = deviance(prunedtree)
  validationscore[i] = deviance(prediction)
}
plot(2:15, validationscore[2:15])


plot(2:15, trainingscore[2:15], xlab = "leafs",type = "b", ylab = "deviance", ylim =c(250,600), col = "black")
axis(side=1, at=c(2:15))
legend("topright", col = c("black", "red"), legend = c("Training Data", "Validation Data"), pch = c(1,12))
points(2:15, validationscore[2:15], col = "red", type = "b")

which.min(validationscore[2:15])
# varför säger den index 3 ?
match(c(min(validationscore[2:15])), validationscore)
# best is 4

bestTree = prune.tree(fit.deviance, best = 4)
plot(bestTree)
text(bestTree, pretty = 0)
prediction = predict(bestTree, newdata = test, type = "class")
newtable = table(actual = test$good_bad, prediction = prediction)
newtable
mean(prediction!=test$good_bad)

###
#Task 4















#######################################################################################


#Task 3
#. Use training and validation sets to choose the optimal tree depth. 
#Present the graphs of the dependence of deviances for the training and the validation
#data on the number of leaves. Report the optimal tree, report it’s depth and
#the variables used by the tree. Interpret the information provided by the tree
#structure. Estimate the misclassification rate for the test data.

#cv.tree = k-fold cross validation to find the deviance/nr if missclassifications as 
# a function of the cost complexity parameter k

treecomplexity = cv.tree(deviance_tree_factor)
plot(treecomplexity$size, treecomplexity$dev, type="b", col="red")

trainingscore = rep(0,12)
validationscore = rep(0,12)

for (i in 2:12){
  prunedtree = prune.tree(deviance_tree_factor, best = i)
  prediction = predict(prunedtree, newdata = valid, type = "tree")
  trainingscore[i] = deviance(prunedtree)
  validationscore[i] = deviance(prediction)
}
plot(2:12, validationscore[2:12])


plot(2:12, trainingscore[2:12], xlab = "leafs",type = "b", ylab = "deviance", ylim =c(250,600), col = "black")
axis(side=1, at=c(2:12))
legend("bottomright", col = c("black", "red"), legend = c("Training Data", "Validation Data"), pch = c(1,12))
points(2:12, validationscore[2:12], col = "red", type = "b")

which.min(validationscore[2:12])
# varför säger den index 3 ?
match(c(min(validationscore[2:12])), validationscore)
# best is 4

bestTree = prune.tree(deviance_tree_factor, best = 4)
plot(bestTree)
prediction = predict(bestTree, newdata = test, type = "class")
newtable = table(actual = test$good_bad, prediction = prediction)
missclassification(newtable, test)

#################################################################################3

# TASK 4
#Use training data to perform classification using Naïve Bayes and report the
#confusion matrices and misclassification rates for the training and for the
#test data. Compare the results with those from step 3

library(e1071)
set.seed(12345)
naiveBayesClassification = naiveBayes(as.factor(good_bad) ~., data = train)

#Prediction on testdata
predictionBayes = predict(naiveBayesClassification, newdata = test)
bayesTable = table(actual = test$good_bad, predicted = predictionBayes)
bayesTable
mean(predictionBayes!= test$good_bad)

#Prediction on trainingdata
predictionBayes2 = predict(naiveBayesClassification, newdata = train)
bayesTable2 = table(actual = train$good_bad, predicted = predictionBayes2, main = "test")
bayesTable2
mean(predictionBayes2 != train$good_bad)

#Task 5
#Use the optimal tree and the Naïve Bayes model to classify the test data by using
#the following principle:

#Compute the TPR and FPR values for the
#two models and plot the corresponding ROC curves. Conclusion?

#type is as default class => will´predict good/bad, raw gives the probablity
naiveBayPredict = predict(naiveBayesClassification, newdata = test, type = "raw")
naiveBayPredict
#type is as default vector => gives the probability while class would have given class prediction
optTreePredict = predict(bestTree, newdata = test, type = "vector" )
optTreePredict

pi = seq(0.05, 0.95, 0.05)

BayesTPR = rep(0, length(pi))
BayesFPR = rep(0, length(pi))

TreeTPR = rep(0, length(pi))
TreeFPR = rep(0, length(pi))

#varför inte [i,2] ?
for (i in 1:length(pi)){
  naiveBayesRoc = ifelse(naiveBayPredict[,2] > pi[i], "good", "bad")
  Matrix.Bayes = as.matrix(table(predicted = as.factor(naiveBayesRoc), actual =test$good_bad))
  BayesTPR[i] = Matrix.Bayes[2,2] / (Matrix.Bayes[2,2] + Matrix.Bayes[1,2])
  BayesFPR[i] = Matrix.Bayes[2,1] /(Matrix.Bayes[2,1] + Matrix.Bayes[1,1])
  
    temp = ifelse(optTreePredict[,2] > pi[i], "good", "bad")
      t = table(pred = temp, actual =test$good_bad)
      if(nrow(t)>1){
        TreeTPR[i] = t[2,2] / (t[2,2] + t[1,2])
        TreeFPR[i] = t[2,1] /(t[2,1] + t[1,1])
      }
     }

bayesframe = data.frame(BayesFPR, BayesTPR)
treeframe = data.frame(TreeFPR,TreeTPR)
plot(treeframe)
plot(bayesframe, type = "b", col = "red")
points(treeframe, type = "b", col = "black")
abline(a=0, b =1)
legend("bottomright", lty = c(1, 1, 1), col = c("red", "black"), legend = c("Bayes", "Tree"))

#Not working
ggplot(bayesframe)
ggplot(data = NULL, aes(col = "classifier")) +
  geom_point(data = bayesframe, aes(x = BayesFPR, y = BayesTPR, col = "Bayes"))
  geom_point(data = treeframe, aes(x = TreeFPR, y = TreeTPR, col = "tree"))

## not working
plot = ggplot(bayesframe)
Matrix.Bayes
ggroc(plot)
dataframe2 = data.frame(TreeFRP, TreeTPR)
ggplot(dataframe2, aes(x = TreeFPR, y=TreeTPR, col="red"))+geom_point()

plot(c(1,BayesFPR,0), c(1,BayesTPR,0), xlim =c(0,1), ylim =c(0,1), type = "b")
points(c(TreeFPR[TreeFRP<=1],0), 
       c(TreeTPR[TreeTPR<=1],0))


##TASK 2.6
set.seed(12345)
new.bayes.model = naiveBayes(as.factor(good_bad)~., data = train, params = list(loss = matix(c(0,10,1,0), 2,2)))
new.bayes.model
new.bayes.predict.test = predict(new.bayes.model, newdata = test, type ="raw")
new.bayes.predict.train = predict(new.bayes.model, newdata = train, type ="raw")

new.bayes.predict.test = ifelse(1*new.bayes.predict.test[,1] > 10*new.bayes.predict.test[,2], "bad", "good")
new.bayes.predict.train = ifelse(1*new.bayes.predict.train[,1] > 10*new.bayes.predict.train[,2], "bad", "good")

table(actual = new.bayes.predict.test, test$good_bad)
mean(new.bayes.predict.test!=test$good_bad)

table(actual = new.bayes.predict.train, train$good_bad)
mean(new.bayes.predict.train!=train$good_bad)
