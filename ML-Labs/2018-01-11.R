RNGversion('3.5.1')
data = read.csv("video.csv")
datas <- subset(data, select = -c(codec))
set.seed(12345)
n=dim(datas)[1]

id=sample(1:n, floor(n*0.5))
train=datas[id,]
test=datas[-id,] 

train_subset = subset(train, select = -c(utime) )
res = prcomp(train_subset)
lambda = res$sdev^2
sprintf("%2.3f", lambda/sum(lambda)*100)
screeplot(res)
biplot(res)
res$rotation


scaled_train_subset = scale(train_subset)



res3 =princomp(scaled_train_subset)
lambda3 = res3$sdev^2
variance.vector2 = as.factor(sprintf("%2.3f", lambda3/sum(lambda3)*100))
variance.vector2
screeplot(res3)
biplot(res3)
plot(res3$scores[,1], res3$scores[,2])
summary(res3)

#för att få upp till 95% med scaling används PC 1 - 9 => 95.387
res3$loadings


#part 2
install.packages("pls")
library(pls)
#newtrain = subset(train, select = -c(codec))

pcr = pcr(utime~ ., ncomp = 17, data =train, scale = TRUE  )


prediction.func.test = function(i){
  prediction.pcr = predict(pcr, newdata = test, ncomp = i, type = "response")
  MS = mean((prediction.pcr - test$utime)^2)
  return(MS)
}

prediction.func.train = function(i){
  prediction.pcr = predict(pcr, newdata = train, ncomp = i, type = "response")
  MS = mean((prediction.pcr - train$utime)^2)
  return(MS)
}


nr.comp = seq(1,17,1)
MSE.test = sapply(nr.comp, prediction.func.test)
MSE.train = sapply(nr.comp, prediction.func.train)
nr.comp[which.min(MSE.test)]
nr.comp[which.min(MSE.train)]
match(c(min(MSE.test)), MSE.test)
match(c(min(MSE.train)), MSE.train)


plot(nr.comp, MSE.train, col = "red", ylim = c(70,270), type = "b")
points(nr.comp, MSE.test, xlab = nr.comp, type = "b")
legend("topright", legend = c("train", "test"), cex = 1.4, col =c("red", "black"), pch = c(1,1,1))

## As nr of components increases so does the complexity.The bias gets lower as the complexity increases (bias is the innability to capture the true relation)
# as complexity goes up though added components the bias goes down and the variance increases. As the variance 
# increases the MSE gets lower. For the test data the best value is found (based on MSe) when M = 14. 
#MSE for training data always gets lower the more complex the model becomes since the bias gets lower.
#MSE for the test data gets lower up until M = 14, after this the model becomes overfitted and MSE gets higher


#also working

#MSE = rep(0,17)
#for (i in 1:17){
#  prediction = predict(pcr, newdata = test, ncomp = i)
#  MSE[i] = mean((prediction - test$utime)^2)
#}
#MSE
#plot(MSE)
#nr.comp[which.min(MSE.test)]
#nr.comp[which.min(MSE.train)]
#match(c(min(MSE.test)), MSE.test)
#match(c(min(MSE.train)), MSE.train)
#which.min(MSE)

## assignment 3 
# use PCR model with M = 8 and report a fitted probabilistic model that shows the connection between the 
#target and the principal compontnes

pcr.8 = pcr(utime~ ., ncomp = 8, data =train, scale = TRUE  )

#Yloadings visar hur responvariabel utime beror på principal components
pcr.8$Yloadings
#för att ta fram variancen till att skriva normalfördelningen (fitted probabilistic modell)
variance = pcr.8$residuals^2
mean(variance)
#=> Utime(scaled ) N~(pcr.8$loadings[,1] * comp1 + pcr.8$loadings[,2] * comp2 +......., variance (standardavikele^2))



## assignment 4
#use orignial data to create a variable class that shows "mpeg" if variable is codec = "mpeg4" and "other" otehrwise
#create a plot "duration" versus "fframes" where cses are colored by "class". Do you think that the classes are 
# easily separable by linear decision boundary
install.packages("ggplot2")
library(ggplot2)
mpeg.frame = data.frame(mpeg = rep("0",1000))
newdataframe = cbind(data, mpeg.frame)
class = ifelse(newdataframe$codec=='mpeg4', 'mpeg', 'other')
mydata = cbind(data, class)
ggplot(mydata, aes(x = duration, y = frames, color = class)) +
         geom_point()

plot(mydata$duration, mydata$frames,  col=mydata$class)

#They are failry separable so a linear decision boundary could be useful

##part 5 
# fit a linear discriminant analysis mmodel with "class" as target and "frames" and "duration" as features to the
#entire dataset (SCALE THE FEUTURES ) PRODUCE the plot showing the classified data and report the
#training error. # explain why LDA was unable to achieve perfect (or nearly perfecr)
#clasification in this case

install.packages('MASS')
library(MASS)

temp = subset(mydata, select = -c(frames, duration))
features = subset(mydata, select = c(frames, duration))
feature = scale(features)
newdata = cbind(temp, feature)
#enklare
#mydata$duration <- scale(mydata$duration)
#mydata$frames <- scale(mydata$frames)

LDA = lda(class~ frames + duration, newdata)
predict.lda = predict(LDA)
class.prediction = predict.lda$class
mean(class.prediction!= newdata$class)
table(actual = newdata$class, prediction = class.prediction)
ggplot(newdata, aes(x=duration, y = frames, col = class.prediction)) +
  geom_point()

## missclassificationrate på 0.172. Classficeringen är relativt dålig. detta beror på att covariansmatriserna
# per class är olika. Lutningarna är olika vilket gör att data kommer att bli felklassificerad


##part 6
#fit a decison tree mdel with class as target and framer and duration aas features to the entire dataset, choose
#an appropriiate tree size by cross validation. report the training error. How many leaves are there
# in the final tree? ecplain why suhc a complicated tree is needed to describe such a 
#simple decison boundary

install.packages('tree')
library(tree)

treefit = tree(class ~ duration + frames, data = mydata)
summary(treefit)

tree.fit = tree(class~ frames + duration, data = mydata)
#fit.deviance = tree(class~ frames + duration, data = mydata, split = "deviance")

tree.predict = predict(tree.fit, type = c("class"))


#gini.predict = predict(fit.gini, type = c("class"))
#deviance.predict = predict(fit.deviance, type = c("class"))
#mean(gini.predict!=mydata$class)
#mean(deviance.predict!=mydata$class)
#table(gini.predict, mydata$class)
#table(deviance.predict, mydata$class)
mean(tree.predict != mydata$class)
table(tree.predict, mydata$class)
#treecomplexity.gini = cv.tree(fit.deviance)
#treecomplexity.deviance = cv.tree(fit.gini)
tree.complexity = cv.tree(tree.fit)
#plot(treecomplexity.gini$size, treecomplexity.gini$dev, type = "b")
#plot(treecomplexity.deviance$size, treecomplexity.deviance$dev, type = "b")
plot(tree.complexity$size, tree.complexity$dev, type ="b")
score = rep(0,11)
for (i in 2:11){
  pruned.tree = prune.tree(tree.fit,best = i)
  #prediction = predict(pruned.tree, type = "tree")
  score[i] = deviance(pruned.tree)
}
plot(2:11, score[2:11], xlab = "leafs", type = "b", ylab = "deviance" )
plot(2:11, score[2:11])

match(c(min(score[2:11])), score)
best.tree = prune.tree(tree.fit, best = 11)
plot(best.tree)
text(best.tree, pretty = 0)
best.prediction = predict(best.tree, type = "class")
pred = ifelse(best.prediction[,1]>best.prediction[,2], 'mpeg', 'other')
mean(best.prediction!=mydata$class)
table(best.prediction, mydata$class)

#lowest error is given using 11 leaves (tree size) hence that MSE is same as before
# it needds many leaves (more complicated) because a linear line (the one we need to split the classes)
#needs many splits to produce a stair kind of decision boundary to fit the classifiation
# since the optimal decision boundary is linear but not paralell to any of the coordinate axes
#accordingly the decision tree would need to produce many splits ( sätta krav om > x => frames osv) för att
#kunna producera a "stir lind of decision boundary 
ggplot(mydata, aes(x = duration, y = frames, col = best.prediction)) +
  geom_point()
