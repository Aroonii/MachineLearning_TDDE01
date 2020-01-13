###Step 1###
library(mboost)
bf <- read.csv2("bodyfatregression.csv")
set.seed(1234567890)
# gradient boosting for optimizing arbitary loss functions where regression trees are utiized as base-learners
m <- blackboost(Bodyfat_percent~Waist_cm+Weight_kg, data=bf)
mstop(m)
cvf <- cv(model.weights(m), type="kfold")
cvm <- cvrisk(m, folds=cvf, grid=1:100)
plot(cvm)
mstop(cvm)

###Step 2####
library(kernlab)
data(spam)
n=dim(spam)[1]
set.seed(1234567890)
id=sample(1:n, floor(n*0.5))
fold1=spam[id,]
fold2=spam[-id,]

#parameters for each model
C <- c(1,5,1,5,1,5)
kernel <- c("rbfdot", "rbfdot", "rbfdot", "rbfdot", "vanilladot", "vanilladot")
width <- c(0.01, 0.01, 0.05, 0.05, 0, 0)

performanceScore = function(traindata, testdata, C, kernel, width) {
  if (width == 0.00){
    svm <- ksvm(type~., data=traindata, kernel=kernel, C=C, cross=2)
  } else {
    svm <- ksvm(type~., data=traindata, kernel=kernel, C=C, cross=2, kpar=list(sigma=width))
  }
  pred <- predict(svm, testdata[,-58])
  CM <- table(pred, testdata[,58])
  MCR <- 1-sum(diag(CM))/sum(CM)
  return (MCR)
}

scores1 = numeric(6)
for (i in 1:6){
  scores1[i] = performanceScore(fold1, fold2, C[i], kernel[i], width[i])
  }
scores2 = numeric(6)
for (i in 1:6){
  scores2[i] = performanceScore(fold2, fold1, C[i], kernel[i], width[i])
}

avgScores = (scores1 + scores2)/2 #The avg performance scores for each model after cross validation

###Step 3###
model <- which.min(avgScores)
final.svm <- svm <- ksvm(type~., data=spam, kernel=kernel[model], C=C[model], kpar=list(sigma=width[model]), cross=2)
# The final model is a Gaussian kernel with C=5 and width=0.01



