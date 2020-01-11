RNGversion('3.5.1')
#data = read.csv2('spambaseTenta.csv')
#View(data)

#use the function kvsm from the paggage kernlag to learn support vector machine (SVM) for classifying the spam .
#Consier the radaial basis fucnction kernel (also known as gaussian) with a width pf 0.05 
#For the C paramteter cosider vvalue 0.5, 1 and 5. This implies that you have to consider three models

#part 1 perform ,model selection ie selctthe most promising of the three ,models ude any method of your choice exept cross
#validation or nested corss validation

install.packages('kernlab')
library(kernlab)
data(spam)


n=dim(spam)[1]
set.seed(1234567890)
id=sample(1:n, floor(n*0.5))
train=spam[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=spam[id2,]
id3=setdiff(id1,id2)
test=spam[id3,]

# a bigger value on width causes the model to take more observations in its close surrounding in account

#help(predict.ksvm)

#width = 0.5
# we do not want to have the predictor in the prediciton. that is why we take it away => valid[-,58]
kernel1 = ksvm(type~., data = train, kernel = "rbfdot", C = 0.5, kpar = list(sigma = 0.05))
prediction1 = predict(kernel1, newdata = valid[,-58])
table1 = table(prediction1 = prediction1, actual =  valid$type)
mean(prediction1!=valid$type)
# MSE = 0.09391
(table1[1,2]+table1[2,1])/sum(table1)
table1

kernel2 = ksvm(type~., data = train, kernel = "rbfdot", C = 1, kpar = list(sigma = 0.05))
prediction2 = predict(kernel2, newdata = valid[,-58])
table2 = table(prediction2 = prediction2, actual =  valid$type)
mean(prediction2!=valid$type)
# MSE = 0.08347
(table2[1,2]+table2[2,1])/sum(table2)
table2

kernel3 = ksvm(type~., data = train, kernel = "rbfdot", C = 5, kpar = list(sigma = 0.05))
prediction3 = predict(kernel3, newdata = valid[,-58])
table3 = table(prediction3 = prediction3, actual =  valid$type)
mean(prediction3!=valid$type)
(table3[1,2]+table3[2,1])/sum(table3)
table3
# MSE = 0.0904

#chose sigma = 1 for best MSE

# part 2
#Estimate generalization error = how well it performs on new unseen data. should use both valid and train and valid
id4 <- c(id, id2)
trva = spam[id4,]
kernel <- ksvm(type~., data=trva, kernel="rbfdot",kpar=list(sigma=0.05),C=1)
prediction.kernel <- predict(kernel,trva[,-58])
mean(prediction.kernel != trva$type)
#=> MSE = 0.0391

#part 3 produce the SVM that will be returned to the user ie show the code
#what is the purpuse of the paramter C => cost of contraint errors
# the kernel method will allow for missclassification and therefore gain a better prediction.
# this erre can naturally  of course not be too big


##Neural network
#train a nerual network to learn the sin fucntion. sample 50 datapoints . aplly the sine function to each
#use train/dat 50%. Validation is uses forealry stop of the gradient decent. Consider threshhold values
#i/1000 with i =1..10. Initalize the weights of the nerural network to random valus i  the interval [-1,1]
#consider two NN architectures. A signle hidden layer of 10 units and two hidden layers with 3 units each.
#choose the most appopriate NN architecture and threshhold values. Motivate your anwer

#estimate the generalization error of the selected NN
data = sample(10,50, replace = T)

#index = sample(50,50)
#tr = data[index[1:25]]
#test = data[index[26:50]]

install.packages('neuralnet')
library(neuralnet)

trva.neural = data.frame(Var = data, Sin = sin(data))

tr = trva.neural[1:25,] #Training
va = trva.neural[26:50,] #validation
thresh.vector = rep(0,10)
thresh.error = rep(0,10)
#Random varaiable initialization of the weights in the interval [-1,1]
winit = runif(31, -1,1)

for(i in 1:10){
  thresh = i/1000
  thresh.vector[i] = thresh
  nn = neuralnet(Sin~ Var, threshold = thresh, data = tr, hidden =c(10), startweights = winit)
  prediction.val = predict(nn, va)
  MSE = mean((prediction.val - va$Sin)^2)
  thresh.error[i] = MSE
}
bestthresh = thresh.vector[which.min(thresh.error)]
which.min(thresh.error)
min(thresh.error) # => thresh error = 0,00017 best

plot(thresh.vector, thresh.error, xlab ="thresh", ylab = "MSE", type ="l")
axis(1, at=(1:10), labels = thresh.vector)

nnbest = neuralnet(Sin~ Var, threshold = bestthresh, data = tr, hidden =c(10), startweights = winit)
prediction.best = predict(nnbest, va)
MSE_best = mean((prediction.best - va$Sin)^2)
MSE_best
# generalization error = 0.00017

plot(nnbest)
#plot the prediction, black dots and the data red dots
plot(va$Var, prediction.best)
points(va$Var, va$Sin, col ="red")


## with hidden layer 2X3
winit = runif(22, -1,1)
for(i in 1:10){
  thresh = i/1000
  thresh.vector[i] = thresh
  nn = neuralnet(Sin~ Var, threshold = thresh, data = tr, hidden =c(3,3), startweights = winit)
  prediction.val = predict(nn, va)
  MSE = mean((prediction.val - va$Sin)^2)
  thresh.error[i] = MSE
}
bestthresh = thresh.vector[which.min(thresh.error)]
which.min(thresh.error)
min(thresh.error) # = 0.4039
#best threshhold i given for i = 4 => threshold 0.004

plot(thresh.vector, thresh.error, xlab ="thresh", ylab = "MSE", type ="l")
axis(1, at=(1:10), labels = thresh.vector)

nnbest = neuralnet(Sin~ Var, threshold = bestthresh, data = tr, hidden =c(3,3), startweights = winit)
prediction.best = predict(nnbest, va)
MSE_best = mean((prediction.best - va$Sin)^2)

plot(nnbest)
#plot the prediction, black dots and the data red dots
plot(va$Var, prediction.best)
points(va$Var, va$Sin, col ="red")

# MSE = 2.8615
# prediction is vero low since the model want to minimize the MSE and does so by lowering the predicted value
#more hidden layers ar in most cases not necesarry. Number of nodes should usually be a number between
#the size of the input and the size of the output layers. Is increased if amount of nodes is increases
