###Step 1###
crabs = read.csv("australian-crabs.csv")
blue.crabs = crabs[crabs$species=='Blue', ]
orange.crabs = crabs[crabs$species=='Orange', ]

plot(orange.crabs$CW, orange.crabs$BD, pch=21, bg="firebrick2", ylab="BD", xlab="CW", main="plot 1")
points(blue.crabs$CW, blue.crabs$BD, pch=21, bg="dodgerblue3")

##Ans: yes CW and BD seems to be good predictors of the spieces

###Step 2###
library(e1071)
fit.nb <- naiveBayes(species~CW + BD, data = crabs)
nb.predict <- predict(fit.nb, newdata = crabs)
CM <- table(nb.predict, crabs$species)
MCR <- 1-sum(diag(CM))/sum(CM) # 0.395
print (list(CM=CM, MCR=MCR))

## Ans: The mcr is high, 39.5% so the quality of the classification is pretty bad.
## The assumptions of the naive bayes: assumes conditional independence for the distribution of input variables.
## This model is not appropriate for this data bescause: It seems is plot 1 that it is the combination of
## CW and BD that can say which spices a crab is.

###Step 3###
library(MASS)
crabs.factors <- crabs
crabs.factors[,1] <- as.factor(crabs.factors[,1])
fit.log <- glm(species~CW + BD, data=crabs, family="binomial")
pred.log <- predict(fit.log, newdata=crabs, type="response")
summary(pred.log)
plot(pred.log)

##??????

###Step 4###
#create a data set without inputs
crabs.inputs = data.frame(crabs[,7:8])

pcd = prcomp(crabs.inputs, scale=TRUE)
screeplot(pcd)

pcd$rotation # a matrix whose columns contain the eigenvectors


## Ans: It can be seen in plot 1 that the crabs vary more in Carpace Width than in Body Depth within a spieces.
# The equations expressin PC coordinates through original coordinates:
# PC1 = (0.707*CW, 0.707*BD)???
# PC2 = (-0.707*CW, 0.707*BD)???

###Step 5###
pcs <- pcd$x
data <- data.frame(pcs, crabs$species)

fit.nb.pcs <- naiveBayes(crabs.species~PC1 + PC2, data = data)
nb.predict.pcs <- predict(fit.nb.pcs, newdata = data)
CM.pcs <- table(nb.predict.pcs, data$crabs.species)
MCR.pcs <- 1-sum(diag(CM.pcs))/sum(CM.pcs) # 0.05
print (list(CM.pcs=CM.pcs, MCR.pcs=MCR.pcs))

## Ans: The classification quality has increased significantly from a MCR=39.5% to MCR=5%.
## This is because the input data is transfered to another input space where a clearer boundary 
## between the classes can be identified (?)