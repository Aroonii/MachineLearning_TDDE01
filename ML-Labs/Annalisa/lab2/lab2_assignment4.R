spectra = read.csv("NIRSpectra.csv", header=TRUE, sep=";", dec=",")
set.seed(12345)

####Step 1####
data1 = spectra
data1$Viscosity = c()
res = prcomp(data1)
lambda = res$sdev^2

sprintf("%2.3f", lambda/sum(lambda)*100)
screeplot(res, main="PCA plot")

plot(res$x[,1], res$x[,2], xlab="PC1", ylab="PC2", main="Score plot")

####Step 2####
U = res$rotation
plot(U[,1], main="trace plot, PC1", ylab="loadings")
plot(U[,2], main="trace plot, PC2", ylab="loadings")

####Step 3####
set.seed(12345)
library(fastICA)
a = fastICA(data1, n.comp=2, alg.typ= "parallel", fun="logcosh", alpha=1, method="R", row.norm=FALSE, maxit=200, tol=0.0001, verbose=TRUE)

Wtic = a$K%*%a$W
plot(Wtic[,1], main="traceplot column 1", ylab="W'1")
plot(Wtic[,2], main="traceplot column 2", ylab="W'2")

plot(a$S)

####Step 4####
library(pls)
set.seed(12345)
pcr_model = pcr(Viscosity~., data=spectra, scale=TRUE, validation="CV")
summary(pcr_model)
validationplot(pcr_model, val.type="RMSEP") #root mean squared error
which.min(pcr_model$validation$PRESS)