file = read.csv2("NIRSpectra.csv")
View(file)
set.seed(12345)
library(fastICA)
library(pls)
library(ggplot2)

df1 = file
df1$Viscosity = c()
res = prcomp(df1)
lambda = res$sdev^2
#eigenvalues
lamda

#Proportion of variation
sprintf("%2.3f", lambda/sum(lambda)*100)
#1 component captures, comp 1 +2 gives +99 variance

#Plot histogram of variance
screeplot(res)

biplot(res)


#Principal componen loading(U)

U = res$rotation
head(U)

#plot scores of coordinated PC1 and PC2, [,1] gives PC1
plot(res$x[,1],res$x[,2], main =" PC1, vs CP2", xlab ="PC1", ylab ="PC2")

##Task 2
#Traceplots 
U = res$rotation
plot(U[,1], main = "Traceplot, PC1", col = "black", xlim = c(0,125), ylim =c(0,0.38))
points(U[,2], main = "Traceplot, PC2", col = "red")


#Task 3
#Perform independent component ananlysis with the nr of components selected in step 1 using
# the fastICA method
#Compute W' = K*W and present the columns for W'in form of the trace plots, compare with previous trace plots
# what kind of measure is presentet by W'
set.seed(12345)
ICA = fastICA(df1, n.comp = 2)
What = ICA$K%*%ICA$W


plot(What[,1],What[,2], main =" latent feature 1vs 2", xlab ="feature 1", ylab ="feature 2")


#Plot the scored of the two latent features and compare  it with the score plot from step 1
plot(What[,1], main = "Traceplot W'1", ylim = c(-10,4))
points(What[,2], main = "Traceplot W'2", col = "red")
