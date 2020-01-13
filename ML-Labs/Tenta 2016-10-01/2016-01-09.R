RNGversion('3.5.1')
data = read.csv('crx.csv')

#Assignment 1
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
train=data[id,]
test=data[-id,] 

#install.packages('tree')
#library(tree)
par(mfcol = c(1,1))
tree = tree(Class~., data = train)
plot(tree)
text(tree, pretty = 0)

train.subset = train[-2,]
tree.subset = tree(Class~., data = train.subset)
plot(tree.subset)
text(tree.subset, pretty = 0)

#sturcture does not change at all

#prune tree to optimal nr of leafs by using cross validation. Provide a cross validation plot and comment how 
# many leaves the optimal tree should have. Which variables were selected by the tree

set.seed(12345)
CVtree = cv.tree(tree)
plot(CVtree)
plot(CVtree$size, CVtree$dev)
plot(log(CVtree$k), CVtree$dev)
which.min(CVtree$size)
which.min(CVtree$dev)

#min value is given by leafs = 5 and uses variables A4, A10, A6
CVtree$dev[3]
finalTree = prune.tree(tree, best = 5)
plot(finalTree)
text(finalTree, pretty = 0)
