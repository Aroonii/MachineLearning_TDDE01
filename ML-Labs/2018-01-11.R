RNGversion('3.5.1')
data = read.csv("video.csv")
View(data)
set.seed(12345)

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,] 

train_subset = subset(train, select = -c(codec, utime) )
res = prcomp(train_subset)
lambda = res$sdev^2
sprintf("%2.3f", lambda/sum(lambda)*100)

screeplot(res)
biplot(res)

scaled_train_subset = scale(train_subset)

res2 = prcomp(scaled_train_subset)
lambda2 = res2$sdev^2
variance.vector = as.factor(sprintf("%2.3f", lambda2/sum(lambda2)*100))

sum(variance.vector)
screeplot(res)
biplot(res)