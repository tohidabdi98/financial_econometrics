#function in R
#functionname=function(arg1,arg2,...){
#  <<expression>>
#  }
test <- function(x) sqrt(x)
test(4)
test(c(2,4))

# trimmed mean
tmean <- function(x, k) {
  xt <- quantile(x, c(k, 1 - k))  
  mean(x[x > xt[1] & x < xt[2]])
}
test <- rnorm(100)
tmean(test, 0.05)

power <- function(x, k = 2) {
  x^k
}
 
power(5)
power(5,3)


cmeans=function(X){
  rval <- rep(0, ncol(X))
  for(j in 1:ncol(X)) {
  mysum <- 0
  for(i in 1:nrow(X)) mysum <- mysum + X[i,j]
  rval[j] <- mysum/nrow(X)
  }
  return(rval)
  }

y=matrix(1:20, ncol = 2)
cmeans(y)  
colMeans(y)

cmeans2=function(X) {
  rval <- rep(0, ncol(X))
  for(j in 1:ncol(X)) rval[j] <- mean(X[,j])
  return(rval)
}

cmeans2(y)

# apply
y=matrix(1:20, ncol = 2)
apply(y,2,mean)
apply(y, 2, max)
thresh <- function(x, d){
  sum(x > d)}
M= matrix(rnorm(10000), ncol = 100)
apply(M, 2, thresh, 0.6)
M1=data.frame(M)
apply(M1, 2, thresh, 0.6)

#sapply()
sapply(M1, thresh, 0.6) # the same as:  apply(M1, 2, thresh, 0.6)

sapply("car.test.frame", is.numeric)

myf=function(x) {
  n =as.integer(sum(x))
  out=1:n
  out
}
testdf <- as.data.frame(matrix(runif(25), ncol = 5))
sapply(testdf, myf) #result is an object with a list structure

#tapply()
x <- rnorm(50)
y <- as.factor(sample(c("A", "B", "C", "D"),size = 50, replace = TRUE))
tapply(x, y, mean, trim = 0.3)

 #arrays and Apply


A=array(1:12, c(2, 3, 2)) # matrix in matrix
A
dim(A); length(A)
nrow(A); ncol(A)
apply(A, 1, mean)
apply(A, 2, mean)
apply(A, 3, mean)

t=array(1:24,dim=2:4) # or array(1:24,c(2,3,4)
t
apply(t, 1, sum)
apply(t, 3, sum)
apply(t, 1:2, sum)
res=apply(t,3,function(x) runif(max(x)))
res
x=rnorm(100)
bins=cut(x, breaks=-4:4)
bins=cut(x,c(-10,-2,1,3,100))
tapply(x, bins, mean)
lapply(res, mean)
sapply(res, mean)
lapply(res, quantile)
sapply(res, quantile)

v <- replicate(500,mean(rnorm(10)))
boxplot(v)
rep(x=1:4, times=4:1)
mapply(rep, x=1:4, times=4:1)
