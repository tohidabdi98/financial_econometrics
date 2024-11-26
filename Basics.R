help.start()
# R as a Calculator
2^3

log(exp(sin(pi/4)^2) * exp(cos(pi/4)^2))
21%%5

# Variables
a = 17
b <- 8
17 -> c
a + b + c
z_42 <- "Hello"
z_42 <- a + b + c
ls()
objects()
rm(z_42)
remove(a)

#scalars

a <- 2 + 2
pi
cos(3*pi/2)
b <- exp(8.17)
class(b)
typeof(a)
a=as.integer(a)
typeof(a)
typeof(b)
typeof(a + b) # double= a vector containing real values
s <- "Hello"
typeof(s)
2 == 3
t <- 2 < 3
typeof(t)

#vectors
v1<- c(2, 3, 5, 8, 4, 6); v1# c stands for "combine" or "concatenate"
length(v1)

typeof(v1) # Type of elements
is.vector(v1)
c(1, 3.14, "Hello")

seq(from=1, to=20, by=2)
seq(1, 20, by=5)
seq(1, 20, length=5)
rep(1, times=10)
#integral
dt=0.01
t=seq(0,pi/6,dt)
f=cos(t)
sin(pi/6)
(I=sum(f*dt))



ones=rep(1,10)
rep(c(1, 2), 3)
rep(c(1, 2), each=3)
trend <- 1981:2005
v1[2]; v1[2:4]; v1[c(1, 4)]
v1[-3]
v1[-1:2] # Error, why?
1:10
v1[-(1:2)]
v1[-c(2, 3, 5)]
v1[3] <- NA; v1
summary(v1)
is.na(v1)
help(NA)
any(is.na(v1))
all(is.na(v1))
v2 <- c(a=32, b=26, c=12, d=41)
v2["b"] <- 22; v2
names(v2)
names(v2) <- c("a1", "a2", "a3", "a4")
v2 > 30
v2[c(T,F,F,T)]
v2[v2 > 30]
which(v2 > 30)
v2 + 100
v1+1:3
v1+1:4 #why error
v1 + v2 # Error, why?
1:4 + v2 ; 1:8 + v2
v2+cos(v2)
length(v2)
sort(v2)
(x <- c(sort(sample(1:20, 9)), NA))
(y <- c(sort(sample(3:23, 7)), NA))
x[x >10]=10;x
index <- x > 9
x[index]
x[x>9]
x[x==1]=NA;x
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)

# Matrices
a=matrix(c(2,3,7,4,1,1.7),ncol = 2)
a
a=matrix(c(2,3,7,4,1,1.7),ncol = 2, byrow = T)
a
A=matrix(1:15, ncol=5)
A; t(A)
B=matrix(1:15, nco=5, byrow=TRUE)
dim(B); length(B)
fix(B)
nrow(A)
ncol(A)
colnames(B)=c("Y","G","C","T","GDP");B
colnames(B)
rownames(B)=c("Tehran","Mashhad","Kermanshah");B
B2 <- B; B2[1, 1] <- "Hello"; B2
typeof(B); typeof(B2)
cbind(A, B)
rbind(A, B)
A[1, 3]; A[2,]; A[, 2]
A[1:3, 2:4]
(g <- seq(1, 20, by=2))
C <- matrix(g, nrow=5)
C1 <- C[c(1,3),1:2]
C[C[, 1] > 6,] # ***
A + B; A * B # Element-wise
A %*% t(B) # Matrix Product
cos(A)
I <- diag(rep(1, 2))
diag(A)
D <- solve(A[1:2, 1:2])
D %*% solve(D)
all(A[1:2, 1:2] %*% D == I) # Why?

apply(A, 2, sum)
apply(A, 1, max)

#Arrays
A <- array(1:12, c(2, 3, 2))# matrix in matrix
A
dim(A); length(A)
nrow(A); ncol(A)
apply(A, 1, mean)
apply(A, 2, sum)
apply(A, 3, sd)

#lists
l1 <- list("Tehran", 1:8); l1
l1[[1]]
l1[[2]] + 10

mylist <- list(sample = rnorm(5),family = "normal distribution",
         parameters = list(mean = 0, sd = 1))
l2 <- list(vect=1:5, text="City", scal=8)
names(l2)
l2$text
l2[c("scal", "vect")]
length(l2); length(l2$vect)
class(l2)
mylist
mylist[[1]]
mylist[1]
mylist[["sample"]]
mylist$sample
mylist[[3]]
mylist[3]
#data.frame
X=data.frame(x1=c(1,3,75),x2=c(3,4,2))
X$x2
fix(X)
X$x3=c(2,3.54,6)
X$x5=X$x1/X$x3
X$lx1=log(X$x1)
X$x3=NULL
Y=subset(X,select=c(x1,x2));Y
Y=subset(X,select=-c(x1,x2));Y
Y=X[,"x1"];Y
Y=X[,c("x1","lx1")];Y
B=data.frame(B) # or  B=as.data.frame(B)
class(B)
B=as.matrix(B)
class(B)
height <- runif(20, 150, 180)
mass <- runif(20, 50, 90)
gender <- sample(c("M", "F"), 20, rep=TRUE)
color <- c("Blue", "Green","Brown")
eyes <- sample(color, 20, rep=TRUE)
table(gender); table(eyes)
table(gender, eyes)
H <- data.frame(height, mass, gender, eyes)
H; summary(H)
head(H)
tail(H)
H[1,]
H$height
H$gender
is.data.frame(H)
is.matrix(H)
as.matrix(H) #H as a matrix
H1=subset(H,mass<70)

#setclass and slots as altenative for list
mo=setClass("mohsen",slots=c(x="numeric",y="numeric"))
t=mo(x=1:10,y=1:10+rnorm(10)) # or t<- new("mohsen", x=1:10,y=1:10+rnorm(10))
t
class(t)
class(mo)
t@x=8:10     
t
slot(t,"x")=1:9
t

mo1<- setClass("mohsen1",slots = c(smooth = "numeric"), contains = "mohsen")
t1 <- mo1(t, smooth = 1:10)
t1
class(t1)
mo2=setClass("mohsen2",slots=c(m="lm"),contains = "mohsen1")
fit=lm(y~x, data=data.frame(x=1:10,y=1:10+rnorm(10)))
t2=mo2(t1,m=fit)
t2
t3=list(smooth=1:10,y=1:10+rnorm(10),m=fit)
t3$smooth
t3$sm
t2@sm# error
t2@smooth
t3[3]
t2[3] # error
objects(t3)
objects(t2) # error


#################Importing and exporting Data#################
getwd()
# ,-separated (Original) csv Format or Comma Separated Values
data_1st <- read.csv("E:/R projects/data for R/FinData1.csv")
data_2nd <- read.table("E:/R projects/data for R/FinData1.csv", header=TRUE, sep = ",")
rm(data_2nd,data_1st)
# ;-separated csv
data2_1st <- read.table("E:/R projects/data for R/FinData2.csv", sep=";",header=TRUE)
data2_2nd <- read.csv2("E:/R projects/data for R/FinData2.csv")
rm(data2_1st,data2_2nd)

# Text File (Tab-separated)
data4 <- read.table("E:/R projects/data for R/FinData3.txt", header=TRUE, sep="\t")

write.csv(data_1st,file="data.csv")
write.table(data_1st,file="data.txt")

A <- seq(1, 10, by=0.5)
write.table(A, "A.txt")
sink("A2.txt")
A
summary(A)
sink()
summary(A)

install.packages("xlsx",dependencies = T)
library("xlsx")
data_3nd=read.xlsx("data.xlsx",sheetName = 1)
data(Journals,package = "AER") # loading data from package

# paste: Concatenate vectors after converting to character.
paste(1:12)
paste0(1:12)# same
as.character(1:12) # same
(nth <- paste(1:12, c("st", "nd", "rd", rep("th", 9))))
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
(nth <- paste(1:2, c("st", "nd", "rd", rep("th", 9))))
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9)),sep=" "))
paste(month.abb, "is the", nth, "month of the year.")
paste(month.abb, letters)
paste(month.abb, "is the", nth, "month of the year.", sep = "_*_")
paste0(nth, collapse = ", ")
paste("1st", "2nd", "3rd", collapse = ", ")
paste("1st", "2nd", "3rd", sep = ", ")
