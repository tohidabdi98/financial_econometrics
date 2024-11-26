## Question 1
seq(-10, 10, 2)


## Question 2
##### a)
normal_samples <- rnorm(100)
head(normal_samples)

##### b)
hist(normal_samples)

##### c)
sample(normal_samples, size=10)


## Question 3
vec_1 <- c(1, NA, 3:4, NA)
vec_1
vec_1[is.na(vec_1)] <- 2
vec_1


## Question 4
mtx_1 <- matrix(seq(1,20), nrow=4, ncol=5, byrow=TRUE)
mtx_1


## Question 5
vec_2 <- c(1, 5, 4, 3, 2, 7, 3.5, 4.3) 
vec_2

##### a)
vec_2[3]

##### b)
vec_2[vec_2>3]

##### c)
vec_2[vec_2>2 & vec_2<4]

##### d)
vec_2[vec_2<3 | vec_2>6]


## Question 6
p <- c(1, 4, 6, 8, 12)
q <- c(-2, -3, 4, 10, 14)
p[q>0]


## Question 7
##### a)
#install.packages("dplyr")
library(dplyr)
a <- seq(-100, 99)
b <- a+5
table <- tibble(a, b)
table

##### b)
nrow(subset(table, a>10 & a<25))


## Question 8
##### a)
vec_3 = seq(0, 100, length.out=1000)
head(vec_3) #first elements of vector

##### b)
sd(vec_3)


## Question 9
##### a)
mtx_2 <- matrix(nrow=30, ncol=30)

##### b)
mtx_2 <- row(mtx_2) * col(mtx_2)

##### c)
mtx_2[1:10, 1:10]