#if(cond) expr
#if(cond) cons.expr  else  alt.expr

#for(var in seq) expr
#while(cond) expr
#?Control
# if statements, conditionals examples 
x=c(1.8, 3.14, 4, 88.169, 13)
if(rnorm(1) > 0) sum(x)
if(rnorm(1) > 0) sum(x) else mean(x)
x= rnorm(10)
if (is.double(x)) print("OK")
if (is.integer(x)) print("KO")
if (x[1] > 0) 1 else -1

if (x[1] > 0) {
  y <- 1
  print("Positive")
  } else {
  y =-1
  print("Non positive")
 }
if (mean(x)<0){ y=1;y=y+1} else{y=2; y=8*y}
# use ifelse if the condition cond is vector
x=c(1.8, 3.14, 4, 88.169, 13)
ifelse(x > 4, sqrt(x), x^2)
y= ifelse(x > 0, 1, -1); y

## switch command
#switch(object,
#       "value1" = {expr1},
#       "value2" = {expr2},
#       "value3" = {expr3},
#       {other expressions}
#)
#If object has value value1 then expr1 is executed, if it has value2 then
#expr2 is executed and so on. If object has no match then other expressions
#is executed. Note that the block {other expressions} does not
#have to be present, the switch will return NULL if object does not match
#any value. An expression expr1 in the above construction can consist of
#multiple statements. Each statement should be separated with a ; or on a
#separate line and surrounded by curly brackets

z = "dqg"
switch(z,cat=print("Hi Felix!"),dog=print("Hi Snowy!"),
           print("What is this pet?"))

#for and loop examples 
x=c(1.8, 3.14, 4, 88.169, 13)
dx=0
for(i in 2:5) { 
  dx[i]=  x[i]- x[i-1]
  }

dx

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

# while loop example: while(cond) expr
x=c(1,2,3.4,5)
while(sum(x) < 100) {
   x=2 * x
  }
x

####examples
x =  c(17, 8, 42, 3)
for (e in x) print(e)

for (i in 1:length(x)) print(x[i])

for (i in seq(x)) print(x[i]) #seq_along()=seq()

for (i in seq(5)) {
  fact=prod(1:i)
  cat(i, "! = ", fact,"\n",  sep="")
}

for(n in c(2,5,10,20,50)) {
  x <- stats::rnorm(n)   # stats::rnorm=rnorm means rnorm in package stats(=)
  cat(n, ": ", sum(x^2), "\n", sep = "")
}

for (k in seq(10)) {
  if (k %% 2 == 0) 
  print(k)
}
for (k in seq(10)) if (k %% 2 != 0)  print(k)

for (k in seq(10)){ if (k %% 2 == 0) next 
print(k)}    # or 

for (k in seq(10)){ if (k %% 2 != 0) print(k)}

# while example for sum
i=1; s =0
 while (i<= length(x)) { s <- s + x[i]
 i <- i + 1
 }
s

x=1

#Central limit theorem
n=0
while(n<=40) {
  n=n+2
  x=c()
  for(i in 1:10000) {
    y=runif(n)
    x[i]=mean(y)
  }
  hist(x, freq=F, col="orange", breaks=100)
  Sys.sleep(0.05)
}

#example for repeat and break
x=0
repeat {
  print(x)
  x <- x + 1
  if (x == 10) break
}


#Central limit theorem
n=0
while(n<=40) {
  n=n+2
  x=c()
  for(i in 1:10000) {
    y=runif(n)
    x[i]=mean(y)
  }
  plot(density(x))
  Sys.sleep(0.05)
}
#example for repeat and break
x=0
repeat {
  print(x)
  x <- x + 1
  if (x == 10) break
}

x=c(1,2,3.4,5)
while(sum(x) < 100) x=2 * x
x

x=c(1,2,3.4,5)
while(sum(x) < 100) x=2 * x; x=x+1
x

x=c(1,2,3.4,5)
while(sum(x) < 100) {x=2 * x; x=x+1}
x

x=c(1,2,3.4,5)
while(sum(x) < 100) {
  x=2 * x
   x=x+1
}
   x
   