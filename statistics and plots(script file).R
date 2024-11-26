# statistics in R
data=SWX 
#The SWX data set contains six financial time series. The first three are Swiss indexes from the Swiss Exchange in Zurich
#the Swiss Performance Index,SPI, the Swiss Bond Index, SBI, and the Swiss Immofund Index (reits), SII. 
#The remaining three time series, named LP25, LP40, LP60, are Swiss Pension Fund Benchmarks provided by Pictet, a Swiss private bank in Geneva.
#library(fBasics)
#data(LPP2005REC)
library(timeSeries)
data1=timeSeries(data[,-1],data$Date) # or as.zoo(data[,-1],data$Date)
data.missing=data1[-1:-10,] # data 1:10 removed
return=diff(log(data1))
any(is.na(return))
return=na.omit(return)
plot(data1)
mean(return$SBI)
apply(return, 2, mean)
colMeans(return)
sd(return$SBI)
apply(return, 2, sd)
colSds(return)
min(return$SBI)
colMins(return)
colMaxs(return)
summary(return)

# read HedgeFund use first column for rownames
Mean = round(apply(HedgeFund, 1, mean), 2)
Sdev = round(apply(HedgeFund, 1, sd), 2)
Min = apply(HedgeFund, 1, min)
Max = apply(HedgeFund, 1, max)
Statistics <- data.frame(cbind(Mean, Sdev, Min, Max))
Statistics

##Subsetting Assets
# Extract a specific date:
data1["2007-04-24", ]
#Subset all records from the first and second quarter:
round(window(data1, start = "2006-01-15", end = "2006-01-21"), 1)


#Rolling Assets
by <- periods(time(return), period="12m", by="1m") #Returns start and end dates for a rolling periods
(ans <- applySeries(100*return, from = by$from, to = by$to, FUN = "colSums"))
# example for periods: by <- periods(time(SWX), period = "52w", by = "4w")
#by <- periods(time(SWX), period = "360d", by = "30d")

#Distribution tests
x <- runif(100)
ks.test(x, "pnorm")# Kolmogorov-Smirnov goodness of fit test
x <- rnorm(100,5,1)
ks.test(x, "pnorm")
ks.test((x-mean(x))/sd(x), "pnorm")
x1 = rnorm(100)
x2 = rnorm(100)
ks.test(x1, x2) #test of the null hypothesis that x and y were drawn from the same continuous distribution

x1 = rnorm(100)
x2 = runif(100)
ks.test(x1, x2)
library(tseries)
skewness(x1)
kurtosis(x1)
jarque.bera.test(x1)
skewness(return$SBI)
kurtosis(return$SBI)
jarque.bera.test(return$SBI)

library(MASS)
library(sn)
(fit.t <- fitdistr(return$SPI, "t"))
m=fit.t$estimate[1]
s=fit.t$estimate[2]
df=fit.t$estimate[3]
ks.test(return$SBI,rst(10000,xi=m,omega=s,nu=df))
mean(return$SBI)

library(fPortfolio)
?assetsTest
shapiro=assetsTest(return)
print(shapiro)

# Plots in R
X=seq(0,150,10)
y=(15+1/x)^(1/x)
plot(x,y)

# Discrete & Qualitative Data
v=c(12,10,7,13,26,16,4,12)
pie(v)
pie(v, clockwise=T)
LETTERS[1:8]
names(v) <- LETTERS[1:8]
barplot(v)
par(mfrow=c(1, 2)) #matrix-frame-by-row
pie(v); barplot(v)
par(mfrow=c(1, 1))
barplot(v, col=1:8)
dotchart(v)
par(bg="lightgrey")# The color to be used for the background
dotchart(v, pch=10, col=1:8)# pch=ploting character or symbol to be used. check pch="a"
par(bg="white")
par(bg=2)
colors()
symbols=sample(c(rep("AMEX", times = 20), rep("NASDAQ",times = 80), rep("NYSE", times = 20)))
barplot(symbols)# error
factorData <- factor(symbols)
plot(factorData, col = "steelblue") # plot(xf)
plot(CPS1988$ethnicity)

# timeseries plot
data=SWX
class(data$Date)
#data$Date<-as.Date(data$Date,format="%Y-%m-%d") # not necessary
head(data)
class(data)
library(xts)
data1<-xts(data[,-1],order.by=data$Date) #The index column will be the date column that will be used as an index
class(data1)
plot(data1$SBI) # compare with plot(data$SBI)
data.missing<-data1[-1000:-1500,]# na is replaced
plot(data.missing$SBI)
summary(data1)
data1[c(1,nrow(data1)),]
data1.last30<-data1[((nrow(data1)-29)):nrow(data1),]
data1.last30

#Subsetting Using Dates
class(data1)
xts.2005<-subset(data1[,1],index(data1) >= "2005-01-01" & index(data1)<= "2005-12-31")
xts.2005[c(1:3,nrow(xts.2005))]

#for dataframe object
data2005=subset(data,data$Date>="2005-01-01"& data$Date<="2005-12-31")
rownames(data2005)<-seq(1,nrow(data2005),1)



#Quantitative Data
Returns <- rnorm(260)
boxplot(Returns)
locator(1)
hist(Returns)
hist(Returns, freq=F, breaks = 20)# probability densities
hist(Returns, probability = TRUE)
stripchart(Returns)
return=rnorm(length(symbols))
plot(factorData, rnorm(length(factorData)), col = "orange")
# import CPS1988
plot(CPS1988$ethnicity,CPS1988$education)




SPI.RET = diff(log(data1[, 2]))
hist(SPI.RET, col = 2, main = "Histogram Plot")

# 2 dimentional (2D) Graphics
#scatter plot
Index <- 1:260

plot(x = Index, y = Returns)
plot(x = Index, y = Returns, main = "Artificial Returns")


x <- seq(-10, 10, by=0.5)
plot(x, sin(x))
plot(x, sin(x), type="l",col=2) # type=p for points, l for lines, b for both,
?plot
abline(v=0, col="blue", lwd=5,lty=2)# for drawing (single) straight lines , lwd=line width, lty=line type
abline(h=sin(0.7), col=3)
abline(a=0,b=0.2)
text(-5, -0.5, "Hello", font=3)
par(mfrow=c(1, 2))
plot(x, sin(x), type="l", col=1,main="Sinus")
plot(x, cos(x), type="b", col=3, xlab="X-axis")
par(mfrow=c(1, 1))
plot(x, cos(x), type="b")
points(0, 1, pch="o",col="blue")
lines(c(-5, 5),c(0, 0.5),lty=2,col=2)
locator(1)
text(locator(2), c("tic","tac"),font=c(2, 3))

par(mfrow = c(2, 1))
Price = cumsum(Returns)
plot(Index, Price, type = "l", main = "Line Plot")
plot(Index, Returns, type = "h", main = "Histogram-like Vertical Lines") #h= histogram like



A <- cbind(1:20, rnorm(20), runif(20))
par(mfrow = c(1, 1))
matplot(A, type="b")

#plot.formula and lines.formula
y=2+x+rnorm(41)
y1=2+x+2*rnorm(41)
plot(y~x)
plot(x,y,type="l")
lines(x,y1)
lines(y1~x,col=2)
 
#Time series plot
A=matrix(rnorm(300),100,3)
colnames(A)=c("x1","x2","x3")
A=ts(A,start=c(1952,2),frequency = 4)
A
class(A)
plot(A)
plot(A[,c("x1","x2")])
A=ts(A,start=c(1950,3),frequency = 12)
A
plot(A)

library(xts)
data=rnorm(10)
as.Date(1:10,origin="1970-01-01")
xts.ts <- xts(data,order.by = as.Date(1:10,origin="1970-01-01"))
xts.ts
xts.ts["1970-01-04/1970-01-08"]


t=SWX[,1]
t1=as.Date(t$Date,format="%d-%b-%y")
data1=xts(SWX[,-1],t1)
plot(data1)
data=SWX[,2:5] #or SWX[,-1]
# or time=rownames(SWX); rownames(SWX)=NULL or c()
# or subset(SWX, select = -Date)

library(xts)
data=SWX 
data1<-xts(data[,-1],order.by=data$Date) #The index column will be the date column that will be used as an index
return=diff(log(data1))
return=na.omit(return)
# or return1 <- log(tail(data, -1) / head(data, -1)) or
# retunn3=fPortfolio::returns(data)
#return4=log(data/lag(data))
acf(return$SBI)
acf(as.numeric(return$SBI))
pacf(return$SBI)
pacf(as.numeric(return$SBI))
ret=rnorm(100,1,4)
qqnorm(ret)
qqline(ret)
abline(a=0,b=1,col=2)
qqnorm(return$SBI, pch = 19)# pch for plotting character see ?points
qqline(return$SBI)

y <- rt(200, df = 2)
qqnorm(y); qqline(y, col = 2) # concave- convex
qqplot(rt(300, df = 2),y)
qqline(y, distribution = function(p) qt(p, df = 2),prob = c(0.25, 0.75),col = 2) #produces a QQ plot of two datasets
qqline(y, distribution = function(p) qt(p, df = 2),prob = c(0.3, 0.7),col = 2)
abline(a=0, b=1, col=1)
qqplot(rnorm(300),y)
qqline(y, col = 2)

qqplot(rt(1000,df=df),as.numeric((return$SPI)-m)/s) 
#qqline((return$SBI-m)/s,col=2) wrong!
qqline((as.numeric(return$SPI) -m)/s,distribution = function(p) qt(p, df = df),prob = c(0.1, 0.6), col=2)
abline(a=0, b=1, col=2)

qqplot(rst(10000,xi=m,omega=s,nu=df),as.numeric(return$SPI)) 
qqline(as.numeric(return$SPI) ,distribution = function(p) qst(p,xi=m,omega=s,nu=df),prob = c(0.1, 0.6))
abline(a=0, b=1, col=2)
#or
qqplot(rt(1000,df=df), as.numeric(return$SPI))
abline(lm(quantile(as.numeric(return$SPI), c(0.25, 0.75))~qt(c(0.25, 0.75), df = df) ))
#or
n=dim(return)[1]
q_grid = (1:n) / (n + 1)
qqplot(qt(q_grid,df), as.numeric(return$SPI))
abline(lm(quantile(as.numeric(return$SPI), c(0.25, 0.75))~qt(c(0.25, 0.75), df = df) ))


plot(density(return$SBI))

## e)) Inspect the fitted distributions by means of Quantile-Quantile Plots
library(MASS)
library(sn)


(fit.norm <- fitdistr(return$SPI, "normal"))
mu.norm=fit.norm$estimate["mean"]
sd.norm=fit.norm$estimate["sd"]
q.n <- rnorm(2000, mean=mu.norm, sd=sd.norm)
# or grid <- seq_along(sp500.ret) / (length(sp500.ret) + 1) # empirical cdf
# q.n <- qnorm(grid, mean=mu.norm, sd=sd.norm) # theoretical quantiles (N)
layout(t(1:2)) # or par(mfrow=c(1, 2))
qqplot(q.n,as.numeric(return$SPI) , xlab="Normal distr.", ylab="Empirical quantiles")
abline(a=0, b=1, col=2)

fit.t <- fitdistr(return$SPI, "t")
mu.t=fit.t$estimate["m"]
lambda=fit.t$estimate["s"]
nu=fit.t$estimate["df"]
q.t <- rst(2000, xi=mu.t, omega=lambda,nu=nu)
qqplot(q.t,as.numeric(return$SPI) , xlab="t location-scale distr", ylab="Empirical quantiles")
abline(a=0, b=1, col=2)



library(fPortfolio)
densityPlot(timeSeries(return$SPI))
densityPlot(timeSeries(return$SPI),fit = F)
qqnormPlot(return$SPI) #returns a normal quantile-quantile plot
qqnigPlot(return$SPI)# returns a normal inverse Gaussian (NIG) quantile-quantile plot
qqghtPlot(return$SPI) #returns a generalized hyperbolic Student' t distribution (sGHT) quantile-quantile plot
#see ghyp: A package on generalized hyperbolic distributions
x <- rnorm(250)
qqnormPlot(x)
x=rnig(500, alpha = 2, beta = 1, delta = 1, mu = 0)
qqnigPlot(x)
x <- rnorm(250)
x=rght(1000, beta = 1, delta = 1, mu = 0, nu = 10)
qqghtPlot(x)
x <- rnorm(250)
qqghtPlot(x)

#In this subsection the daily returns of Hewlett Packard (HWP) stock
#are fitted to the GHD and its special cases, the HYP and NIG
library( ghyp )
library(timeSeries)
library(fBasics)
# Fitting HPW returns to the GHD
## Return calculation
data(DowJones30)
y<-timeSeries(DowJones30[,"HWP"],charvec =
                      as.character(DowJones30[,1]))
yret<-na.omit(diff(log(y))*100) 

#Fitting
ef<-density(yret)
ghdfit<-fit.ghypuv(yret,symmetric = FALSE,
                        control=list(maxit=1000))
hypfit<-fit.hypuv(yret,symmetric=FALSE,
                    control=list(maxit=1000))
nigfit<-fit.NIGuv(yret,symmetric=FALSE,
                      control=list(maxit=1000))

#Densities
ghddens<-dghyp(ef$x,ghdfit)
hypdens<-dghyp(ef$x,hypfit)
nigdens<-dghyp(ef$x,nigfit)
nordens<-dnorm(ef$x,mean=mean(yret),sd=
             sd(c(yret[,1])))
col.def<-c("black","red","blue","green","orange")
plot(ef,xlab="",ylab=expression(f(x)),ylim=c(0,0.25))
lines(ef$x,ghddens,col="red")
lines(ef$x,hypdens,col="blue")
lines(ef$x,nigdens,col="green")
lines(ef$x,nordens,col="orange")
legend("topleft",
    legend=c("empirical","GHD","HYP", "NIG","NORM"),
              col=col.def,lty=1)

plot(ef,xlab="",ylab=expression(f(x)),ylim=c(0,0.05),xlim=c(-15,-5))
lines(ef$x,ghddens,col="red",ylim=c(0,0.05),xlim=c(-15,-5))
lines(ef$x,hypdens,col="blue",ylim=c(0,0.05),xlim=c(-15,-5))
lines(ef$x,nigdens,col="green",ylim=c(0,0.05),xlim=c(-15,-5))
lines(ef$x,nordens,col="orange",ylim=c(0,0.05),xlim=c(-15,-5))
legend("topleft",
       legend=c("empirical","GHD","HYP", "NIG","NORM"),
       col=col.def,lty=1)

# QQ???Plots
qqghyp(ghdfit,line=TRUE,ghyp.col="red",
         plot.legend=FALSE,gaussian=FALSE,
         main="",cex=0.8)
qqghyp(hypfit,add=TRUE,ghyp.pch=2,ghyp.col="blue",
         gaussian=FALSE,line=FALSE,cex=0.8)
qqghyp(nigfit,add=TRUE,ghyp.pch=3,ghyp.col="green",
         gaussian=FALSE,line=FALSE,cex=0.8)
legend("topleft",legend=c("GHD","HYP","NIG"),
              col=col.def[-c(1,5)],pch=1:3)


# Diagnostics 42
AIC<-stepAIC.ghyp(yret,dist=c("ghyp","hyp","NIG"),
                        symmetric=FALSE ,
                control=list(maxit=1000))
AIC$fit.table
LRghdnig<-lik.ratio.test(ghdfit,nigfit)
LRghdnig$p.value
LRghdhyp<-lik.ratio.test(ghdfit,hypfit)
LRghdhyp$p.value




