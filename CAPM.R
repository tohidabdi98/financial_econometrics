data=CAPM_Example
n = dim(data)[1]

Excess_R_SP500 =data$SP500.Close[2:n]/data$SP500.Close[1:(n-1)] - 1  -data$Tbill.Close[2:n]/(100*365)
# or Excess_R_SP500 =diff(log(data$SP500.Close)) -data$Tbill.Close[2:n]/(100*365) 
Excess_R_MSFT =data$MSFT.Close[2:n]/data$MSFT.Close[1:(n-1)] - 1  - data$Tbill.Close[2:n]/(100*365)
# or Excess_R_MSFT = diff(log(data$MSFT.Close)) - data$Tbill.Close[2:n]/(100*365)

#Simple beta estimation : Security Characteristic Line (SCL):
cov(Excess_R_MSFT,Excess_R_SP500 ) / var(Excess_R_SP500)

#Beta estimation from linear regression
fit = lm(Excess_R_MSFT~Excess_R_SP500)

summary(fit)
#hypothesis testing
library(car)
linearHypothesis(fit,"Excess_R_SP500=1")


fit_withoutInt = lm(Excess_R_MSFT~Excess_R_SP500-1) # "1" means intercept in lm

summary(fit_withoutInt)
plot(Excess_R_SP500, Excess_R_MSFT)
abline(fit, col = 'red')
locator(1)
par(mfrow = c(2, 2))
plot(fit)
locator(1)

#with ts
data1=CAPM_FORD
data1=ts(data1[,-1],start = c(2002,1),frequency = 12)
plot(data1)
window(data1,start=c(2002,1),end=c(2002,5))
n= dim(data1)[1]
Excess_R_SP5001 =diff(log(data1[,"SANDP"])) -data1[,"USTB3M"][2:n]/100 
Excess_R_FORD1 = diff(log(data1[,"FORD"])) - data1[,"USTB3M"][2:n]/100 

#Beta estimation from linear regression
fit1 = lm(Excess_R_FORD1~Excess_R_SP5001)

summary(fit1)

library(quantreg)
fit_rq=rq(Excess_R_FORD~Excess_R_SP500,tau =c(0.1,0.5,0.7,0.8)) #seq(0.1,0.9,by=0.1))
summary(fit_rq)
plot(fit_rq)
# test whether the effects of the regressors are uniform across quantiles
fit_rq20=rq(Excess_R_FORD~Excess_R_SP500,tau = 0.1)
fit_rq80=rq(Excess_R_FORD~Excess_R_SP500,tau = 0.3)
anova(fit_rq80,fit_rq20)



#data = read.csv("CAPM_Example.csv",header=T)



# attach(data)


data=CAPM_Example
n = dim(data)[1]

Excess_R_SP500 =data$SP500.Close[2:n]/data$SP500.Close[1:(n-1)] - 1  -data$Tbill.Close[2:n]/(100*365) 
# or Excess_R_SP500 =diff(log(data$SP500.Close)) -data$Tbill.Close[2:n]/(100*365) 
Excess_R_MSFT =data$MSFT.Close[2:n]/data$MSFT.Close[1:(n-1)] - 1  - data$Tbill.Close[2:n]/(100*365) 
# or Excess_R_MSFT = diff(log(data$MSFT.Close))  - data$Tbill.Close[2:n]/(100*365)

#Simple beta estimation : Security Characteristic Line (SCL):
cov(Excess_R_MSFT,Excess_R_SP500 ) / var(Excess_R_SP500)

#Beta estimation from linear regression
fit = lm(Excess_R_MSFT~Excess_R_SP500)

summary(fit)

#hypothesis testing
library(car)

linearHypothesis(fit,"Excess_R_SP500=1")


fit_withoutInt = lm(Excess_R_MSFT~Excess_R_SP500-1) # "1" means intercept in lm

summary(fit_withoutInt)
par(mfrow = c(1, 1))
plot(Excess_R_SP500, Excess_R_MSFT)
abline(fit, col = 'red')
locator(1)
par(mfrow = c(2, 2))
plot(fit)
locator(1)
library(quantreg)
fit_rq=rq(Excess_R_MSFT~Excess_R_SP500,tau = seq(0.05,0.8,by=0.05))
summary(fit_rq)
plot(fit_rq)
# test whether the effects of the regressors are uniform across quantiles
fit_rq20=rq(Excess_R_MSFT~Excess_R_SP500,tau = 0.2)
fit_rq80=rq(Excess_R_MSFT~Excess_R_SP500,tau = 0.8)
anova(fit_rq80,fit_rq20)

#with ts format
data1=CAPM_FORD
data1=ts(data1[,-1],start=c(2002,1),frequency = 12)
plot(data1)
window(data1,start=c(2002,1),end=c(2002,5))
n = dim(data1)[1]
Excess_R_SP5001 =diff(log(data1[,"SANDP"])) -data1[,"USTB3M"][2:n]/100 
Excess_R_FORD1 = diff(log(data1[,"FORD"])) - data1[,"USTB3M"][2:n]/100
fit1= lm(Excess_R_FORD1~Excess_R_SP5001)
summary(fit1)
par(mfrow = c(2, 2))
plot(fit1)

# Modeling the SML (wrong!)
# data from folder CAPM:data_capm1, first row  for column names
#library(fBasics)
data("DowJones30")
data=apply(data_capm1[,-1],2,as.numeric)
# data=data.frame(data) not necassary
rf=data[,89] # risk free rate (rft)
riskprem <- function(x) diff(log(x))-tail(rf,-1)
returns <- apply(data[,1:88], 2, riskprem)
# returns=as.data.frame(returns) not necassary
beta_mean <-function(x)
  c(beta = lm(x ~ returns[,88])$coefficients[2],mean = mean(x,na.rm = T))
results=apply(returns[,1:87],2,beta_mean)
results=t(results)
stock_names=colnames(returns)# not necessary
rownames(results)=stock_names[1:87]# not necessary
colnames(results)=c("beta","mean") # not necessary
results=as.data.frame(results)
par(mfrow = c(1, 1))
plot(results$beta, results$mean)
abline(lm(results$mean ~ results$beta), col = 'red')
summary(lm(results$mean ~ results$beta))
mean(returns$sp500-tail(rf,-1),na.rm = T)

# the second solution (wrong!)
data=apply(data_capm1[,-1],2,as.numeric)
#data=data.frame(data) not necessary
rf=data[,89] # risk free rate (rft) 
riskprem <- function(x) diff(log(x)) - tail(rf,-1)
returns <- apply(data[,1:88], 2, riskprem)
# returns=as.data.frame(returns) not necassary
stock_names=colnames(returns)
# colnames(returns)=stock_names # or change  rf=rf$rf1 in riskprem above
results2 <- t(sapply(stock_names[1:87], function(name)
  c(beta = lm(returns[, name] ~ returns[,88])$coefficients[2],
    mean = mean(returns[, name], na.rm = T))))

results2 <- as.data.frame(results2)
plot(results2$beta, results2$mean)
abline(lm(results2$mean ~ results2$beta), col = 'red')


# Modeling the SML ( it is true)
# data from folder CAPM:data_capm1, first row  for column names
data=apply(data_capm1[,-1],2,as.numeric)
# data=data.frame(data) not necassary
rf=data[,89] # risk free rate (rft)
#riskprem <- function(x) diff(log(x))-tail(rf,-1)
rf=tail(rf,-1)
returns=diff(log(data[,1:88]))
#returns <- apply(data[,1:88], 2, riskprem)
# returns=as.data.frame(returns) not necassary
beta_mean <-function(x)
  c(beta = lm(x-rf ~ returns[,88]-rf)$coefficients[2],mean = mean(x,na.rm = T))
results=apply(returns[,1:87],2,beta_mean)
results=t(results)
stock_names=colnames(returns)# not necessary
rownames(results)=stock_names[1:87]# not necessary
colnames(results)=c("beta","mean") # not necessary
results=as.data.frame(results)
par(mfrow = c(1, 1))
plot(results$beta, results$mean)
abline(lm(results$mean ~ results$beta), col = 'red')
summary(lm(results$mean ~ results$beta))
mean(returns[,88]-rf,na.rm = T)



#Testing the explanatory power of the individual variance
beta_mean_risk=function(name){
  beta <- lm(returns[,name]-rf ~ returns[,88]-rf)$coefficients[2]
  c(beta = beta, mean = mean(returns[,name], na.rm = TRUE),
    risk = var(returns[,name], na.rm = TRUE) - beta^2 * var(returns[,88], na.rm = T))}
results3 <- t(sapply(stock_names[1:87],beta_mean_risk))
results3=as.data.frame(results3)
colnames(results3)=c("beta","mean","risk")
summary(lm(results3$mean ~ results3$beta + results3$risk))



# Fama MacBeth two stage procedure
# Read data and create joint data set
# monthlyfactors <- read_excel ("D:/ Programming Guide / data / monthlyfactors . xlsx ")
# vw_sizebm_ 25 groups <- read_excel ("D:/ Programming Guide / data / vw_sizebm_ 25groups . xlsx ")
vw_sizebm_25groups[,-1] = vw_sizebm_25groups[,-1] - monthlyfactors$rf
data = data.frame (vw_sizebm_25groups, monthlyfactors[,2:7])
data = data [1:363 ,] # Adjust data set to Gregory et al. period

# First stage regressions
betas = NULL
for (var in 2:26) {
   lr = lm( data [,var ] ~ rmrf + smb + hml + umd , data = data )
   betas = rbind (betas , lr$coefficients [2:5])
    }

# Second stage regressions
lambdas = Rsq = NULL
 for (t in 1: nrow ( data )) {
   lr = lm(t( data [t,2:26]) ~ betas )
   lambdas = rbind ( lambdas , lr$coefficients )
   Rsq = c(Rsq , summary (lr)$r.squared )
   }

# Compute market prices of risk and t- statistics
colMeans ( lambdas )*100
nrow ( data )^0.5* colMeans ( lambdas )/ apply ( lambdas , 2, sd)
mean (Rsq )
