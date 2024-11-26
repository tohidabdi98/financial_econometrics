library(quadprog)
?solve.QP
D = 2*diag(c(1,2,4))
d = c(1,1,-5)
At = matrix(0,nrow=3,ncol=3)
At[1,] = c(-1,0,-1)
At[2,] = c(1,0,0)
At[3,] = c(0,-1,0)
b0 = c(-1,5,0)
D
At
b0
x= solve.QP(D, d, t(At), b0)$solution
x

#Swiss Pension Fund  assets returns benchmark,available from the (fBasics) package.

library(fBasics)

library(timeSeries)

data(LPP2005REC)
assetReturns <- 100 * LPP2005REC[, 1:6]
head(assetReturns)
class(assetReturns)
VCOV<-cov(assetReturns)
VCOV
#Construct the Target Portfolio Return Vector 
avg.ret<-apply(assetReturns,2,mean)
avg.ret
min.ret<-min(avg.ret)
min.ret
max.ret<-max(avg.ret)
max.ret

Dmat<-2*VCOV
dvec<-c(rep(0,length(avg.ret)))
Amat<-cbind(rep(1,length(avg.ret)),avg.ret,
            diag(1,nrow=ncol(assetReturns)))
bvec<-c(1,0.01,rep(0,ncol(assetReturns)))
soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
soln
increments=100
tgt.ret<-seq(min.ret,max.ret,length=increments)
head(tgt.ret)
tail(tgt.ret)

#Construct Dummy Portfolio Standard Deviation Vector
tgt.sd<-rep(0,length=increments)
tgt.sd

#Construct Dummy Portfolio Weights Vector
wgt<-matrix(0,nrow=increments,ncol=length(avg.ret))
head(wgt)

#Run the quadprog Optimizer

for (i in 1:increments){
  Dmat<-2*VCOV
  dvec<-c(rep(0,length(avg.ret)))
  Amat<-cbind(rep(1,length(avg.ret)),avg.ret,
               +diag(1,nrow=ncol(assetReturns)))
  bvec<-c(1,tgt.ret[i],rep(0,ncol(assetReturns)))
  soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
  tgt.sd[i]<-sqrt(soln$value)
  wgt[i,]<-soln$solution}

head(tgt.sd)
tail(tgt.sd)
head(wgt)
tail(wgt)
colnames(wgt)<-colnames(assetReturns)
head(wgt)
tail(wgt)

CHECK<-rowSums(wgt)
CHECK

#Combine Portfolio Returns, Portfolio Standard Deviations, and Portfolio Weights 
tgt.port<-data.frame(cbind(tgt.ret,tgt.sd,wgt))
head(tgt.port)
tail(tgt.port)
no.short.tgt.port<-tgt.port

#Minimum Variance Portfolio
minvar.port<-subset(tgt.port,tgt.port$tgt.sd==min(tgt.port$tgt.sd))
minvar.port

#Tangency Portfolio
riskfree=0
tgt.port$Sharpe<-(tgt.port$tgt.ret-riskfree)/tgt.port$tgt.sd
head(tgt.port)
tail(tgt.port)
tangency.port<-subset(tgt.port,tgt.port$Sharpe==max(tgt.port$Sharpe))
tangency.port

#Identify Efficient Portfolios
eff.frontier<-subset(tgt.port,tgt.port$tgt.ret>=minvar.port$tgt.ret)
eff.frontier[c(1:3,nrow(eff.frontier)),]

#Plot the MV Efficient Frontier
plot(x=tgt.sd,y=tgt.ret,xlim = c(0, 1),col="gray",xlab="Portfolio Risk",
     ylab="Portfolio Return",
     main="Mean-Variance Efficient Frontier of six Assets Based on the Quadratic Programming Approach(Not Allowing Short Selling)")
abline(h=0,lty=1)
points(x=minvar.port$tgt.sd,y=minvar.port$tgt.ret,pch=17,cex=2)
points(x=tangency.port$tgt.sd,y=tangency.port$tgt.ret,pch=19,cex=2)
points(x=eff.frontier$tgt.sd,y=eff.frontier$tgt.ret)
points(0,riskfree, cex = 2, pch = "*") # show risk-free asset
lines(c(0, 1), riskfree + c(0, 1) * (tangency.port$tgt.ret - riskfree) / tangency.port$tgt.sd,
      lwd = 4, lty = 1, col = "blue")
#Effect of Allowing Short Selling
tgt.ret<-seq(min.ret,max.ret*2,length=increments) # no bounds in short selling
head(tgt.ret)
tail(tgt.ret)
library(quadprog)

for (i in 1:length(tgt.ret)){
   Dmat<-2*VCOV
   dvec<-c(rep(0,length(avg.ret)))
   Amat<-cbind(rep(1,length(avg.ret)),avg.ret)
   bvec<-c(1,tgt.ret[i])
   soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
   tgt.sd[i]<-sqrt(soln$value)
   wgt[i,]<-soln$solution}
head(tgt.sd)
tail(tgt.sd)
head(wgt)
tail(wgt)
CHECK.wgt<-rowSums(wgt)
CHECK.wgt
tgt.port<-data.frame(cbind(tgt.ret,tgt.sd,wgt))
head(tgt.port)
tail(tgt.port)
with.short.tgt.port<-tgt.port
minvar.port<-subset(tgt.port,tgt.port$tgt.sd==min(tgt.port$tgt.sd))
minvar.port
tgt.port$Sharpe<-(tgt.port$tgt.ret-riskfree)/tgt.port$tgt.sd
head(tgt.port)
tangency.port<-subset(tgt.port,tgt.port$Sharpe==max(tgt.port$Sharpe))
tangency.port
eff.frontier<-subset(tgt.port,tgt.port$tgt.ret>=minvar.port$tgt.ret)
eff.frontier[c(1:3,nrow(eff.frontier)),]
plot(x=tgt.sd,
      y=tgt.ret,
      col="gray40",
      xlab="Portfolio Risk",
      ylab="Portfolio Return",
      main="Mean-Variance Efficient Frontier of Four Assets
      Based on the Quadratic Programming Approach
      (Allowing Short Selling)")
     
abline(h=0,lty=1)
points(x=minvar.port$tgt.sd,y=minvar.port$tgt.ret,pch=17,cex=3)
points(x=tangency.port$tgt.sd,y=tangency.port$tgt.ret,pch=19,cex=3)
points(x=eff.frontier$tgt.sd,y=eff.frontier$tgt.ret)

plot(x=no.short.tgt.port$tgt.sd,
      y=no.short.tgt.port$tgt.ret,
      xlab="Portfolio Risk",
      ylab="Portfolio Return",
      type="l",
      lwd=6,
      lty=3,
      main="MV Efficient Frontier for Four Assets
      With and Without Short Selling")
lines(x=with.short.tgt.port$tgt.sd,
         y=with.short.tgt.port$tgt.ret,
         col="gray60",
         type="l",
         lwd=2)
legend("bottomright",
          c("Not Allow Short","Allow Short"),
          col=c("black","gray60"),
          lty=c(3,1),
          lwd=c(6,2))

