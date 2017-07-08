setwd("C:/Directory")
energy.data=read.table('', header=T)
plot.ts(energy.data, type='b')

##build time series model####
y<-ts((energy.data))
t<-time(y)
L<-4   
m<-10
T=L*m
Q<-factor(rep(c(2:L,1),m))
energy.model<-lm(y~t+I(t^2)+Q)
summary(energy.model)

#check prediction and its 95% CIs#
y.pred<-predict(energy.model, data.frame(t=41, Q=as.factor(2)), interval='p')
y.pred<-predict(energy.model, data.frame(t=42, Q=as.factor(3)), interval='p')
y.pred<-predict(energy.model, data.frame(t=43, Q=as.factor(4)), interval='p')
y.pred<-predict(energy.model, data.frame(t=44, Q=as.factor(1)), interval='p')

##Durbin Watson Test for autocorrelation#
library(car)
dwt(energy.model, max.lag=1, alternative="t")
dwt(energy.model, max.lag=1, alternative="p")

###ARIMA model for the error term##
error.model<-ar(resid(energy.model),aic=FALSE,demean=FALSE,order.max=1,method="yw")
phi<-as.numeric(error.model$ar)

####Another testing####
energy.data<-read.table('t6-9 energy bills.txt', header=T)
y<-ts((energy.data))
t<-time(y)
L<-4   
m<-10
T=L*m
Q<-factor(rep(c(2:L,1),m))
energy.model<-lm(y~t+I(t^2)+Q)
error.model <- ar(resid(energy.model), aic=FALSE, demean=FALSE, order.max=1, method='yw')
phi<- as.numeric(error.model$ar)
ynew<-y[2:T]-phi*y[1:(T-1)]
X<-model.matrix(energy.model)
Xdiff<-X[-1,-1]-phi*X[-T,-1]
Xnew<-cbind(rep(1,T-1), Xdiff)
beta_hat1<-solve(t(Xnew)%*%Xnew)%*%t(Xnew)%*%ynew
energy.model2<-lm(ynew~Xnew-1)
beta_hat1<-coef(energy.model2)
beta_hat<-c(beta_hat1[1]/(1-phi), beta_hat1[-1])
summary(energy.model2)

s<-summary(energy.model2)$sigma
fitted.y<-(Xnew)%*%beta_hat1
s<- sqrt(sum((fitted.y-ynew)^2)/((T-1)-length(beta_hat)))
result<-list(beta_hat=beta_hat,phi=phi)


###Comparison with the results without autoregressive model on error term###

ynew<-y[2:T]-phi*y[1:(T-1)]
X<-model.matrix(energy.model)
Xdiff<-X[-1,-1]-phi*X[-T,-1]
Xnew<-cbind(rep(1,T-1), Xdiff)
beta_hat1<-solve(t(Xnew)%*%Xnew)%*%t(Xnew)%*%ynew
energy.model2<-lm(ynew~Xnew-1)
beta_hat1<-coef(energy.model2)
beta_hat<-c(beta_hat1[1]/(1-phi), beta_hat1[-1])
s<-summary(energy.model2)$sigma
fitted.y<-(Xnew)%*%beta_hat1
s<- sqrt(sum((fitted.y-ynew)^2)/((T-1)-length(beta_hat)))
alpha<-0.05
z<-(-qnorm(alpha/2))
s*z


#############New data set###########
##calculate moving average###
oli.data<-read.table('', header=T)
oli.data<-ts(oli.data)
window.length<-4
library(TTR)

moving.average<-SMA(oli.data,n=window.length)
cma<-SMA(moving.average,n=2)
cma<-c(cma[-(1:window.length/2)],rep(NA,window.length/2))

sn.txir.t<-oli.data/cma
sn.txir.t

n.year<-length(oli.data)/window.length
###calculate estimated seasonal factor sn###

sn<-numeric(window.length)

for (i in (window.length/2+1):window.length)
{
  for (j in  seq(from=i, to=length(oli.data)-window.length/2, by=window.length)){
    sn[i]<-sn[i]+sn.txir.t[j,]
  }
  sn[i]<-sn[i]/(n.year-1)
}

for (i in 1:(window.length/2))
{
  for (j in seq(from=i+window.length, to=length(oli.data)-window.length/2, by=window.length)){
    sn[i]<-sn[i]+sn.txir.t[j,]
  }
  sn[i]<-sn[i]/(n.year-1)
}

sn<-sn*window.length/sum(sn)
dt<-oli.data/sn
plot.ts(dt,type="b")



t<-time(ts(dt))
trend.model<-lm(dt~t)
tr<-fitted(trend.model)
clxir<-oli.data/(tr*sn)
cl<-SMA(clxir,n=3)
cl<-c(cl[-1],rep(NA,1))

##trend model => trt=intercept+coef*t##
####yt=trt+snt###
###so in order to predict future values, use exisiting data and insert the values in the prediction formula###
