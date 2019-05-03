#####################################################################
###   Time Series in R
#####################################################################

### Preface
# clearing workspace and set wd
#install.packages("dynlm")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("vars")
#install.packages("urca")

rm(list=ls())
cat("\014")  
setwd("~/Google Drive/UniBonn/X_Time_Series/TimeSeries_R")
#setwd("C:/Users/fabia/Google Drive/UniBonn/X_Time_Series/TimeSeries_R")
library(dynlm)
library(forecast)
library(tseries)

#####################################################################
###   Exmaple: CPI from US-FRED quarterly and seasonal adjusted
###            "CPIAUCSL"

cpi=read.csv("cpi.csv")
y = cpi[,2]
y = ts(y,start=c(1947,1),freq=4)     # defining as TS
par(mfrow=c(2,2))
z = 400*diff(log(y))  
z2 = window(z,c(1984,1),c(2019,1))
# 2x3 format
par(mfrow=c(2,3))
ts.plot(z)
plot(Acf(z,lag.max=40,plot=F))
plot(Pacf(z,lag.max=40,plot=F))
ts.plot(z2)
plot(Acf(z2,lag.max=40,plot=F))
plot(Pacf(z2,lag.max=40,plot=F))
# 3x2 format
par(mfrow=c(3,2))
ts.plot(z)
ts.plot(z2)
plot(Acf(z,lag.max=40,plot=F))
plot(Acf(z2,lag.max=40,plot=F))
plot(Pacf(z,lag.max=40,plot=F))
plot(Pacf(z2,lag.max=40,plot=F))

# test: non-zero autocorrelation at lags 4,8,12 (Ljung-Box)
Box.test(z2,lag = 4, type = 'Ljung-Box')   # p-value is 0.0005 hence we reject
Box.test(z2,lag = 8, type = 'Ljung-Box') 
Box.test(z2,lag = 12, type = 'Ljung-Box') 


### How to set/ choose AR model? 
#   Solution: by using information criteria e.g. AIC, BIC ...
#   Note: BIC is more restrictive when it comes to adding lags

# Check all AR(q)-models up to 5 lags - AIC determines optimal lag length
ar5.mod = dynlm(z ~ L(z,c(1:5)))
ar5 = summary(ar5.mod)
ar5  # here we just compute the AIC and BIC for an AR(5) by hand
aic5 = log(ar5$sigma^2) + 2*length(ar5$coef[,1])/length(z2)
bic5 = log(ar5$sigma^2) + log(length(z2))*length(ar5$coef[,1])/length(z2)
res5 = ar5$residuals
par(mfrow=c(1,2))
plot(res5)
plot(Acf(res5,lag.max=40,plot=F))
Box.test(res5,lag = 4, type = 'Ljung-Box')   
Box.test(res5,lag = 8, type = 'Ljung-Box') 
Box.test(res5,lag = 12, type = 'Ljung-Box') 

### Constructing a Forecast for US-CPI in 2019Q2
#   using a simple AR(1)

ar1.mod = dynlm(z2 ~ L(z2,c(1:5)))
ar1 = summary(ar1.mod)
T = length(z2)
forec = ar1$coef[1,1] + ar1$coef[2,1]*z2[T]
forec
# assessing the uncertainty of the point forecast by 95%-CI
forec.upp = forec + 1.96*ar1$sigma
forec.low = forec - 1.96*ar1$sigma
round(c(forec.low,forec,forec.upp),3) # hence, our forecast is very unprecise!

### Inspect residuals under normality 
jarque.bera.test(ar1$res)
qqnorm(ar1$res)
qqline(ar1$res)
# we see that the obs. in the left corner is likely driving our results
# let's create a recession dummy for 2008Q4
dum.rec = matrix(0,T,1)
dum.rec[100,1] = 1
dum.rec = ts(dum.rec,start=c(1984,1),freq=4)
dum.rec
ts.plot(dum.rec)
ar1.mod = dynlm(z2 ~ L(z2,1) + dum.rec) # now AR(1)+X model
ar1 = summary(ar1.mod)
ar1
jarque.bera.test(ar1$res)
qqnorm(ar1$res)
qqline(ar1$res) 