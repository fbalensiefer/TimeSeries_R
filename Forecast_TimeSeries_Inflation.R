#####################################################################
###   Time Series Forecasting 
#####################################################################

### Preface
# clearing workspace and set wd
#install.packages("tseries")
#install.packages("forecast")
#install.packages("urca")
#install.packages("rugarch")


rm(list=ls())
cat("\014")
dev.off()
#setwd("~/Google Drive/UniBonn/X_Time_Series/TimeSeries_R")
setwd("C:/Users/fabia/Google Drive/UniBonn/X_Time_Series/TimeSeries_R")
library(forecast)
library(tseries)
library(urca)
library(rugarch)

#####################################################################
###   Exmaple: CPI from US-FRED quarterly and seasonal adjusted
###            "CPIAUCSL"

cpi=read.csv("cpi.csv")
y = cpi[,2]
y = ts(y,start=c(1947,1),freq=4)     # defining as TS
z = 400*diff(log(y))                 # US-cpi inflation (400= 100%*4Q) yearly
z=z[,2]
RESULT=rep(NA,13)

#####################################################################
### Analysis on complete sample including the structural break (1984)

# AR(1) 
model= auto.arima(z, max.p = 1, max.q = 0 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[1]=forec$mean

# AR(2)
model= auto.arima(z, max.p = 2, max.q = 0 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[2]=forec$mean

# Auto ARIMA(p,q)
model = auto.arima(z, max.p = 5, max.q = 5 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[3]=forec$mean

# ARFIMA - Fractional Integration
model= arfima(z)
forec=forecast(model,h=1)
RESULT[4]=forec$mean

### Split the sample since we observe a structural break in the data
#   Idea: before 1984Q1 high Variance, after 1984Q1 less Variance
#   Note: we still estimate with OLS, hence assume homoskecasticity

z1 = window(z,c(1947,2),c(1983,4))
z2 = window(z,c(1984,1),c(2019,1))

#####################################################################
### Analysis on subset z1

# AR(1) 
model= auto.arima(z1, max.p = 1, max.q = 0 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[5]=forec$mean

# AR(2)
model= auto.arima(z1, max.p = 2, max.q = 0 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[6]=forec$mean

# Auto ARIMA(p,q)
model = auto.arima(z1, max.p = 5, max.q = 5 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[7]=forec$mean

# ARFIMA - Fractional Integration
model= arfima(z1)
forec=forecast(model,h=1)
RESULT[8]=forec$mean

#####################################################################
### Analysis on subset z2

# AR(1) 
model= auto.arima(z2, max.p = 1, max.q = 0 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[9]=forec$mean

# AR(2)
model= auto.arima(z2, max.p = 2, max.q = 0 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[10]=forec$mean

# Auto ARIMA(p,q)
model = auto.arima(z2, max.p = 5, max.q = 5 ,ic="bic", seasonal = F, stationary = F) 
forec=forecast(model,h=1)
RESULT[11]=forec$mean

# ARFIMA - Fractional Integration
model= arfima(z2)
forec=forecast(model,h=1)
RESULT[12]=forec$mean

# GRACH(1,1)
model = garch(z2 ,c(1,1))
forec=forecast(model,h=1)
RESULT[13]=forec$mean

#####################################################################
### Calculation of the average Forecast

forecast_inflation=mean(RESULT)
paste("The current Inflation in 2019 Q1 is:", z[288])
paste("The Inflation Forecast for 2019 Q2 is:", forecast_inflation)