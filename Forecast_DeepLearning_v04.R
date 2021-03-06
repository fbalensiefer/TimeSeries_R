#####################################################################
###   Time Series Forecasting using LSTM and Keras
#####################################################################

### Preface
# clearing workspace and set wd
#install.packages("keras")
#install.packages("tensorflow")

rm(list=ls())
cat("\014")
dev.off()
#setwd("~/Google Drive/UniBonn/X_Time_Series/TimeSeries_R")
setwd("C:/Users/fabia/Google Drive/UniBonn/X_Time_Series/TimeSeries_R")
library(keras)
library(tensorflow)

#####################################################################
###   Exmaple: CPI from US-FRED quarterly and seasonal adjusted
###            "CPIAUCSL"

cpi=read.csv("cpi.csv")
y = cpi[,2]
y = ts(y,start=c(1947,1),freq=4)     # defining as TS
z = 400*diff(log(y))                 # US-cpi inflation (400= 100%*4Q) yearly
z2 = window(z,c(1984,1),c(2019,1))
iter.prediction<-numeric(20)


for (j in 1:20){
  #####################################################################
  ### using Keras to apply Time Series Forecasting
  ### with Long short-term memory (LSTM)
  
  ### Let's concentrade on the second peroid,
  #   since there is clearly a structural break before!
  
  # transform data by centering (first differencing)
  diffed=z2   # z2 already seems to be stationary --> AVOID overdifferencing
  #diffed=diff(z2, differences = 1)
  #t.test(diffed, mu=0) # test whether data has become stationary
  # creating a lagged dataset for training
  lag_transform <- function(x, k= 1){
    
    lagged =  c(rep(NA, k), x[1:(length(x)-k)])
    DF = as.data.frame(cbind(lagged, x))
    colnames(DF) <- c( paste0('x-', k), 'x')
    DF[is.na(DF)] <- 0
    return(DF)
  }
  supervised = lag_transform(diffed, 1)
  # split sample into two subsets (training and test data)
  N = nrow(supervised)
  n = round(N *0.8, digits = 0)
  train = supervised[1:n, ]
  test  = supervised[(n+1):N,  ]
  # scale data
  scale_data = function(train, test, feature_range = c(0, 1)) {
    x = train
    fr_min = feature_range[1]
    fr_max = feature_range[2]
    std_train = ((x - min(x) ) / (max(x) - min(x)  ))
    std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
    scaled_train = std_train *(fr_max -fr_min) + fr_min
    scaled_test = std_test *(fr_max -fr_min) + fr_min
    return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  }
  Scaled = scale_data(train, test, c(-1, 1))
  y_train = Scaled$scaled_train[, 2]
  x_train = Scaled$scaled_train[, 1]
  y_test = Scaled$scaled_test[, 2]
  x_test = Scaled$scaled_test[, 1]
  # inverse-transform
  invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
    min = scaler[1]
    max = scaler[2]
    t = length(scaled)
    mins = feature_range[1]
    maxs = feature_range[2]
    inverted_dfs = numeric(t)
    for( i in 1:t){
      X = (scaled[i]- mins)/(maxs - mins)
      rawValues = X *(max - min) + min
      inverted_dfs[i] <- rawValues
    }
    return(inverted_dfs)
  }
  # Reshape the input to 3-dim (samples, timesteps, features)
  dim(x_train) <- c(length(x_train), 1, 1)
  # specify required arguments
  X_shape2 = dim(x_train)[2]
  X_shape3 = dim(x_train)[3]
  batch_size = 1
  units = 1
  #=========================================================================================
  # modeling
  model <- keras_model_sequential()
  model%>%
    layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
    layer_dense(units = 1)
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam( lr= 0.002, decay = 1e-6 ),
    metrics = c('accuracy')
  )
  summary(model)
  #=========================================================================================
  # fit the model
  Epochs = 50
  for(i in 1:Epochs ){
    model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
    model %>% reset_states()
  }
  #=========================================================================================
  # predicting
  L = length(x_test)
  scaler = Scaled$scaler
  predictions = numeric(L)
  
  for(i in 1:L){
    X = x_test[i]
    dim(X) = c(1,1,1)
    yhat = model %>% predict(X, batch_size=batch_size)
    # invert scaling
    yhat = invert_scaling(yhat, scaler,  c(-1, 1))
    # invert differencing
    yhat  = yhat + z2[(n+i)]
    # store
    predictions[i] <- yhat
  }
  iter.prediction[j]<-predictions[1]
}

avg.prediction<-mean(iter.prediction)

#print(predictions)
#=========================================================================================
# Forecast is:
T = length(z2)
paste("The current Inflation in 2019 Q1 is:", z2[T])
paste("The Inflation Forecast for 2019 Q2 is:", avg.prediction)

#plotpred=c(rep(NA, length(z2)),predictions)
#plottot=c(z2, predictions)
#ts.plot(plottot)
#lines(plotpred, col="red")
