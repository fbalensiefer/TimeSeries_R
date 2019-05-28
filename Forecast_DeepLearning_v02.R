#####################################################################
###   RNN Inflation Forecast with Keras and TF
#####################################################################

rm(list=ls())
cat("\014")
dev.off()
#setwd("~/Google Drive/UniBonn/X_Time_Series/TimeSeries_R")
setwd("C:/Users/fabia/Google Drive/UniBonn/X_Time_Series/TimeSeries_R")
library(keras)
library(tensorflow)

cpi=read.csv("cpi.csv")
y = cpi[,2]
y = ts(y,start=c(1947,1),freq=4)     # defining as TS
z = 400*diff(log(y))                 # US-cpi inflation (400= 100%*4Q) yearly
z2 = window(z,c(1984,1),c(2019,1))

model <- keras_model_sequential() %>% 
  bidirectional(
    layer_gru(units = 32), input_shape = list(NULL, dim(data)[[-1]])
  ) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)