
source("train.R")
source("predict.R")

train_chap("example_data/trainData.csv", "example_data/model")
predict_chap("example_data/model", "example_data/historic_data.csv", "example_data/future_data.csv", "example_data/predictions.csv")


library(tsibble)
library(dplyr)
preds <- read.csv("example_data/predictions.csv")
model <- readRDS("example_data/model")

summary(model)

preds <- filter(preds, yearmonth(time_period) >= yearmonth("2017-01")) #only works for this specific test data
yearmonth(preds[1, "time_period"]) < yearmonth("2017-01")


