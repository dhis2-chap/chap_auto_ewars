
source("train.R")
source("predict.R")

train_chap("example_data/trainData.csv", "example_data/model")
predict_chap("example_data/model", "example_data/historic_data.csv", "example_data/future_data.csv", "example_data/predictions.csv")

#preds <- read.csv("example_data/predictions.csv")
