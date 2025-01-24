
source("train.R")
source("predict.R")

#test with random data
train_chap("example_data/training_data.csv", "model")
predict_chap("model", "example_data/historic_data.csv", "example_data/future_data.csv", "example_data/predictions.csv")

#test with data from Vietnam
#train_chap("example_data_Viet/trainData.csv", "model")
#predict_chap("model", "example_data_Viet/historic_data.csv", "example_data_Viet/future_data.csv", "example_data_Viet/predictions.csv")

#test with data from Vietnam data that failed
#train_chap("ex_data_fails/trainData.csv", "model")
#predict_chap("model", "ex_data_fails/historic_data.csv", "ex_data_fails/future_data.csv", "ex_data_fails/predictions.csv")


#testing
# library(tsibble)
# library(dplyr)
# preds <- read.csv("example_data/predictions.csv")
model2 <- readRDS("model")

summary(model2)

summary(model)
month_num <- model$summary.random$month_num
n = 4
plot(month_num$mean[(198*n):(198*(n+1))])

season <- model$summary.random$month
plot(season$mean)


# preds <- filter(preds, yearmonth(time_period) >= yearmonth("2017-01")) #only works for this specific test data
# yearmonth(preds[1, "time_period"]) < yearmonth("2017-01")


