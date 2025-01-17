
source("train.R")
source("predict.R")

train_chap("example_data/trainData.csv", "model")
predict_chap("model", "historic_data.csv", "future_data.csv", "predictions.csv", "", "samples")

hist_df <- read.csv("historic_data.csv")
future_df <- read.csv("future_data.csv")
#For testing with the CHAP-data locally
#train_chap("input/training_data.csv", "output/model.bin")
#predict_chap("output/model.bin", "input/historic_data.csv", "input/future_data.csv", "output/predictions_CHAP.csv")

Rscript train.R example_data/trainData.csv example_data/example_model.model
Rscript predict.R example_data/example_model.model example_data/trainData.csv example_data/futureData.csv example_data/predictionsHal.csv graphName metrics

inla.doc("nbinomial")

