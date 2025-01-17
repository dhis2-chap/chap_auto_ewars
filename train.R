# left side is the names used in the code, right side is the internal names in CHAP
# Cases = number of cases
# E = population
# month = month
# ID_year = year
# ID_spat = location
# rainsum = rainfall
# meantemperature = mean_temperature

library(INLA)
source('lib.R')

train_chap <- function(train_fn, model_fn){
  #would normally train the model here
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
  train_fn <- args[1]
  model_fn <- args[2]
  
  train_chap(csv_fn, model_fn)
}


