# !/usr/bin/env Rscript
# Y = number of cases
# E = pop.var.dat
# T1 = week
# T2 = year
# S1 = district

library(INLA)
source('lib.R')

# from_command_line= TRUE
# if (from_command_line) {
#     args = commandArgs(trailingOnly=TRUE)
# } else {
#     args = c('chap_ewars.model', 'future_data.csv', 'predictions.csv', 'none')
# }
# model_filename = args[1] # filename of the saved model
# historic_data_filename = args[2]
# data_filename =  args[3] # filename of the data necessary for prediction
# out_filename =  args[4] # where to save the predictions
# graph_filename =  args[5] # filename of the graph
# output_format = args[6]
# print(output_format)
# stopifnot(output_format %in% c('metrics', 'samples'))

# check if args[5] is set, if not set output format to samples
#if (length(args) < 5) {
#	output_format = 'metrics'
#	print('Output format not set, defaulting to metrics')
#} else {
#	print('Output format set to ' + output_format)
#}

model_fn <- "example_data/example_model.model"
hist_fn <- "historic_data.csv"
future_fn <- "future_data.csv"
preds_fn <- "predictions.csv"
format_str <- "metrics"

predict_chap <- function(model_fn, hist_fn, future_fn, preds_fn, graph_fn, format_str){
  stopifnot(format_str %in% c('metrics', 'samples'))
  load(file = model_fn)
  
  df <- read.csv(future_fn)
  df$Cases = rep(NA, nrow(df))
  df$disease_cases= rep(NA, nrow(df)) #so we can rowbind it with historic
  
  historic_df = read.csv(hist_fn)
  df = rbind(historic_df, df) #funker dette? eller må de være etter hverandre for hver lokasjon
  df <- offset_years_and_months(df)
  
  basis_meantemperature = extra_fields(df)
  basis_rainsum = get_basis_rainsum(df)
  summary(basis_rainsum)
  summary(basis_meantemperature)
  #na.mask = apply(basis_meantemperature, 1, function(row) (any(is.na(row))))
  
  
  #df$Cases <- as.integer(df$Cases)
  
  # formula_2 <- Cases ~ 1 + f(ID_spat, model = "iid", replicate = ID_year) + 
  #   f(month, model = "rw1", cyclic = T, scale.model = T) + basis_meantemperature + 
  #   basis_rainsum
  # 
  # model <- inla(formula = formula_2, data = df, family = "nbinomial", offset = log(E),
  #               control.inla = list(strategy = 'adaptive'),
  #               control.compute = list(dic = TRUE, config = TRUE, cpo = TRUE, return.marginals = FALSE),
  #               control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
  #               control.predictor = list(link = 1, compute = TRUE),
  #               verbose = F, safe=FALSE)
  
  summary(model)
  
  #new code for cbinding the basises to the df and also to update the formula
  
  #df <- cbind(df, basis_meantemperature, basis_rainsum)
  
  model = mymodel(selectedFormula, df, config = TRUE, basis_meantemperature = basis_meantemperature, basis_rainsum = basis_rainsum)
  
  casestopred <- df$Cases # response variable
  
  # Predict only for the cases where the response variable is missing
  idx.pred <- which(is.na(casestopred))
  mpred <- length(idx.pred)
  s <- 100
  y.pred <- matrix(NA, mpred, s)
  # Sample parameters of the model
  xx <- inla.posterior.sample(s, model)  # This samples parameters of the model
  xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx) # This extracts the expected value and hyperparameters from the samples
  
  # Sample predictions
  for (s.idx in 1:s){
    xx.sample <- xx.s[, s.idx]
    y.pred[, s.idx] <- rnbinom(mpred,  mu = exp(xx.sample[-1]), size = xx.sample[1])
  }
  #print(y.pred)
  # Generate new dataframe with summary statistics
  #print(y.pred)
  if (output_format == 'metrics') {
    new.df = df[idx.pred,]
    new.df$mean = rowMeans(y.pred)
    new.df$std = apply(y.pred, 1, sd)
    new.df$max = apply(y.pred, 1, max)
    new.df$min = apply(y.pred, 1, min)
    new.df$quantile_low = apply(y.pred, 1, function(row) quantile(row, 0.1))
    new.df$median = apply(y.pred, 1, function(row) quantile(row, 0.5))
    new.df$quantile_high = apply(y.pred, 1, function(row) quantile(row, 0.9))
  } else {
    # make a dataframe where first column is the time points, second column is the location, rest is the samples
    # rest of columns should be called sample_0, sample_1, etc
    new.df = data.frame(time_period = df$time_period[idx.pred], location = df$location[idx.pred], y.pred)
    colnames(new.df) = c('time_period', 'location', paste0('sample_', 0:(s-1)))
  }
  
  # Write new dataframe to file
  write.csv(new.df, preds_fn)
}

# 
# #data_filename = 'future_data.csv'
# #model_filename = 'tmp.csv'
# #out_filename = 'tmp.txt'
# #graph_filename = 'none'
# source('lib.R') # mymodel, extra_fields
# #inla.debug.graph(graph_filename)
# # Load the model
# load(file = model_filename)
# 
# # Load the data
# df <- read.table(data_filename, sep=',', header=TRUE)
# # add disease cases column with nan values
# df$Cases = rep(NA, nrow(df))
# df$disease_cases= rep(NA, nrow(df))
# 
# # print start of df
# print(head(df))
# 
# historic_df = read.table(historic_data_filename, sep=',', header=TRUE)
# # add historic_df at the beginning of df
# print("historic")
# print(head(historic_df))
# print("df")
# print(head(df))
# df = rbind(historic_df, df)
# 
# df <- offset_years_and_months(df)
# #df$week = as.numeric(substr(df$time_period, 6, 8))
# basis_meantemperature = extra_fields(df)
# basis_rainfall = get_basis_rainfall(df)
# # create a row mask for any missing values in row
# na.mask = apply(basis_meantemperature, 1, function(row) (any(is.na(row))))
# #df = df[!na.mask,]
# #basis_meantemperature = basis_meantemperature[!na.mask,]
# model = mymodel(selectedFormula, df, config = TRUE)
# #model2 = mymodel(lagged_formula, df, config = TRUE)
# 
# 
# #For each top 5
# ## Do out of sample
# ## Set NA one year, train model, sample from the posterior of  the NA, calculate |Prediction-Truth|
# ##
# ## Calculate MAE
# 
# casestopred <- df$Cases # response variable
# 
# # Predict only for the cases where the response variable is missing
# idx.pred <- which(is.na(casestopred))
# mpred <- length(idx.pred)
# s <- 100
# y.pred <- matrix(NA, mpred, s)
#     # Sample parameters of the model
# xx <- inla.posterior.sample(s, model)  # This samples parameters of the model
# xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx) # This extracts the expected value and hyperparameters from the samples
# 
# # Sample predictions
# for (s.idx in 1:s){
#     xx.sample <- xx.s[, s.idx]
#     y.pred[, s.idx] <- rnbinom(mpred,  mu = exp(xx.sample[-1]), size = xx.sample[1])
#     }
# #print(y.pred)
# # Generate new dataframe with summary statistics
# #print(y.pred)
# if (output_format == 'metrics') {
# 	new.df = df[idx.pred,]
# 	new.df$mean = rowMeans(y.pred)
# 	new.df$std = apply(y.pred, 1, sd)
# 	new.df$max = apply(y.pred, 1, max)
# 	new.df$min = apply(y.pred, 1, min)
# 	new.df$quantile_low = apply(y.pred, 1, function(row) quantile(row, 0.1))
# 	new.df$median = apply(y.pred, 1, function(row) quantile(row, 0.5))
# 	new.df$quantile_high = apply(y.pred, 1, function(row) quantile(row, 0.9))
# } else {
#  	# make a dataframe where first column is the time points, second column is the location, rest is the samples
#  	# rest of columns should be called sample_0, sample_1, etc
#  	new.df = data.frame(time_period = df$time_period[idx.pred], location = df$location[idx.pred], y.pred)
#  	colnames(new.df) = c('time_period', 'location', paste0('sample_', 0:(s-1)))
# }
# 
# # Write new dataframe to file
# write.csv(new.df, out_filename)
# 
