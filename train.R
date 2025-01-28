# left side is the names used in the code, right side is the internal names in CHAP
# Cases = number of cases
# E = population
# month = month
# ID_year = year
# ID_spat = location
# rainsum = rainfall
# meantemperature = mean_temperature

library(INLA)

train_chap <- function(train_fn, model_fn){
  # df <- read.csv(train_fn)
  # df <-group_by(df, location) |>
  #   mutate(month_num = row_number())
  # 
  # basis_meantemperature <- crossbasis(df$meantemperature, lag=3, 
  #                                     argvar = list(fun = "ns", knots = equalknots(df$meantemperature, 2)), 
  #                                     arglag = list(fun = "ns", knots = 3/2), group = df$ID_spat)
  # colnames(basis_meantemperature) = paste0("basis_meantemperature.", colnames(basis_meantemperature))
  # 
  # basis_rainsum <- crossbasis(df$rainsum, lag=3, 
  #                             argvar = list(fun = "ns", knots = equalknots(df$rainsum, 2)), 
  #                             arglag = list(fun = "ns", knots = 3/2), group = df$ID_spat)
  # colnames(basis_rainsum) = paste0("basis_rainsum.", colnames(basis_rainsum))
  # 
  # 
  # #also need some conversion from geojson file to adjacency matrix in R, obs for harmonization
  # #f(ID_spat, model = "icar", graph = adjacency_matrix), the ICAR formula, can also use a BYM
  # # just ICAR + iid for the spatial regions
  # #lagged_formula <- Cases ~ 1 + f(ID_spat, model='iid', replicate=ID_year) + 
  # #  f(month, model='rw1', cyclic=T, scale.model=T) + basis_meantemperature + basis_rainsum
  # 
  # df$ID_spat <- as.factor(df$ID_spat)
  # df$ID_spat_num <- as.numeric(as.factor(df$ID_spat))
  # 
  # lagged_formula <- Cases ~ 1 + f(ID_spat, model='iid', hyper=list(prec = list(prior = "pc.prec", 
  #   param = c(1, 0.01)))) + f(month_num, model = "rw1", scale.model = T, 
  #   replicate = ID_spat_num, hyper=list(prec = list(prior = "pc.prec", param = c(1, 0.01)))) +
  #   f(month, model='rw1', cyclic=T, scale.model=T, hyper=list(prec = list(prior = "pc.prec", 
  #   param = c(1, 0.01)))) + basis_meantemperature + basis_rainsum
  # 
  # model <- inla(formula = lagged_formula, data = df, family = "nbinomial", offset = log(E),
  #               control.inla = list(strategy = 'adaptive'),
  #               control.compute = list(config = TRUE, return.marginals = FALSE),
  #               control.fixed = list(correlation.matrix = TRUE, prec.intercept = 0.1, prec = 1),
  #               control.predictor = list(link = 1, compute = TRUE),
  #               verbose = F, safe=FALSE)
  # 
  # saveRDS(model, file = model_fn)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
  train_fn <- args[1]
  model_fn <- args[2]
  
  train_chap(train_fn, model_fn)
}


