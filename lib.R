# Y = number of cases
# E = pop.var.dat
# T1 = week
# T2 = year
# S1 = district

#Data operations

# library(tsibble)
# library(lubridate)
# library(dplyr)
# 
#future_df <- read.csv("example_data/futureData.csv")
#hist_df <- read.csv("example_data/trainData.csv") #|>
#   mutate(month = month(yearmonth(time_period)))
# 
#colnames(df)[c(8, 11, 12)] <- c("Cases", "ID_year", "ID_spat")

#write.csv(df, file = "example_data/trainData.csv")


local({r <- getOption("repos")
       r["CRAN"] <- "https://cran.r-project.org"
       options(repos=r)
})

library(tsModel)
library(dlnm)

get_crossbasis <- function(var, group, nlag){
    #lagged <- tsModel::Lag(var, group = group, k = 0:nlag)
    lagknot = equalknots(0:nlag, 2)
    basis <- crossbasis(var, argvar = list(fun = "ns", knots = equalknots(var, 2)), arglag = list(fun = "ns", knots = nlag/2))
    return(basis)
}

get_last_month <- function(df) {
  df = df[!is.na(df$Cases),]
  return(df$month[length(df$month)])
}

get_month_diff <- function(df){
  last_month = get_last_month(df)
  
  if (last_month<=6) {
    month_diff = 6-last_month
  } else {
    month_diff = 18-last_month
  }
  return(month_diff)
}

offset_years_and_months <- function(df) {
  month_diff = get_month_diff(df)
  new_month = df$month + month_diff
  month = ((new_month-1) %% 12)+1
  ID_year = ifelse(new_month>12, df$ID_year+1, df$ID_year)
  df$month = month
  df$ID_year = ID_year
  return(df)
}


extra_fields <- function(df) {
    basis_meantemperature <- get_crossbasis(df$meantemperature, df$ID_spat, 3)
    colnames(basis_meantemperature) = paste0("basis_meantemperature.", colnames(basis_meantemperature))
    return (basis_meantemperature)
}

get_basis_rainsum <- function(df) {
  basis <- get_crossbasis(df$rainsum, df$ID_spat, 3)
  colnames(basis) = paste0('basis_rainsum.', colnames(basis))
  return (basis)
}


mymodel <- function(formula, data = df, family = "nbinomial", config = FALSE, basis_meantemperature = NA, basis_rainsum = NA)
{
  model <- inla(formula = formula, data = data, family = family, offset = log(E),
                control.inla = list(strategy = 'adaptive'),
                control.compute = list(dic = TRUE, config = config, cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE),
                verbose = F, safe=FALSE)
  return(model)
}

basis_formula <- Cases ~ 1 + f(ID_spat, model='iid', replicate=ID_year) + f(month, model='rw1', cyclic=T, scale.model=T)
lagged_formula <- Cases ~ 1 + f(ID_spat, model='iid', replicate=ID_year) + f(month, model='rw1', cyclic=T, scale.model=T) + basis_meantemperature + basis_rainsum

