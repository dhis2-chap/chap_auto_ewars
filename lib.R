# left side is the names used in the code, right side is the internal names in CHAP
# Cases = number of cases
# E = population
# month = month
# ID_year = year
# ID_spat = location
# rainsum = rainfall
# meantemperature = mean_temperature

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


