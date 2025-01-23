# left side is the names used in the code, right side is the internal names in CHAP
# Cases = number of cases
# E = population
# month = month
# ID_year = year
# ID_spat = location
# rainsum = rainfall
# meantemperature = mean_temperature


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



