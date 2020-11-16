library(tseries)
library(zoo)
library(rio)

# Read exceel files using rio library
excel_read <- function(path) {
  df = import_list(path)
  return (df)
}

# We calculated optimized nth variance by fitting GARCH from i to n-1 days and for the length of our daily returns
# For nth day variance, we use the the last value of fitted volatilities from GARCH results with its coefficients and previous day mean
get_optimized_garch_variance <- function(p, q, u1) {
  fitted_variance = c()
  for (i in 3: length(u1)) {
    res = garch(daily_return[1:i-1], order = c(p,q))
    coef = as.numeric(res$coef)
    volatilities = res$fitted.values[,1]
    tail = volatilities[length(volatilities)]
    nthday_volatility = coef[1] + (coef[2] * u1[i-1]^2) + (coef[3] * tail)
    fitted_variance = c(fitted_variance, nthday_volatility)
  }
  
  return (fitted_variance)
} 

get_daily_return <- function(closing_prices) {
  series = as.ts(closing_prices)
  daily_return <- (lag(series) - series) / series
  daily_return = daily_return[!is.na(daily_return)]
  return (daily_return)
}

get_moving_average <- function(mean_return, window) {
  moving_avg = rollmean(mean_return, window)
  return (moving_avg)
}

list_to_matrix <- function(list_to_transfrom, df_col_names) {
  df = data.frame(matrix(unlist(list_to_transfrom), ncol=length(list_to_transfrom), byrow = T))
  colnames(df) = df_col_names
  return (df)
}

get_compound_return <- function(cumulative_return, number_rows) {
  compound_return = (cumulative_return)^(1 / (number_rows - 1))
  return (compound_return)
}