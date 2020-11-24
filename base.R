library(tseries)
library(zoo)
library(rio)
library(rugarch)

# Read exceel files using rio library
excel_read <- function(path) {
  df = import_list(path)
  return (df)
}

# We calculated optimized nth variance by fitting GARCH from i to n-1 days and for the length of our daily returns
# For nth day variance, we use the the last value of fitted volatilities from GARCH results with its coefficients and previous day mean
# We use rugarch package to deal with false convergence problem.
# Although we wont get the exact likelihood as true convergence, but we use the parameters that provides us a decent trade off between performance and time complexity
# The default values of parameters are used as bencmark 
get_optimized_garch_variance <- function(p, q, u1) {
  fitted_variance = c()
  for (i in 3: length(u1)) {
    res = garch(tail(u1[1:i-1], n = 20), order = c(p,q), control = garch.control(falsetol = 1e-2))
    res$n.likeli
    coef = as.numeric(res$coef)
    volatilities = res$fitted.values[,1]
    tail = volatilities[length(volatilities)]
    nthday_volatility = coef[1] + (coef[2] * u1[i-1]^2) + (coef[3] * tail)
    fitted_variance = c(fitted_variance, nthday_volatility)
  }
  
  return (fitted_variance)
} 

# Function to get daily return    
get_daily_return <- function(prices) {
  opening = as.ts(prices[,1])
  closing = as.ts(prices[,2])
  daily_return = (closing - opening) / opening
  daily_return = daily_return[!is.na(daily_return)]
  return (daily_return)
}

# Function to get moving average over a window
get_moving_average <- function(mean_return, window) {
  moving_avg = rollmean(mean_return, window)
  return (moving_avg)
}

# Function to change list of returns to matrix
list_to_matrix <- function(list_to_transfrom, df_col_names) {
  df = data.frame(matrix = matrix(unlist(list_to_transfrom), ncol = length(df_col_names)))
  colnames(df) = df_col_names
  return (df)
}

# Function to get compounded return at end of period
get_compound_return <- function(cumulative_return, number_rows) {
  compound_return = (cumulative_return)^(1 / (number_rows))
  return (compound_return)
}
