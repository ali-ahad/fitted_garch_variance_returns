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
get_optimized_garch_variance <- function(u1, garch_p = 1, garch_q = 1, var_recursion_init = 'all', dist_model = 'norm', arma_p = 0, arma_q = 0) {
  fitted_variance = c()
  for (i in 3: length(u1)) {
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arma_p,arma_q), include.mean = TRUE, archm = TRUE, archpow = 1), 
                             variance.model = list(model = "sGARCH", garchOrder = c(garch_p,garch_q)),
                             distribution.model = dist_model)
    
    set.seed(1)
    garch_fit <- ugarchfit(spec = garch_spec, data = u1, solver = 'hybrid', fit.control = list(rec.init = var_recursion_init))
    coef = coef(garch_fit)
    volatilities = garch_fit@fit$var
    
    tail = volatilities[length(volatilities)]
    nthday_volatility = as.numeric(coef["omega"]) + (as.numeric(coef["alpha1"]) * u1[i-1]^2) + (as.numeric(coef["beta1"]) * tail)
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