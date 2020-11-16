library(tseries)
library(zoo)
cur_dir <- getwd()
setwd(cur_dir)

# We calculated optimized nth variance by fitting GARCH from i to n-1 days and for the length of our daily returns
# For nth day variance, we use the the last value of fitted volatilities from GARCH results with its coefficients and previous day mean
optimized_garch_variance <- function(p, q, u1) {
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

closing_prices = get.hist.quote(instrument = "BHP", start = "2020-01-01", end = "2020-11-13", quote = "Close", provider = "yahoo")
closing_prices

series = as.ts(closing_prices)

# Calculate daily percentage returns and clean the result to omit NA values
daily_return = (lag(series) - series) / series
daily_return = daily_return[!is.na(daily_return)]
daily_return

# Calculate the moving average for daily returns from zoo library
moving_average = rollmean(daily_return, 3)
moving_average

fitted_variance = optimized_garch_variance(1, 1, daily_return)
kellys_vector = moving_average / fitted_variance
kellys_vector

# ***************************** #
# CODE FOR APPLYING METHODOLGOY 1
# GIVING EQUAL WEIGHT TO DAYS WHERE KELLY'S VALUE > 1/3
cumulative_return = c(1)
for (i in 2:length(kellys_vector)) {
  kelly_val = kellys_vector[i-1]
  if (kelly_val > 0.5) {
    weight = 1/3
  } else {
    weight = 0
  }
  
  cur_return = daily_return[i]
  latest_cumulative_return = cumulative_return[length(cumulative_return)]
  cumulative_return = c(cumulative_return, latest_cumulative_return + (weight * cur_return))  
}

cumulative_return
# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
