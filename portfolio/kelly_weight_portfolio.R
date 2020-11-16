library(tseries)
library(zoo)
library(rio)
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

path = paste(cur_dir,'/datasets/large_cap.xlsx', sep = "")
df = import_list(path)
basic_materials = df$basic_materials
capital_goods = df$capital_goods
consumer_cyclical = df$consumer_cyclical
consumer_noncyclical = df$consumer_noncyclical
energy = df$energy
financial = df$financial
healthcare = df$healthcare
technology = df$technology
utilities = df$utilities

# Initializing lists for transforming them in matrices later
daily_return_list = list()
moving_average_list = list()
kellys_list = list()

# Change the dataframe here for appropriate codes and names
codes = basic_materials[,2]
names = basic_materials[,1]

# Create a list of daily returns, moving averages and kellys criterion such that each index represents a stock
for (i in 1:length(codes)) {
  closing_prices = get.hist.quote(instrument = codes[i], start = "2020-01-01", end = "2020-11-13", quote = "Close", provider = "yahoo")
  series = as.ts(closing_prices)
  daily_return = (lag(series) - series) / series
  
  daily_return = daily_return[!is.na(daily_return)]
  fitted_variance = optimized_garch_variance(1, 1, daily_return)
  moving_average = rollmean(daily_return, 3)
  kellys_vector = moving_average / fitted_variance
  
  daily_return_list[[i]] = daily_return
  moving_average_list[[i]] = moving_average
  kellys_list[[i]] = kellys_vector
}

# Transform daily return list to dataframe
df_daily_return = data.frame(matrix(unlist(daily_return_list), ncol=length(daily_return_list), byrow = T))
colnames(df_daily_return) = names
df_daily_return

# Transform moving average list to dataframe
df_moving_average = data.frame(matrix(unlist(moving_average_list), ncol = length(moving_average_list), byrow = T))
colnames(df_moving_average) = names
df_moving_average

# Transform kelly criterion list to dataframe
df_kelly = data.frame(matrix(unlist(kellys_list), ncol = length(kellys_list), byrow = T))
colnames(df_kelly) = names
df_kelly

# ************** METHODOLOGY 2 **************** #
# For each day assign weights according the value of kelly criterion for each stock in that portfolio
# Base this weight as a probability of kelly value over the sum of values for that day such that the kelly values are > 1/2
# Else assign 0 as a weight.
# Track cumulative returns for the portfolio over the course of dates
cumulative_return = c(1)
for (i in 2:nrow(df_kelly)) {
  
  kellys_val = as.numeric(df_kelly[i-1,])
  avg = as.numeric(df_moving_average[i,])
  weight = c()
  condtional_sum = sum(kellys_val[kellys_val > 1/2])
  if (condtional_sum == 0) {
    conditional_sum = 1
  }
  
  for (j in 1:length(names)) {
    if (kellys_val[j] > 1/2) {
      weight = c(weight, kellys_val[j] / condtional_sum)
    } else {
      weight = c(weight, 0)
    }
  }
  
  latest_return = cumulative_return[length(cumulative_return)]
  append_return = latest_return * (1 + (t(avg) %*% weight))
  cumulative_return = c(cumulative_return, append_return)
}

compounded_return = (cumulative_return[length(cumulative_return)])^(1 / (length(cumulative_return) - 1))
final_return = compounded_return - 1

cumulative_return

print(paste("Compounded Return:", round(compounded_return, 5), sep = " "))
print(paste("Compound Final Return: ", round((final_return) * 100, 5), "%", sep = ""))

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
