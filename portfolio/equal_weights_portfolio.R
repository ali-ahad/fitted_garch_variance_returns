library(tseries)
library(zoo)
library(rio)
library(rugarch)
source("base.R")
source("methods.R")
cur_dir <- getwd()
setwd(cur_dir)

path = paste(cur_dir,'/datasets/large_cap.xlsx', sep = "")
df = excel_read(path)
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
  
  daily_return = get_daily_return(closing_prices)
  fitted_variance = get_optimized_garch_variance(daily_return, var_recursion_init = 1e-1, dist_model = 'sstd', arma_p = 1, arma_q = 1)
  moving_average = get_moving_average(daily_return, 3)
  kellys_vector = moving_average / fitted_variance
  
  daily_return_list[[i]] = daily_return
  moving_average_list[[i]] = moving_average
  kellys_list[[i]] = kellys_vector
}

# Transform daily return list to dataframe
df_daily_return = list_to_matrix(daily_return_list, names)
df_daily_return

# Transform moving average list to dataframe
df_moving_average = list_to_matrix(moving_average_list, names)
df_moving_average

# Transform kelly criterion list to dataframe
df_kelly = list_to_matrix(kellys_list, names)
df_kelly

# ************** METHODOLOGY 1 - Equal Weights **************** #
cumulative_return = equal_weights_return(df_kelly, df_moving_average)
compounded_return = get_compound_return(cumulative_return[length(cumulative_return)], length(cumulative_return))
final_return = compounded_return - 1

cumulative_return

print(paste("Compounded Return:", round(compounded_return, 5), sep = " "))
print(paste("Compound Final Return: ", round((final_return) * 100, 5), "%", sep = ""))

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
