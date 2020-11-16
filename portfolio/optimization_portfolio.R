library(tseries)
library(zoo)
library(rio)
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
  fitted_variance = get_optimized_garch_variance(1, 1, daily_return)
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

# Kellys criterion dataframe train test split
k_train_length = ceiling(0.8*nrow(df_kelly))
k_train = df_kelly[1 : k_train_length,]
k_test = df_kelly[seq(k_train_length + 1, nrow(df_kelly)),]

# Daily return dataframe train test split
m_train_length = ceiling(0.8*nrow(df_moving_average))
avg_train = df_moving_average[1: m_train_length, ]
avg_test = df_moving_average[seq(m_train_length + 1, nrow(df_moving_average)),]

a_initial = 0.5
A_optim = optim(a_initial, general_kellys_criterion, df_kelly = k_train, df_moving_average= avg_train, method = 'Brent', lower = 0 , upper = 1,control = list(fnscale = -1))
A_optim

# Use the test set to see the performance
cumulative_return = general_kellys_criterion(A_optim$par, k_test, avg_test)
cumulative_return

compounded_return = get_compound_return(cumulative_return, nrow(df_kelly))
final_return = compounded_return - 1

print(paste("Compounded Return:", round(compounded_return, 5), sep = " "))
print(paste("Compound Final Return: ", round((final_return) * 100, 5), "%", sep = ""))

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
