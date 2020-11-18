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

# Getting the branchmark data (SPY)
  SPY_closing_prices = get.hist.quote(instrument = "SPY", start = "2020-01-01", end = "2020-11-13", quote = "Close", provider = "yahoo")
  SPY_daily_return = get_daily_return(SPY_closing_prices)
  SPY_cumlative_return=cumprod(1+SPY_daily_return)
  SPY_cumlative_return_ts = as.ts(cumprod(1+SPY_daily_return))
  
  SPY_compounded_return = get_compound_return(SPY_cumlative_return[length(SPY_cumlative_return)], length(SPY_cumlative_return))
  SPY_final_return = SPY_compounded_return - 1

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

#################################################################
# ************** METHODOLOGY 1 - Equal Weights **************** #
################################################################
equal_cumulative_return = equal_weights_return(df_kelly, df_moving_average)
equal_compounded_return = get_compound_return(equal_cumulative_return[length(equal_cumulative_return)], length(equal_cumulative_return))
equal_final_return = equal_compounded_return - 1

print(paste("SPY_Cumulative Return:", round(SPY_cumlative_return[length(SPY_cumlative_return)], 5),sep = " "))
print(paste("Cumulative Return:", round(equal_cumulative_return[length(equal_cumulative_return)], 5), sep = " "))
print(paste("SPY_Compounded Return:", round(SPY_compounded_return, 5), sep = " "))
print(paste("SPY_Compound Final Return: ", round((SPY_final_return) * 100, 5), "%", sep = ""))
print(paste("Compounded Return:", round(equal_compounded_return, 5), sep = " "))
print(paste("Compound Final Return: ", round((equal_final_return) * 100, 5), "%", sep = ""))

equal_cumulative_return_ts<-as.ts(equal_cumulative_return)

plot(equal_cumulative_return_ts,xlab = 'Time',ylab = 'Cumulative Return', col='red')
lines(SPY_cumlative_return_ts,col='blue')

####################################################################################
# ************** METHODOLOGY 2 - Weights dependent on kelly value **************** #
####################################################################################
kelly_cumulative_return = kelly_weights_return(df_kelly, df_moving_average)
kelly_compounded_return = get_compound_return(kelly_cumulative_return[length(kelly_cumulative_return)], length(kelly_cumulative_return))
kelly_final_return = kelly_compounded_return - 1

kelly_cumulative_return

print(paste("SPY_Cumulative Return:", round(SPY_cumlative_return[length(SPY_cumlative_return)], 5),sep = " "))
print(paste("Cumulative Return:", round(kelly_cumulative_return[length(kelly_cumulative_return)], 5), sep = " "))
print(paste("SPY_Compounded Return:", round(SPY_compounded_return, 5), sep = " "))
print(paste("SPY_Compound Final Return: ", round((SPY_final_return) * 100, 5), "%", sep = ""))
print(paste("Kelly Weighted Compounded Return:", round(kelly_compounded_return, 5), sep = " "))
print(paste("Kelly Weighted Compound Final Return: ", round((kelly_final_return) * 100, 5), "%", sep = ""))

kelly_cumulative_return_ts<-as.ts(equal_cumulative_return)

plot(kelly_cumulative_return_ts,xlab = 'Time',ylab = 'Cumulative Return', col='red')
lines(SPY_cumlative_return_ts,col='blue')

################################################################################################
# ************** METHODOLOGY 3 - Parameter choosing on maximization of profit **************** #
################################################################################################
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
optim_cumulative_return = general_kellys_criterion(A_optim$par, k_test, avg_test)
optim_cumulative_return

optim_cumulative_return_series=general_kellys_criterion_cumulative_return_series(A_optim$par, k_test, avg_test)

optim_compounded_return = get_compound_return(optim_cumulative_return, nrow(df_kelly))
optim_final_return = optim_compounded_return - 1

print("The optimal Kelly Citerion",A_optim$par)
print(paste("SPY_Cumulative Return:", round(SPY_cumlative_return[length(SPY_cumlative_return)], 5),sep = " "))
print(paste("Cumulative Return:", round(optim_cumulative_return, 5), sep = " "))
print(paste("SPY_Compounded Return:", round(SPY_compounded_return, 5), sep = " "))
print(paste("SPY_Compound Final Return: ", round((SPY_final_return) * 100, 5), "%", sep = ""))
print(paste("Optimized Parameter Compounded Return:", round(optim_compounded_return, 5), sep = " "))
print(paste("Optimized Parameter Compound Final Return: ", round((optim_final_return) * 100, 5), "%", sep = ""))

optim_cumulative_return_series_ts<-as.ts(optim_cumulative_return_series)

plot(optim_cumulative_return_series_ts,xlab = 'Time',ylab = 'Cumulative Return', col='red')
lines(SPY_cumlative_return_ts,col='blue')

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
