library(tseries)
library(zoo)
library(rio)
source("base.R")
source("methods.R")
cur_dir <- getwd()
setwd(cur_dir)

path = paste(cur_dir,'/datasets/profit_earnings.xlsx', sep = "")
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
name = "energy"
codes = basic_materials[,2]
codes
names = basic_materials[,1]

# Getting the branchmark data (SPY)
SPY_closing_prices = get.hist.quote(instrument = "SPY", start = "2019-11-12", end = "2020-11-13", quote = "Close", provider = "yahoo")
SPY_daily_return = get_daily_return(SPY_closing_prices)
SPY_cumlative_return=cumprod(1+SPY_daily_return)
SPY_cumlative_return_ts = as.ts(cumprod(1+SPY_daily_return))

SPY_compounded_return = get_compound_return(SPY_cumlative_return[length(SPY_cumlative_return)], length(SPY_cumlative_return))
SPY_final_return = SPY_compounded_return - 1

# Create a list of daily returns, moving averages and kellys criterion such that each index represents a stock
for (i in 1:length(codes)) {
  closing_prices = get.hist.quote(instrument = codes[i], start = "2019-11-12", end = "2020-11-13", quote = "Close", provider = "yahoo")
  
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

# Computing the Long and Hold Strategy on target Stock evenly
daily_return_matrix<-as.matrix(df_daily_return)
w<-as.matrix(rep(0.2,5))
Long_Hold_Daily_Return<-daily_return_matrix%*%w
Long_Hold_Cumulative_Return<-cumprod(1+Long_Hold_Daily_Return)
Long_Hold_compounded_return = get_compound_return(Long_Hold_Cumulative_Return[length(Long_Hold_Cumulative_Return)], length(Long_Hold_Cumulative_Return))
Long_Hold_final_return = Long_Hold_compounded_return - 1

#################################################################
# ************** METHODOLOGY 1 - Equal Weights **************** #
################################################################
equal_cumulative_return = equal_weights_return(df_kelly, df_daily_return)
equal_compounded_return = get_compound_return(equal_cumulative_return[length(equal_cumulative_return)], length(equal_cumulative_return))
equal_final_return = equal_compounded_return - 1

equal_cumulative_return

print(paste("Long_Hold_Cumulative Return:", round(Long_Hold_Cumulative_Return[length(Long_Hold_Cumulative_Return)], 5),sep = " "))
print(paste("SPY_Cumulative Return:", round(SPY_cumlative_return[length(SPY_cumlative_return)], 5),sep = " "))
print(paste("Cumulative Return:", round(equal_cumulative_return[length(equal_cumulative_return)], 5), sep = " "))
print(paste("SPY_Compounded Return:", round(SPY_compounded_return, 5), sep = " "))
print(paste("SPY_Compound Final Return: ", round((SPY_final_return) * 100, 5), "%", sep = ""))
print(paste("Long_Hold_Compounded Return:", round(Long_Hold_compounded_return, 5), sep = " "))
print(paste("Long_Hold_Compound Final Return: ", round((Long_Hold_final_return) * 100, 5), "%", sep = ""))
print(paste("Compounded Return:", round(equal_compounded_return, 5), sep = " "))
print(paste("Compound Final Return: ", round((equal_final_return) * 100, 5), "%", sep = ""))

equal_cumulative_return_ts<-as.ts(equal_cumulative_return)

y_max<-max(max(equal_cumulative_return_ts),max(SPY_cumlative_return_ts),max(Long_Hold_Cumulative_Return))
y_min<-min(min(equal_cumulative_return_ts),min(SPY_cumlative_return_ts),min(Long_Hold_Cumulative_Return))

n<-paste(name,"_equal_weight")

pdf(n)

plot(equal_cumulative_return_ts,ylim=c(y_min,y_max),xlab = 'Time(days)',ylab = 'Cumulative Return', col='red')
title(name)
lines(SPY_cumlative_return_ts,col='blue')
lines(Long_Hold_Cumulative_Return,col='green')
legend("topleft",legend = c("Portfolio_Equal_Weight","SPY","Long&Hold"),col = c("red","blue","green"), fill = c("red","blue","green"))

dev.off()

####################################################################################
# ************** METHODOLOGY 2 - Weights dependent on kelly value **************** #
####################################################################################
kelly_cumulative_return = kelly_weights_return(df_kelly, df_daily_return)
kelly_compounded_return = get_compound_return(kelly_cumulative_return[length(kelly_cumulative_return)], length(kelly_cumulative_return))
kelly_final_return = kelly_compounded_return - 1

kelly_cumulative_return

print(paste("Long_Hold_Cumulative Return:", round(Long_Hold_Cumulative_Return[length(Long_Hold_Cumulative_Return)], 5),sep = " "))
print(paste("SPY_Cumulative Return:", round(SPY_cumlative_return[length(SPY_cumlative_return)], 5),sep = " "))
print(paste("Cumulative Return:", round(kelly_cumulative_return[length(kelly_cumulative_return)], 5), sep = " "))
print(paste("Long_Hold_Compounded Return:", round(Long_Hold_compounded_return, 5), sep = " "))
print(paste("Long_Hold_Compound Final Return: ", round((Long_Hold_final_return) * 100, 5), "%", sep = ""))
print(paste("SPY_Compounded Return:", round(SPY_compounded_return, 5), sep = " "))
print(paste("SPY_Compound Final Return: ", round((SPY_final_return) * 100, 5), "%", sep = ""))
print(paste("Kelly Weighted Compounded Return:", round(kelly_compounded_return, 5), sep = " "))
print(paste("Kelly Weighted Compound Final Return: ", round((kelly_final_return) * 100, 5), "%", sep = ""))

kelly_cumulative_return_ts<-as.ts(kelly_cumulative_return)

y_max<-max(max(kelly_cumulative_return_ts),max(SPY_cumlative_return_ts),max(Long_Hold_Cumulative_Return))
y_min<-min(min(kelly_cumulative_return_ts),min(SPY_cumlative_return_ts),min(Long_Hold_Cumulative_Return))

n<-paste(name,"_kelly_citerion")

pdf(n)

plot(kelly_cumulative_return_ts,ylim=c(y_min,y_max),xlab = 'Time(days)',ylab = 'Cumulative Return', col='red')
title(name)
lines(SPY_cumlative_return_ts,col='blue')
lines(Long_Hold_Cumulative_Return,col='green')
legend("topleft",legend = c("Portfolio_Kelly_Weight","SPY","Long&Hold"),col = c("red","blue","green"), fill = c("red","blue","green"))

dev.off()

################################################################################################
# ************** METHODOLOGY 3 - Parameter choosing on maximization of profit **************** #
################################################################################################
# Kellys criterion dataframe train test split
k_train_length = ceiling(0.8*nrow(df_kelly))
k_train = df_kelly[1 : k_train_length,]
k_test = df_kelly[seq(k_train_length + 1, nrow(df_kelly)),]

# Daily return dataframe train test split
m_train_length = ceiling(0.8*nrow(df_daily_return))
avg_train = df_daily_return[1: m_train_length, ]
avg_test = df_daily_return[seq(m_train_length + 1, nrow(df_daily_return)),]

a_initial = 0.5
A_optim = optim(a_initial, general_kellys_criterion, df_kelly = k_train, df_daily_return = avg_train, method = 'Brent', lower = 0 , upper = 1,control = list(fnscale = -1))
A_optim

# Use the test set to see the performance
optim_cumulative_return = general_kellys_criterion(A_optim$par, k_test, avg_test)
optim_cumulative_return

optim_cumulative_return_series=general_kellys_criterion_cumulative_return_series(A_optim$par, k_test, avg_test)
optim_cumulative_return_series_ts<-as.ts(optim_cumulative_return_series)

optim_compounded_return = get_compound_return(optim_cumulative_return, nrow(k_test))
optim_final_return = optim_compounded_return - 1

#Computing the corresponding SPY Return
SPY_daily_return_corr<-SPY_daily_return[length(SPY_daily_return)-length(optim_cumulative_return_series_ts)+1:length(SPY_daily_return)]
SPY_cumlative_return_corr<-cumprod(1+SPY_daily_return_corr[1:length(optim_cumulative_return_series_ts)])
SPY_cumlative_return_corr_ts<-as.ts(SPY_cumlative_return_corr)
SPY_compounded_return_corr = get_compound_return(SPY_cumlative_return_corr[length(SPY_cumlative_return_corr)], length(SPY_cumlative_return_corr))
SPY_final_return_corr = SPY_compounded_return_corr - 1

#Computing the corresponding Long&Hold Return
Long_Hold_daily_return_corr<-Long_Hold_Daily_Return[length(Long_Hold_Daily_Return)-length(optim_cumulative_return_series_ts)+1:length(Long_Hold_Daily_Return)]
Long_Hold_cumlative_return_corr<-cumprod(1+Long_Hold_daily_return_corr[1:length(optim_cumulative_return_series_ts)])
Long_Hold_cumlative_return_corr_ts<-as.ts(Long_Hold_cumlative_return_corr)
Long_Hold_compounded_return_corr = get_compound_return(Long_Hold_cumlative_return_corr[length(Long_Hold_cumlative_return_corr)], length(Long_Hold_cumlative_return_corr))
Long_Hold_final_return_corr = Long_Hold_compounded_return_corr - 1

print(paste("The optimal Kelly Citerion :",A_optim$par))
print(paste("Long_Hold_Cumulative Return:", round(Long_Hold_cumlative_return_corr[length(Long_Hold_cumlative_return_corr)], 5),sep = " "))
print(paste("SPY_Cumulative Return:", round(SPY_cumlative_return_corr[length(SPY_cumlative_return_corr)], 5),sep = " "))
print(paste("Cumulative Return:", round(optim_cumulative_return, 5), sep = " "))
print(paste("Long_Hold_Compounded Return:", round(Long_Hold_compounded_return_corr, 5), sep = " "))
print(paste("Long_Hold_Compound Final Return: ", round((Long_Hold_final_return_corr) * 100, 5), "%", sep = ""))
print(paste("SPY_Compounded Return:", round(SPY_compounded_return_corr, 5), sep = " "))
print(paste("SPY_Compound Final Return: ", round((SPY_final_return_corr) * 100, 5), "%", sep = ""))
print(paste("Optimized Parameter Compounded Return:", round(optim_compounded_return, 5), sep = " "))
print(paste("Optimized Parameter Compound Final Return: ", round((optim_final_return) * 100, 5), "%", sep = ""))

optim_cumulative_return_series_ts<-as.ts(optim_cumulative_return_series)

y_max<-max(max(optim_cumulative_return_series_ts),max(SPY_cumlative_return_corr_ts),max(Long_Hold_cumlative_return_corr_ts))
y_min<-min(min(optim_cumulative_return_series_ts),min(SPY_cumlative_return_corr_ts),min(Long_Hold_cumlative_return_corr_ts))

n<-paste(name,"_Optimal_citerion")

pdf(n)

plot(optim_cumulative_return_series_ts ,ylim=c(y_min,y_max),xlab = 'Time(days)',ylab = 'Cumulative Return', col='red')
title(name)
lines(SPY_cumlative_return_corr_ts,col='blue')
lines(Long_Hold_cumlative_return_corr_ts,col='green')
legend("topleft",legend = c("Portfolio_Optimal_Citerion","SPY","Long&Hold"),col = c("red","blue","green"), fill = c("red","blue","green"))

dev.off()

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
