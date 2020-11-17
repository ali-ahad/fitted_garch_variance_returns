library(tseries)
library(zoo)
library(rio)

# ************** METHODOLOGY 1 **************** #
# For each day assign equal weights for each stock in portfolio
# Track cumulative returns for the portfolio over the course of dates
equal_weights_return <- function(df_kelly, df_moving_average) {
  cumulative_return = c(1)
  for (i in 2:nrow(df_kelly)) {
    
    kellys_val = as.numeric(df_kelly[i-1,])
    avg = as.numeric(df_moving_average[i,])
    weight = c()
    
    for (j in 1:length(names)) {
      if (kellys_val[j] > 1/2) {
        weight = c(weight, (1 / ncol(df_kelly)))
      } else {
        weight = c(weight, 0)
      }
    }
    
    latest_return = cumulative_return[length(cumulative_return)]
    append_return = latest_return * (1 + (t(avg) %*% weight))
    cumulative_return = c(cumulative_return, append_return)
  } 
  return (cumulative_return)
}

# ************** METHODOLOGY 2 **************** #
# For each day assign weights according the value of kelly criterion for each stock in that portfolio
# Base this weight as a probability of kelly value over the sum of values for that day such that the kelly values are > 1/2
# Else assign 0 as a weight.
# Track cumulative returns for the portfolio over the course of dates
kelly_weights_return <- function(df_kelly, df_moving_average) {
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
  return (cumulative_return)
}

# ************** METHODOLOGY 3 **************** #
# Find parameeter a for kelly criterion to assign equal weights such that a maximizes the cumulative return
# Test the performance over a test set while train the model over a train set
general_kellys_criterion <- function(a, df_kelly, df_moving_average) {
  cumulative_return = c(1)
  for (i in 2:nrow(df_kelly)) {
    
    kellys_val = as.numeric(df_kelly[i-1,])
    avg = as.numeric(df_moving_average[i,])
    weight = c()
    
    for (j in 1:length(names)) {
      if (kellys_val[j] > a) {
        weight = c(weight, (1 / ncol(df_kelly)))
      } else {
        weight = c(weight, 0)
      }
    }
    
    latest_return = cumulative_return[length(cumulative_return)]
    append_return = latest_return * (1 + (t(avg) %*% weight))
    cumulative_return = c(cumulative_return, append_return)
  }
  
  return(cumulative_return[length(cumulative_return)])
}

general_kellys_criterion_cumulative_return_series <- function(a, df_kelly, df_moving_average) {
  cumulative_return = c(1)
  for (i in 2:nrow(df_kelly)) {
    
    kellys_val = as.numeric(df_kelly[i-1,])
    avg = as.numeric(df_moving_average[i,])
    weight = c()
    
    for (j in 1:length(names)) {
      if (kellys_val[j] > a) {
        weight = c(weight, (1 / ncol(df_kelly)))
      } else {
        weight = c(weight, 0)
      }
    }
    
    latest_return = cumulative_return[length(cumulative_return)]
    append_return = latest_return * (1 + (t(avg) %*% weight))
    cumulative_return = c(cumulative_return, append_return)
  }
  return(cumulative_return)
}
