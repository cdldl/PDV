library(Rcpp)
library(RcppEigen)
library(data.table)
library(RcppParallel)
library(RQuantLib)
library(fasttime)
library(anytime)

# Load functions 
sourceCpp('/home/cyril/Desktop/Coding/kernelcapital/VolatilityIsMostlyPathDependent/pdv_4factor2.cpp')

# Get option data
sub_options = fread('/home/cyril/Desktop/Coding/kernelcapital/VolatilityIsMostlyPathDependent/two_underlyings_sub.csv')

# Define constants
timestep_per_day = 1/252 * 5
maturity = 0.25
n_timesteps  = as.integer(maturity/timestep_per_day)
path_parameters  = '/home/cyril/PDV/parameters/'
dt = 1/252
max_delta = 1000

# Get parameters: beta0, beta1, beta2, lambda10, lambda11, theta1, lambda20, lambda21, theta2
parameters = fread('/home/cyril/Desktop/Coding/kernelcapital/VolatilityIsMostlyPathDependent/GOOGL.csv')
parameters = c(parameters$V2[2:length(parameters$V2)], 0, 0)
parameters2 = fread('/home/cyril/Desktop/Coding/kernelcapital/VolatilityIsMostlyPathDependent/AMZN.csv')
parameters2 = c(parameters2$V2[2:length(parameters2$V2)], 0, 0)

# Load data
data = fread('/home/cyril/Desktop/Coding/kernelcapital/VolatilityIsMostlyPathDependent/GOOGL.US.csv')
data[,para:=list(parameters)]
data[,ticker:='GOOGL']
data2 = fread('/home/cyril/Desktop/Coding/kernelcapital/VolatilityIsMostlyPathDependent/AMZN.US.csv')
data2[,para:=list(parameters2)]
data2[,ticker:='AMZN']
data = rbind(data,data2)

# Initialize returns and vol
exp_kernel_GPU <- function(t, lam, c=1) {
  return(c * lam * exp(-lam * t))
}

initialize_R <- function(lam, past_prices, max_delta=1000, transformation='identity') {
  returns = past_prices$returns
  returns <- rev(tail(returns, max_delta))
  timestamps <- seq(length(returns)) * dt
  weights <- exp_kernel_GPU(timestamps, lam[1])
  weights2 <- exp_kernel_GPU(timestamps, lam[2])
  result = c(sum(returns * weights),sum(returns * weights2))
  result2 = c(sum(returns^2 * weights),sum(returns^2 * weights2))
  if(transformation == 'identity') return(result) else return(result2)
}
data[,returns:=(Open - shift(Open,1, type='lag')) / shift(Open,1, type='lag')]
data[,R_init1:=list(list(initialize_R(para[[1]][4:5],.SD[Date >= "2015-01-01" & Date < '2020-01-01'],
                            max_delta,transformation="identity"))),by=ticker]
data[,R_init2:=list(list(initialize_R(para[[1]][7:8],.SD[Date >= "2015-01-01" & Date < '2020-01-01'],
                                      max_delta,transformation="squared"))),by=ticker]

n_paths = 1000
timestep_per_day = 5
data[,date:=Date]
options = merge(sub_options,data,by=c('ticker','date'))
options2 = options[date==date[1]]
options2[,for_bid:=(oracle_ret * askPrice) + askPrice]

# Compute Barone Adesi Whaley 
PDV_baroneAdesiWhaley = function(data) {
  # data = copy(options2)
  
  # Simulate PDV Paths
  sim_paths = data[,list(sim=list(simulate_cpp(n_paths, underlyingClose[1],
                                               para[[1]], R_init1[[1]], R_init2[[1]],
                  expected_maturity[1], timestep_per_day))),by=list(date, ticker, expected_maturity)] 
  
  # FIXME
  # data[,optionPrices:=computeBaroneAdesiWhaleyOptionPriceParallel(
  # sim_paths[sim_paths$ticker == ticker[1] & sim_paths$date == date[1] &
  # sim_paths$expected_maturity == expected_maturity[1]]$sim[[1]]$S_array[nrow(sim_paths$sim[[1]]$S_array),],
  #                                                 strike,
  #                                                 0., #riskFree rate
  #                                                 0., #dividendYield
  # sim_paths[sim_paths$ticker == ticker[1] & sim_paths$date == date[1] &
  # sim_paths$expected_maturity == expected_maturity[1]]$sim[[1]]$vol_array[nrow(sim_paths$sim[[1]]$vol_array),],
  #                                                 expected_maturity,
  #                                                 fifelse(type=='call',T,F)),
  #    by=list(date,ticker,expected_maturity)]
  
  computeBaroneAdesiWhaleyOptionPrice = function(spot,
                                                 strike,
                                                 riskFreeRate,
                                                 dividendYield,
                                                 volatility,
                                                 timeToExpiry,
                                                 call_put) {
    option_prices = vector(length=length(spot))
    for(i in seq(length(spot))) {
      # i = 1
      option_prices[i] = AmericanOption(call_put, spot[i], strike, 
                                                 dividendYield, riskFreeRate, 
                                                 timeToExpiry, volatility[i])$value  
    }
    mean_option = mean(option_prices,na.rm=T)
    mean_option
  }
  data[,optionPrices:=computeBaroneAdesiWhaleyOptionPrice(
    sim_paths[sim_paths$ticker == .SD$ticker[1] & sim_paths$date == .SD$date[1] &
                sim_paths$expected_maturity == .SD$expected_maturity[1]]$sim[[1]]$S_array[timestep_per_day,],
    strike,
    iRate_mat[1],
    0., #dividendYield
    sim_paths[sim_paths$ticker == ticker[1] & sim_paths$date == date[1] &
                sim_paths$expected_maturity == expected_maturity[1]]$sim[[1]]$vol_array[timestep_per_day,],
    expected_maturity,
    type),
    by=seq.int(nrow(data))]
  data
}

data = PDV_baroneAdesiWhaley(options2)

data[,c('for_bid','optionPrices')]

# FIXME: too slow / Need to test speed against BaroneAdesiWhaley engine 
if(F) {
  # Need to replicate this:
  options2[,option_prices:=PDV_LS(n_paths, underlyingClose[1], parameters[[1]], 
                                  R_init1[[1]], R_init2[[1]], expected_maturity[1], 
                                  strike,  iRate_mat[1], 5, 2)
           ,by=list(date, ticker, expected_maturity)]
  
  
  
  # Generate sample input data
  set.seed(123)
  n_paths <- 10
  timestep_per_day <- 10
  
  yo = simulate_cpp(n_paths, 1, parameters, R_init1, R_init2, maturity, timestep_per_day) 
  
  # Simulated paths: (n_paths, n_timesteps)
  paths <- t(yo$S_array)
  
  dim(paths)
  # Simulated cashflows
  cashflows <- compute_cash_flows(paths, maturity, strike=c(1), risk_free_rate = 0.001) 
  
  paths[,1]
  cashflows[[1]][,1]
  # Call the Rcpp function
  degree = 2
  continuation_values_cpp <- longstaff_schwartz_cpp(paths, cashflows[[1]],degree)
  
  # Price each option by list(ticker,expected_maturity)
  
  options_prices = mean(continuation_values_cpp[,ncol(continuation_values_cpp)])
  options_prices
  
  continuation_values_cpp[,ncol(continuation_values_cpp)]
  continuation_values_r[,ncol(options_prices_r)]
  identical(continuation_values_cpp, continuation_values_r)
  
}