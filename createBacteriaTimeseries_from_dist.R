#################################################
# FUNCTION Create bacteria timeseries from Distribution
# J. Wolfand 8/7/2017
# Purpose: Function to create bacteria timeseries
# Inputs:
#   datetime = date/time of timeseries
#   storm_timeseries = timeseries of when storm events occur, provided by findStorms
#   mu_w = mean of lognormal distribution for wet weather
#   sigma_w = standard deviation of lognormal distribution for wet weather
#   mu_d = mean of lognormal distribution for dry weather
#   sigma_d = standard deviation of lognormal distribution for dry weather
# Outputs:
#   bacteria_timeseries = timeseries of bacteria concentration
# Use findStorms function to find the storms
# For dry and wet weather pull from lognormal distribution
# Based on observed data at BCB-5, 
#   mu_w = 3.21328
#   sigma_w = 0.747
#   mu_d = 2.71036
#   sigma_d = 0.576922

createBacteriaTimeseries_fromDist <-
  function(datetime,
           storm_timeseries,
           mu_w, sigma_w,
           mu_d,
           sigma_d) {
    library("lubridate")
    library("truncnorm")
    
    set.seed(123)
    
     # Initialize concentration vector
    concentration <- vector(mode = "numeric", length = length(datetime))
    
    
    # For all days when storm time series = 0, concentration should be dry weather conc. from that month
    # concentration[storm_timeseries == 0] <-
    #   dry_weather[month(datetime[storm_timeseries == 0])]
     
    # Wet Weather: Assign EMC for each storm event
    for (i in 1:max(storm_timeseries)) {
      emc <-  10^(rnorm(1, mu_w, sigma_w))
      concentration[storm_timeseries == i] <- emc
    }
    
    # Dry Weather: Pick concentration from lognormal distribution
    for (j in 1:length(storm_timeseries)){
      if(storm_timeseries[j] == 0){
        concentration[j] <- 10^(rnorm(1,mu_d,sigma_d))
      }
    }
    
    return (concentration)
  }

