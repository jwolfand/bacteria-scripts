# FUNCTION checkSingleSampleTMDL_byWY
# J. Wolfand 9/5/2017
# This function determines whether or not a concentration time series meets
# the Ballona Creek TMDL. It is adapted from the checkSingleSampleTMDL script that was previously developed.
# It has been updated to report annual exceedances by water year.
# This function checks the number of TMDL exceedances for Ballona Creek
# Reach 2. This reach is designated for Limited Contact Recreation (LREC-1).
# The geometric mean (GM) standard is 126/100 mL. The single sample (SS)
# standard is 576/100 mL. 5 exceedances are allowable for dry weather days.
# 15 exceedances are allowable for wet-weather days. Wet-weather days are
# defined as days with >0.1" of rain and 3 days after. No exceedances are
# allowed for the geometric mean target.
# Inputs:
#   datetime = date datetime timeseries, daily
#   precip = precipitation time series, daily
#   conc = concentration timeseries, daily
# Outputs: data frame with three columns: water year, # wet day exceedances, # of dry day exceedances

# Establish dummy variables for testing code
# source("findWetWeatherDays.R")
# datetime <-
#   seq(as.POSIXct("1997-10-01 00:00:00"),
#       as.POSIXct("2015-09-30 23:00:00"),
#       by = "day")
# conc <- abs(rnorm(
#   n = length(datetime),
#   mean = 1000,
#   sd = 25
# ))
# precip_hourly <-
#   read.table(
#     "Modeled_precip_98_15.txt",
#     sep = "\t",
#     colClasses = c("NULL", "NULL", "numeric")
#   )
# precip_temp <- matrix(precip_hourly$V3, ncol = 24, byrow = TRUE)
# precip <- rowSums(precip_temp)
# conc <- read.table("Cout_170906.txt")
##############

checkSingleSampleTMDL_byWY <- function(datetime, precip, conc) {
  wetWeatherDays <- findWetWeatherDays(precip)
  tmdl.lim <- 576
  
  # Throw error if length of inputs are not equal
  try(if (length(datetime) != length(precip) &&
          length(datetime) != length(conc))
    stop("Input vectors are not the same length"))
  
  # Initialize exceedance tracker
  exceedance_tracker <-
    data.frame(wy = numeric(length = length(datetime)),
               ex = logical(length = length(datetime)))
  
  # Find the water year for each day
  exceedance_tracker$wy <-
    ifelse(as.numeric(format(datetime, "%m")) > 9,
           as.numeric(format(datetime, "%Y")) + 1,
           as.numeric(format(datetime, "%Y")))
  
  # Find whether the daily concentration is greater than the TMDL limit
  exceedance_tracker$ex <- conc >= tmdl.lim
  
  # Initialize output data frame for exceedances by water year
  exceedances <-
    data.frame(
      wy = unique(exceedance_tracker$wy),
      wet_weather = rep(NA, length(unique(
        exceedance_tracker$wy
      ))),
      dry_weather = rep(NA, length(unique(
        exceedance_tracker$wy
      )))
    )
  
  # Sum the number of exceedances during dry and wet weather for each water year
  for (i in 1:length(exceedances$wy)) {
    exceedances$wet_weather[i] <-
      sum(
        exceedance_tracker$wy == exceedances$wy[i] &
          wetWeatherDays == 'TRUE' & exceedance_tracker$ex == 'TRUE'
      )
    exceedances$dry_weather[i] <-
      sum(
        exceedance_tracker$wy == exceedances$wy[i] &
          wetWeatherDays == 'FALSE' &
          exceedance_tracker$ex == 'TRUE'
      )
  }
  
  return(exceedances)
} # End function