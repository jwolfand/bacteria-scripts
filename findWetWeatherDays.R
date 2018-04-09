# FUNCTION findWetWeatherDays
# J. Wolfand 7/5/2017
# This function finds wet weather days, as defined by the Ballona Creek TMDL:
# Days with > 0.1 inches of rain or the following three days after
# Inputs: precip = precipitation time series
# Outputs: wetDays = boolean time series of wet weather days - TRUE if wet
findWetWeatherDays <- function(precip) {
  wetDays = logical(length(precip))
  for (i in 1:length(precip)){
    if(precip[i] > 0.1){
      wetDays[i]<-TRUE
      wetDays[i+1]<-TRUE
      wetDays[i+2]<-TRUE
      wetDays[i+3]<-TRUE
    }
  }
  return(wetDays)
}