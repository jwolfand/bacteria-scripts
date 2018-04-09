#################################################
# FUNCTION Calculate removal
# J. Wolfand Written 6/29/17 Updated 7/14/17
# Purpose: Function to take log removal of concentration during storm events
# Inputs:
#   Cin = timeseries of concentration in
#   storm_timeseries = timeseries noting when storms are occuring
#   logRem = log removal
# Outputs:
#   Cout = concentration after removal

calculateRemoval <- function(Cin, storm_timeseries, logRem) {
  Cout <- Cin
  # Route both dry and wet weather through BMPs for removal
  Cout<-
    10 ^ (log10(Cin) - logRem)
  return (Cout)
}