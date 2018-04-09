#################################################
# FUNCTION transformFlow
# J. Wolfand 6/21/2017
# Purpose: Transform flow to a semi-log scale where transformed flow = ((flow + 1).^gamma -1)/gamma where gamma is 0.3
# Input: 
#   flow = timeseries of flow
# Output: 
#   transformedFlow = timeseries of transformed flow

transformFlow <- function(flow) {
  gamma <- 0.3;
  transformedFlow <- ((flow + 1)^gamma -1)/gamma
  return(transformedFlow)
}