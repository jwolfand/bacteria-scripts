#################################################
# FUNCTION findStorms
# J. Wolfand Written: 6/21/2017, Modified: 6/29/2017
# Purpose: Uses the Hewlett-Hibbert method to identify and tally storm events
# Note: This was written for hourly flow, but could also be used for daily flow
# Input:
#   input_df = data frame with the datetime, precip, and flow
#   hhm_base = Hewlett-Hibbert base slope (recommended 0.05 [cfs / mi^2 / hr] )
#   event_threshold = event threshold, anything under this amount (in inches) is not considered a storm
#   added_end_timestep = arbitrarily have the storm end several timesteps after the identified end. This is
#     so the falling limb of the storm is captured. Note that the function automatically starts the storm
#     one timestep before its identified.
# Outputs (list containing):
#   storm_timeseries = timeseries vector with storm events labeled by number. Non-storms are zeros.
#   storm_stats = Allist of storms and their stats (start time, end time, cumulative volume, cumulative precip)

findStorms <-
  function(input_df,
           hhm_base,
           event_threshold,
           added_end_timestep) {
    # input_df is a data frame with the datetime, precip, and flow
    colnames(input_df) <- c("datetime", "precip", "flow")
    
    # DEFINE VARIABLES
    slope <- vector(mode = "numeric")
    storm_timeseries <- numeric()
    num <- numeric()
    start <- as.POSIXct(character())
    stop <- as.POSIXct(character())
    event_precip <- numeric()
    event_volume <- numeric()
    
    # DEFINE CONSTANTS
    watershed_area <- 130 # sq mi
    hhm <- hhm_base * watershed_area / 3600 #cfs/s
    len <- length(input_df$datetime)
    
    # Set storm counter
    noStorm <- 0 #counter for no. storms in timeseries
    isStorm <- FALSE
    stormDur <-
      0 #cumulative duration of each storm event in timesteps
    
    # Calculate slope of flow time series (subtract next entry from current entry and divide by 3600 s)
    for (i in 1:len) {
      slope[i + 1] <- (input_df$flow[i + 1] - input_df$flow[i]) / 3600
    }
    
    #Since first entry is NA, make it equal to zero
    slope[1] <- 0
    
    # For every flow data point
    for (i in 1:len) {
      
      # If you're not in a storm event already
      if (isStorm == FALSE) {
        
        # Check to see if storm has started
        if (slope[i] > hhm) {
          # Storm starts
          isStorm <- TRUE
          noStorm <- noStorm + 1
          stormDur <- 1
          storm_timeseries[i] <- noStorm
          storm_timeseries[i - 1] <-
            noStorm # start the storm one timestep before it's identified
          num[noStorm] <- noStorm
          start[noStorm] <-
            input_df$datetime[i - 1] # mark the start time as one timestep before it's identified
          event_precip[noStorm] <-
            storm_df$precip[i - 1] + storm_df$precip[i] # totals precip as one timestep before its identified
          event_volume[noStorm] <-
            storm_df$flow[i - 1] + storm_df$flow[i] # totals flow as one timestep before its identified
        }
        
        # It's not storming
        else {
          # Check to see if storm timeseries is NA. It would only not be NA if a storm event has just stopped
          if (is.na(storm_timeseries[i])) {
            storm_timeseries[i] <- 0
          }
        }
      }
      
      # If you are in a storm event already
      else if (isStorm == TRUE) {
        
        # Check to see if storm has ended
        if ((stormDur * hhm * 3600) > input_df$flow[i]) {
          #Criteria for ending storm; hhm line intersects flow timeseries
          # Storm ends
          isStorm <- FALSE
          storm_timeseries[i] <- noStorm
          
          # end the storm added_end_timestep after identified end
          for (k in 0:added_end_timestep) {
            storm_timeseries[i + k] <- noStorm
            stop[noStorm] <- input_df$datetime[i + k]
          }
          
          # If event volume is <0.1 inches, then look back and get rid of storm event
          if (event_precip[noStorm] < event_threshold) {
            duration <-
              stormDur + 1 + added_end_timestep #because started storm 1 timestep before and ended added_end_timestep after
            
            for (j in 0:duration) {
              storm_timeseries[(i + added_end_timestep) - j] <- 0
            }
            
            # Remove 1 from storm count
            noStorm <- noStorm - 1
          }
          
        }
        else {
          # Storm continues
          stormDur = stormDur + 1
          storm_timeseries[i] <- noStorm
          event_precip[noStorm] <-
            event_precip[noStorm] + storm_df$precip[i]
          event_volume[noStorm] <-
            event_volume[noStorm] + storm_df$flow[i]
        }
      }
    }
    
    storm_stats <-
      data.frame(num, start, stop, event_precip, event_volume)
    return_list <-
      list("Storm timeseries" = storm_timeseries, "Storm stats" = storm_stats)
    return(return_list)
    
  }