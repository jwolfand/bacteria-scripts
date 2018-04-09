#################################################
# FUNCTION findStormsBMP
# J. Wolfand Written: 9/22/17
# Purpose: This function identifies when storm flow is coming out of a BMP
# Note: This was written for hourly flow, but could also be used for daily flow
# Input:
#   Qin = Inflow to the BMP
#   Qout = Outflow from the BMP
#   stormIn = Timeseries flagging when storms went in to the BMP
# Output:
#   stormOut = Timeseries flagging when storm flow comes out of the BMP

# TEST DATA, delete when done testing
# data = read.table("Input/test_findStormsBMP2.txt", header = T)
# Qin <- data$Qin
# Qout <- data$Qout
# stormIn <- data$StormIn

findStormsBMP <-
  function(Qin, Qout, stormIn) {
    start <- 41 #Start after 41 hours of run
    stormOut <- vector(mode = "logical", length = length(stormIn))
    count <- 0
    isStorm <- FALSE
    # Check that input timeseries are the same length
    if (length(Qin) != length(Qout) ||
        length(Qin) != length(stormIn)) {
      stop("Input timeseries are not the same length")
    }
    
    # Create stormOut timeseries
    for (i in start:length(stormIn)) {
      #   if (!(stormIn[i] == 0 && round(Qin[i],1) == round(Qout[i],1))) {
      #     if (stormOut[i - 1] == 0) {
      #       count <- count + 1
      #     }
      #     stormOut[i] <- count
      #   }
      # }
      
      # If you are in an identified storm event
      if (stormIn[i] != 0) {
        isStorm <- TRUE
        # Check to see if it's the beginning and update count
        if (stormOut[i - 1] == 0) {
          count <- count + 1
        }
        stormOut[i] <- count
      }
      # If yesterday was a storm event, see if it's still going
      else {
        if(stormOut[i-1]!=0 && (round(Qin[i],2) != round(Qout[i],2))){
          stormOut[i]<-count
        }
        
      }

    } # end for loop through timeseries

    # x <- data.frame(Qin,Qout,stormIn, stormOut)
    # write.table(x,
    #             file = paste("Output/", paste("Test_findStormsBMP", Sys.Date(), sep = "_"), ".txt", sep = ""),
    #             sep = "\t")
    return(stormOut)

  }