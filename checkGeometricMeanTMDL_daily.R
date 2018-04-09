# FUNCTION checkGeometricMeanTMDL
# C. Bell 7/14/17 Modified for daily data by J. Wolfand 8/3/17
# This function determines whether or not a concentration time series meets
# the geometric mean TMDL for Ballona Creek Reach 2. This reach is designated
# for Limited Contact Recreation (LREC-1).
# The geometric mean (GM) standard is 126/100 mL. No exceedances are
# allowed for the geometric mean target. The geometric mean is calculated weekly
# as a 6 week geometric average, starting on Sunday.
# Inputs:
#   datetime = datetime timeseries, daily
#   conc = concentration timeseries, daily
# Outputs: matrix (2 columns) with water year, and corresponding # of excceedances

#-----
# Establish dummy data for testing code -- DELETE THIS!!!
datetime <-
  seq(as.POSIXct("1997-10-01 00:00:00"),
      as.POSIXct("2015-09-30 23:00:00"),
      by = "day")
conc <- abs(rnorm(
  n = length(datetime),
  mean = 125,
  sd = 25
))
#-----

#-----
# Initialize two other parameters -- INSERT INTO MAIN CODE EVENTUALLY
ave_period <-
  1 * 60 * 60 * 24 * 7 * 6 #  Find numerical value of 6 weeks (in seconds)
tmdl_lim <- 126 # TMDL
#-----

checkGeometricMeanTMDL <-
  function(datetime, conc, tmdl_lim, ave_period) {
    # Conc is daily data
    daily.means <- conc
    
    # Initialize data frame with all the TS (easier for subsetting)
    dat <- data.frame(
      datetime = datetime,
      conc = conc,
      wy = ifelse(
        as.numeric(format(datetime, "%m")) > 9,
        as.numeric(format(datetime, "%Y")) + 1,
        as.numeric(format(datetime, "%Y"))
      ),
      dow = weekdays(datetime)
    )
    
    # Find the 7th Sunday (per TMDL)
    sunday.start <- subset(dat, dat$dow == "Sunday")$datetime[7]
    
    # Make data frame "exceedance_tracker" with "week" column being all sundays in between first sunday and last day of record
    exceedance_tracker <-
      data.frame(week = seq(
        from = sunday.start,
        to = tail(dat$datetime, 1),
        by = "1 week"
      ))
    # Initialize a column ot store geomtric mean
    exceedance_tracker$c_geomean <- NA
    
    
    # Loop through each week
    for (i in seq(1:length(exceedance_tracker$week))) {
      # Subset observations to 6 weeks prior to the sundays ("week" column)
      dat.tmp <-
        subset(
          dat,
          datetime <= exceedance_tracker$week[i] &
            datetime >= (exceedance_tracker$week[i] - ave_period)
        )
      
      # Compute 6-week geometric mean of all daily geometric mean values
      exceedance_tracker$c_geomean[i] <-
        (prod(dat.tmp$conc) ^ (1 / length(dat.tmp$conc)))
    }
    
    # Compare geometric mean to tmdl limit - create a flag if >than limit
    exceedance_tracker$exceed_fl <-
      ifelse(exceedance_tracker$c_geomean > tmdl_lim, 1, 0)
    
    # Add WY to exceedance tracker dataframe
    exceedance_tracker$wy <-
      ifelse(as.numeric(format(exceedance_tracker$week, "%m")) > 9,
             as.numeric(format(exceedance_tracker$week, "%Y")) + 1,
             as.numeric(format(exceedance_tracker$week, "%Y")))
    
    
    # Create data frame of exceedances per water year
    exceedances <-
      data.frame(
        wy = unique(dat$wy),
        exceedances = rep(NA, length(unique(dat$wy))),
        num_weeks = rep(NA, length(unique(dat$wy)))
      )
    
    # Compute number of exceedances in each water year, and the number of analyzed in that WY
    for (i in seq(1:length(exceedances$wy))) {
      tracker.tmp <- subset(exceedance_tracker, wy == exceedances$wy[i])
      exceedances$exceedances[i] <- sum(tracker.tmp$exceed_fl)
      exceedances$num_weeks[i] <- length(tracker.tmp$exceed_fl)
    }
    
    return(exceedances)
  }
