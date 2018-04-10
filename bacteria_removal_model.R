#------------------------------------------------------------------------------
# SCRIPT bacteria_removal_model
# J. Wolfand, adapted from script with date 11/14/17.
# Purpose: to predict bacteria removal in stormwater BMPs in a large watershed.
# Read in file to get precip and datetime
# Read SUSTAIN file to get Qin and Qout as well as totals
# Read in SUSTAIN data to get Qin and Qout from a single BMP (bioretention)
# Find storm events [FUNCTION = findStorms]
# Create concentration timeseries to get Cin [FUNCTION = createBacteriaTimeseries]
# Calculate removal within BMPs [FUNCTION = calculateRemoval]
# Check to see annual single sample TMDL exceedances [FUNCTION = checkSinglesampleTMDL_byWY]
# BMP flow is in Junction 1, bypassed flow in Junction 4, and total flow in Junction 2
# Cin is the concentration going into BMPs and bypassing BMPs
# CoutBMPs is the concentration coming out of BMPs
# QinBMPs is the flow going into BMPs
# Qinbypass is the flow bypassing BMPs
# Input files needed: precipitation time series with datetime, SUSTAIN files with BMP flow time series 
#---------------------------------------------------------------------------------

ptm <- proc.time()
source("readSUSTAINfile.R")
source("findStorms.R")
source("findStormsBMP.R")
source("createBacteriaTimeseries_from_dist.R")
source("transformFlow.R")
source("calculateRemoval.R")
source("findWetWeatherDays.R")
source("checkSinglesampleTMDL_byWY.R")
source("checkGeometricMeanTMDL_daily.R")
library(ggplot2)

# USER MODIFIED CONSTANTS ----------------------------------------------------------
output.file <- "noinf"
saveToFile <- TRUE
dirList = c(# "../SUSTAIN_files/1709/logrem_model_0BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_100BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_500BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_1000BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_2500BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_5000BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_7500BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_10000BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_25000BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_50000BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_75000BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_100000BR-noinf_0pct/",
  # "../SUSTAIN_files/1709/logrem_model_0BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_100BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_500BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_1000BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_2500BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_5000BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_7500BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_10000BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_25000BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_50000BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_75000BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_100000BR-noinf_100pct/",
  # "../SUSTAIN_files/1709/logrem_model_0BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_100BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_500BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_1000BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_2500BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_5000BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_7500BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_10000BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_25000BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_50000BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_75000BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_100000BR-noinf_95pct/",
  # "../SUSTAIN_files/1709/logrem_model_0BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_100BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_500BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_1000BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_2500BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_5000BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_7500BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_10000BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_25000BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_50000BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_75000BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_100000BR-noinf_90pct/",
  # "../SUSTAIN_files/1709/logrem_model_0BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_100BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_500BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_1000BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_2500BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_5000BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_7500BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_10000BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_25000BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_50000BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_75000BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_100000BR-noinf_75pct/",
  # "../SUSTAIN_files/1709/logrem_model_0BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_100BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_500BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_1000BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_2500BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_5000BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_7500BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_10000BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_25000BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_50000BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_75000BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_100000BR-noinf_50pct/",
  # "../SUSTAIN_files/1709/logrem_model_0BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_100BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_500BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_1000BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_2500BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_5000BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_7500BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_10000BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_25000BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_50000BR-noinf_25pct/",
  # "../SUSTAIN_files/1709/logrem_model_75000BR-noinf_25pct/",
  "../SUSTAIN_files/1709/logrem_model_100000BR-noinf_25pct/")

datetime <-
  seq(as.POSIXct("1997-10-01 00:00:00"),
      as.POSIXct("2015-09-30 23:00:00"),
      by = "hour")
datetime_daily <-
  seq(as.POSIXct("1997-10-01 00:00:00"),
      as.POSIXct("2015-09-30 23:00:00"),
      by = "day")
logRem <- c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3)
#logRem <- 0

# MAIN SCRIPT -----------------------------------------------------------------------
# Read in precipitation data; hourly
precip <-
  read.table(
    "Input/Modeled_precip_98_15.txt",
    sep = "\t",
    colClasses = c("NULL", "NULL", "numeric")
  )
# Get sum of hourly precip for daily precip
precip_temp <- matrix(precip$V3, ncol = 24, byrow = TRUE)
precip_daily <- rowSums(precip_temp)

# The SUSTAIN output files of interest
BMP_filename <-  "Init_BioRetentionBasin_1_4"
bypass_filename <- "Init_Junction_4"

# MONTE CARLO MODEL ADDITION - Uncomment to run monte carlo
#iter <-10
iter <- 1
monte.results <- matrix(NA, nrow = iter, ncol = 3)
# Loop for multiple rns to make stochastic model

#for (w in 1:iter){
#  print(paste ("Iteration #",w))
# Initialize results data frame

results <-
  data.frame(
    pct.rtd = numeric(),
    SCM = numeric(),
    num.SCMs = numeric(),
    log.rem = numeric(),
    wet.exceedances = numeric(),
    dry.exceedances = numeric(),
    GM.exceedances = numeric(),
    mass.load = numeric()
  )


# Initialize data frame for flow in BMP
QBMP <- data.frame(matrix(ncol = 0, nrow = 0))
MoutBMP <- vector(mode = "numeric", length = length(datetime))
Cout_raw <- vector(mode = "numeric", length = length(datetime))
SS_exceedances <-
  matrix(nrow = length(logRem), ncol = 2) # wet weather & dry weather exceedances
GM_exceedances <-
  vector(mode = "numeric", length = length(logRem))
mass <- vector(mode = "numeric", length = length(logRem))


# Loop for multiple SUSTAIN runs, based on dirList

for (d in 1:length(dirList)) {
  print (paste("Computing exceedances for ", dirList[d]))
  
  # Info for plotting in ggplot. Determine the SUSTAIN scenario that is run. Find number of SCMs, SCM type, and percent routed
  scenario <- sapply(strsplit(dirList[d], "_"), "[[", 4)
  scenario_numSCMS <- gsub("[^[:digit:]]", "", scenario)
  scenario_SCMtype <- gsub("[[:digit:]]", "", scenario)
  pct <- sapply(strsplit(dirList[d], "_"), "[[", 5)
  pct <- gsub("[^[:digit:]]", "", pct)
  
  # Read in SUSTAIN files, put inflow and outflow in their own matrices, each column is for a different BMP
  QBMP <-
    readSUSTAINfile(paste(dirList[d], BMP_filename, ".out", sep = ""))
  colnames(QBMP) <- c("inflow", "weir", "und", "out", "seep")
  # To account for QBMP$out not including seepage through media
  QBMP$out <- QBMP$weir + QBMP$und + QBMP$seep
  
  # Read in bypass flow (flow not going to any BMPs) from SUSTAIN file
  import <-
    readSUSTAINfile(paste(dirList[d], bypass_filename, ".out", sep = ""))
  Qbypass <- import[, "Inflow_t"]
  
  # Sum flow from BMPs and bypass flow into total flow
  QinTot <- QBMP$inflow + Qbypass
  QoutTot <- QBMP$out + Qbypass
  
  # create data frame with datetime | precip | flow
  storm_df = data.frame(datetime, precip$V3, QinTot)
  colnames(storm_df) <- c("datetime", "precip", "flow")
  
  # Call function findStorms to find the storm events given the datetime, total flow, and precip timeseries
  hhm <- 0.05
  event_threshold <- 0.005
  post_timestep <- 2
  output <-
    findStorms(storm_df, hhm, event_threshold, post_timestep)
  stats <- output$`Storm stats`
  timeseries_in <- output$`Storm timeseries`
  
  # Call function findStormsBMP to find the storms coming out of the BMPs
  timeseries_out <-
    findStormsBMP(QBMP$inflow, QBMP$out, timeseries_in)
  
  # CALCULATE CONCENTRATION GOING INTO BMPS
  # Cin is defined by calling createBacteriaTimeseries on the total of Qin, storm timeseries,
  # Cin is the same for each of the BMPs
  Cin <-
    createBacteriaTimeseries_fromDist(datetime, timeseries_in, 3.21, 4.58, 2.71, 2.78)
  write.table(Cin, "Input/simulated_FIB_timeseries.txt", sep = "\t")
  
  # CALCULATE MASS IN AND OUT OF BMPS
  MinBMP <- QBMP$inflow * Cin * 283.168 # units of MPN/s
  Mbypass <- Qbypass * Cin * 283.168 # units of MPN/s
  MinTot <- MinBMP + Mbypass
  
  # CALCULATE CONCENTRATION GOING OUT OF BMPS WITH NO LOG REMOVAL
  # BMP results in spread of inflow hydrograph, conserve mass by spreading over entire storm hydrograph
  # Loop from 1 to length of max number in timeseries_out
  # Create a dataframe for all storm events coming out. sum mass over each event, sum flow over each event, calc conc out
  len <- max(timeseries_out)
  storms_out <-
    data.frame(mass = numeric(len),
               flow = numeric(len),
               conc = numeric(len))
  
  for (m in 1:len) {
    storms_out$mass[m] <- sum(MinBMP[timeseries_out == m])
    storms_out$flow[m] <- sum(QBMP$out[timeseries_out == m])
    storms_out$conc[m] <-
      storms_out$mass[m] / storms_out$flow[m] / 283.168
  }
  
  # Distribute new EMC for each event coming out
  for (p in 1:length(timeseries_out)) {
    if (timeseries_out[p] == 0) {
      Cout_raw[p] <- Cin[p]
    }
    else{
      Cout_raw[p] <- storms_out$conc[timeseries_out[p]]
    }
  }
  
  # LOOP FOR DIFFERENT LOG REMOVALS
  for (j in 1:length(logRem)) {
    # CALCULATE CONCENTRATION GOING OUT OF BMPs
    # Cout is calculated for each of the BMPs with a function calculateRemoval
    
    # If there are more than zero BMPs
    if (scenario_numSCMS != 0) {
      CoutBMP <- calculateRemoval(Cout_raw, timeseries_in, logRem[j])
      # Note that the total mass is the mass treated within the BMP (flows through underdrain) plus
      # the mass untreated by the BMP due to overflow (flows over weir)
      MoutBMP <-
        (QBMP$und * CoutBMP + QBMP$weir * Cout_raw) * 28316.8 / 100
    }
    
    # If there are zero BMPs
    else{
      CoutBMP <- Cin
      MoutBMP <- (QBMP$inflow * Cin) * 283.168
      QoutTot <- QinTot
    }
    
    # CALCULATE MOUTTOT
    MoutTot <- MoutBMP + Mbypass
    
    # CALCULATE COUT/CIN TOTAL # Updated 9/14/17 to include weir overflow from bmps
    CoutTot <- MoutTot / QoutTot / 283.168
    CinTot <- MinTot / QinTot / 283.168
    
    # Make all nans where zero was divided by zero, to zero
    is.nan.data.frame <- function(x)
      do.call(cbind, lapply(x, is.nan))
    CoutTot[is.nan(CoutTot)] <- 0
    
    # CALCULATE TMDL EXCEEDANCES
    # calculated Cin and Cout based on hourly data, but need to know daily for TMDL exceedances.
    
    Cin_temp <- matrix(Cin, ncol = 24, byrow = TRUE)
    Cout_temp <- matrix(CoutTot, ncol = 24, byrow = TRUE)
    
    # Take the geometric mean of 24 hours within a day to aggregate to daily
    # Cin_daily <- 10 ^ (rowMeans(log10(Cin_temp)))
    # Cout_daily <- 10 ^ (rowMeans(log10(Cout_temp)))
    
    # Take the 10 am sample to aggregate to daily
    Cin_daily <- Cin_temp[, 10]
    Cout_daily <- Cout_temp[, 10]
    
    # Calculate single sample exceedances, geometric mean exceedances, and total mass load for each scenario
    # SS_exceedances[j,] <-
    
    q <-
      checkSingleSampleTMDL_byWY(datetime_daily, precip_daily, Cout_daily)
    SS_exceedances[j, 1] <- mean(q$wet_weather)
    SS_exceedances[j, 2] <- mean(q$dry_weather)
    
    ave_period <-
      1 * 60 * 60 * 24 * 7 * 6 #  Find numerical value of 6 weeks (in seconds)
    tmdl_lim <- 126 # Geometric mean TMDL
    temp_exceedances <-
      checkGeometricMeanTMDL(datetime, Cout_daily, tmdl_lim, ave_period)
    GM_exceedances[j] <- sum(temp_exceedances$exceedances)
    mass[j] <- sum(MoutTot)
    
    # Add result for wet weather, GM exceedances same for wet/dry weather
    results[nrow(results) + 1, ] <-
      list(
        as.numeric(pct),
        scenario_SCMtype,
        as.numeric(scenario_numSCMS),
        as.character(logRem[j]),
        SS_exceedances[j, 1],
        SS_exceedances[j, 2],
        GM_exceedances[j],
        mass[j]
      )
  } # end loop through log removals
} # end loop through different file names

if (saveToFile) {
  write.table(results,
              file = paste(
                "Output/",
                paste(output.file, Sys.Date(), sep = "_"),
                ".txt",
                sep = ""
              ),
              sep = "\t")
  
  # Uncomment to create a data.frame of results
  # x <- data.frame(QBMP, timeseries_in, timeseries_out, Cin, Cout_raw, precip)
  # write.table(x,
  #             file = paste("Output/", paste("Timeseries", Sys.Date(), sep = "_"), ".txt", sep = ""),
  #             sep = "\t")
}

# Print out run time
proc.time() - ptm

# Uncomment for Monte Carlo simulations
#monte.results[w,] <- results$wet.exceedances
#} # end for loop for multiple iterations
