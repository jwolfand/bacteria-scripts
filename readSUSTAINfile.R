# FUNCTION readSUSTAINFile
# J. Wolfand 6/23/2017 Revised 7/5/2017
# Reads the total inflow, outflow, and infiltration from a SUSTAIN .out file, return in a list
readSUSTAINfile <- function(file) {
  output = read.table(
    paste(file),
    colClasses = c(
      rep("NULL", 3),
      "numeric",
      "numeric",
      "NULL",
      "numeric",
      "NULL",
      "numeric",
      rep("NULL",3),
      "numeric",
      rep("NULL", 7)
    ),
    sep = "\t",
    comment.char = "T",
    col.names = c(
      "BMP/Date/time",
      "Volume",
      "Stage",
      "Inflow_t",
      "Outflow_w",
      "Outflow_o",
      "Outflow_ud",
      "Outflow_ut",
      "Outflow_t",
      "Infiltration",
      "Perc",
      "AET",
      "Seepage",
      "Mass_in_1",
      "Mass_w_1",
      "Mss_o_1",
      "Mass_ud_1",
      "Mass_ut_1",
      "Mass_out_1",
      "Conc_1"
    )
  )
  return(output)
}