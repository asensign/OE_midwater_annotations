import_midwater_start_post2020 <- function(filename) {
  dive_summary <- scan(filename, what = 'character', skip = 3, sep="")
  start_midwater <- as.POSIXct(dive_summary[2], tz="UTC", format = "%H:%M:%OS")
  print(start_midwater)
}