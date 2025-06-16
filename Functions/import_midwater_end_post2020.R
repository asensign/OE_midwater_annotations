#Scan individual dive summary .txt files and extract the midwater end time for
#dives conducted after 2020

import_midwater_end_post2020 <- function(filename) {
  dive_summary <- scan(filename, what = 'character', skip = 34, sep="")
  print(dive_summary[7])
  end_midwater <- as.POSIXct(dive_summary[7], tz="UTC",
                              format = "%H:%M:%OS")
  print(end_midwater)
}
