library(stringr)
import_midwater_transect_duration_post2020 <- function(filename) {
  dive_summary <- scan(filename, what = character(), skip = 3, sep="\n")

  transect_index <- str_which((dive_summary), 'Transect ')
  
  for (i in length(transect_index)) {
    duration1 <-dive_summary[transect_index+5]
    duration <-str_replace(duration1, "Duration:   ", "")
  }
}