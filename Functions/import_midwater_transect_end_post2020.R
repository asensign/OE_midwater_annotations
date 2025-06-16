library(stringr)
import_midwater_transect_end_post2020 <- function(filename) {
  dive_summary <- scan(filename, what = character(), skip = 3, sep="\n")
  transect_index <- str_which((dive_summary), 'Transect ')
  
  transect_end_list2 <- list()
  
  for (i in length(transect_index)) {
    transect_end <- str_replace(dive_summary[transect_index+3], "End:        ", "")
    transect_end_list2[[length(transect_end_list2) + 1]] <- transect_end
  }

  return(transect_end_list2)
}