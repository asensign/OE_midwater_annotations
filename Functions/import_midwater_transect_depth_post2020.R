library(stringr)
import_midwater_transect_depth_post2020 <- function(filename) {
  
  dive_summary <- scan(filename, what = character(), skip = 3, sep="\n")
  
  transect_index <- str_which((dive_summary), 'Transect ')
  transect_depth_list2 <- list()
  
  for (i in length(transect_index)) {
    transect_number <- dive_summary[transect_index]
    depth1 <-dive_summary[transect_index+6]
    depth <-str_replace(depth1, "Depth:      ", "")
    
    transect_depth_list2[[length(transect_depth_list2) + 1]] <- depth
    
  }
  return(transect_depth_list2)
}