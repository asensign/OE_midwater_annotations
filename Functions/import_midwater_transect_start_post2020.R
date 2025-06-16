library(stringr)
import_midwater_transect_start_post2020 <- function(filename) {
  dive_summary <- scan(filename, what = character(), skip = 3, sep="\n")
  transect_index <- str_which((dive_summary), 'Transect ')
  
  transect_start_list2 <- list()
  
  for (i in length(transect_index)) {
    
    transect_start1 <- dive_summary[transect_index+1]
    transect_start <- str_replace(transect_start1, "Start:      ", "")
    print(transect_start)
    
    transect_start_list2[[length(transect_start_list2) + 1]] <- transect_start
  }
#print(transect_start_list2)
return(transect_start_list2)
}