#Tests for package availability and installs missing packages that are needed
#in order to run this code:
if(!require('dplyr'))install.packages('dplyr')
if(!require('lubridate'))install.packages('lubridate')
if(!require('geosphere'))install.packages('geosphere')
if(!require('leaflet'))install.packages('leaflet')
if(!require('stringr'))install.packages('stringr')
if(!require('TTR'))install.packages('TTR')
if(!require('ggplot2'))install.packages('ggplot2')

function_names <- list.files(path = "C:/Users/Alexandra.Ensign/Documents/GitHub/OE_midwater_annotations/Functions", 
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)
#-------------------------------------------------------------------------------
# expedition <- "EX2107"
expedition <- "EX1806"
# expedition <- "EX1903L2"

ROV_filepath <- paste0("C:/Users/Alexandra.Ensign/Documents/midwater_R_files/",expedition,"/ROV_tracks/")
wd <- paste0("C:/Users/Alexandra.Ensign/Documents/midwater_R_files/",expedition)
setwd(wd)
transect_times_wd <- paste0("C:/Users/Alexandra.Ensign/Documents/midwater_R_files/", 
                            expedition, "/exports/")
#-------------------------------------------------------------------------------
transect_times <- read.csv(paste0(transect_times_wd,"midwater_transect_times_",expedition,".csv"), header = TRUE)
print(transect_times)

# dive number
i = "02"
# # transect number
# k = 1

# reads in rov stuff
ROV_import_df <- ROV_import(paste0(ROV_filepath,expedition,"_DIVE",i,"_ROVtrack.csv"))
ROV_clean_df <- ROV_clean(ROV_import_df)
# print(ROV_clean_df)

ROV_transects <- data.frame()
# filter for ROV times (unix) within transect times (also unix)

for(m in 1:nrow(ROV_clean_df)){                                             
  # for (n in 1:nrow(transect_times)){                                              
  for (n in 1:1){  
    ROV_time<-ROV_clean_df$unix_time[m]
    t_start<- transect_times$start_unix[n]                                 
    t_end<- transect_times$end_unix[n]
    
    if (ROV_time > t_start && ROV_time < t_end){                                 
      ROV_transects <- rbind((ROV_clean_df[m,]), ROV_transects)                         
    }
  }
}
# View(ROV_transects) #  worked to here

print(transect_times)
# Trying to join the transect_times df to the ROV_clean_df by unix time
ROV_join <- dplyr::left_join(ROV_clean_df, transect_times,
                             dplyr::join_by("unix_time" == "start_unix"))

# transect_times$start_UTC <- as.POSIXct(transect_times$start_UTC, tz = "UTC")  # trying with UTC also does not work
# ROV_join <- dplyr::left_join(ROV_clean_df, transect_times,
#                              dplyr::join_by("UTC" == "start_UTC")) 

View(ROV_join)





