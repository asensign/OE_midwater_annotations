if(!require('purrr'))install.packages('purrr')
if(!require('dplyr'))install.packages('dplyr')
if(!require('tidyr'))install.packages('tidyr')
if(!require('stringr'))install.packages('stringr')
if(!require('ggplot2'))install.packages('ggplot2'); library('ggplot2')
if(!require('tidyverse'))install.packages('tidyverse'); library('tidyverse')
if(!require('lubridate'))install.packages('lubridate'); library('lubridate')
if(!require('stringr'))install.packages('stringr'); library('stringr')    
library(data.table)
library(tidyr)
library(dplyr)
library(data.table)
library(gdata)
library(hms)


function_names <- list.files(path = "C:/Users/Alexandra.Ensign/Documents/GitHub/OE_midwater_annotations/Functions",
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)

# expedition <- "EX1903L2" 
expedition <- "EX1806"
# expedition <- "EX2107"

wd <- paste0("C:/Users/Alexandra.Ensign/Documents/midwater_R_files/",expedition)
print(wd)
setwd(wd)

# set dive numbers depending on which expedition
if (expedition == "EX1806") {
  dive_number<-c(2,4,15)
  dive_names <- c("DIVE02", "DIVE04", "DIVE15")
} else if (expedition=="EX2107") {
  dive_number<-c(3,13)
  dive_names <- c("DIVE03", "DIVE13")
} else if (expedition=="EX1903L2") {
  dive_number<-c(2,6,8,9,14)
  dive_names <- c("DIVE02", "DIVE06", "DIVE08", "DIVE09", "DIVE14")
}
  
# read in cleaned annotations from midwater_annotations_cleaning
annotations_clean <- read.csv(paste0(wd,"/exports/midwater_annotations_",expedition,".csv"), header = TRUE)

# read in acoustics data
acoustics <- read.csv(paste0(wd,"/",expedition,"_echoview-output.csv"), header = TRUE)
# head(acoustics)

# reorder cols to make more sense (times first since we're binding by time)
acoustics <- select(acoustics, Process_ID, Interval, Layer, Date_S, Time_S, Date_E, Time_E, 
                    Lat_S, Lon_S, Lat_E, Lon_E, Ping_S, Ping_E, Dist_M,
                    Sv_mean, NASC, Height_mean, Depth_mean, Layer_depth_min, Layer_depth_max,
                    Noise_Sv_1m, Minimum_Sv_threshold_applied, Maximum_Sv_threshold_applied, 
                    Standard_deviation, Thickness_mean, Range_mean,
                    Exclude_below_line_range_mean, Exclude_above_line_range_mean, Center_of_mass, Inertia,
                    Proportion_occupied, Equivalent_area, Aggregation_index)

### convert dates and times in acoustics to be the same format as annotations (but going to keep the old cols for now)
# there is probably a better way to do this 
acoustics$Date_S <- as.POSIXct(strptime(acoustics$Date_S, format = "%Y%m%d", tz = "UTC"))
acoustics$Date_E <- as.POSIXct(strptime(acoustics$Date_E, format = "%Y%m%d", tz = "UTC"))
 
acoustics$Time_S <- as_hms(as.POSIXct(strptime(acoustics$Time_S, format = "%H:%M:%OS", tz = "UTC")))
acoustics$Time_E <- as_hms(as.POSIXct(strptime(acoustics$Time_E, format = "%H:%M:%OS", tz = "UTC")))

acoustics$Datetime_S <- with(acoustics, as.POSIXct(paste(Date_S, Time_S),
                                                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
acoustics$Datetime_E <- with(acoustics, as.POSIXct(paste(Date_E, Time_E),
                                                   format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
# head(acoustics)


### Subset annotations based on start and end times from acoustics
# create a new empty dataframe for the filtered annotations
annotations_bin_subset <- data.frame()

# use timestamps to filter for annotations that fall within Time_S and Time_E from the bins
# this takes a little while to run fyi

for(i in 1:nrow(annotations_clean)){                                              
  for (j in 1:nrow(acoustics)){                                              
    
    ann_time<-ymd_hms(annotations_clean$date_time[i])
    
    acoustic_start <- ymd_hms(acoustics$Datetime_S[j])                                 
    acoustic_end <- ymd_hms(acoustics$Datetime_E[j])
    
    if (ann_time > acoustic_start && ann_time < acoustic_end){                                 
      annotations_bin_subset <- rbind((annotations_clean[i,]), annotations_bin_subset)                         
    }
  }
}

# View(annotations_bin_subset)

### Now that the annotations are filtered, join them to the acoustics dataframe
# only pull over acoustics where there are annotations

test <-  dplyr::left_join(annotations_bin_subset, 
                                     acoustics,
                                     dplyr::join_by("date_time"=="Datetime_S"))
head(test)
# ROV_join <- arrange(ROV_join, Date)


# write.csv(transect_times, paste0(wd,"/exports/midwater_transect_times_", data_name, ".csv"),row.names = FALSE)
