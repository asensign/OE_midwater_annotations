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

# split the annotations date_time column into date and time
annotations_clean$Date_S <- as.Date (annotations_clean$date_time)
annotations_clean$Time <- format((annotations_clean$date_time),format = "%H:%M:%S") ### check
annotations_clean$Time_S <- as.POSIXct(annotations_clean$Time, tz = "UTC") # rename this depending on if you use T_S, T_E, or T_M from the acoustics
# head(annotations_clean)

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

# convert dates and times to utc yayyyy
acoustics$Date_S <- as.POSIXct(strptime(acoustics$Date_S, format = "%Y%m%d", tz = "UTC"))
acoustics$Date_E <- as.POSIXct(strptime(acoustics$Date_E, format = "%Y%m%d", tz = "UTC"))

acoustics$Time_S <- as.POSIXct(acoustics$Time_S, format = "%H:%M:%OS", tz = "UTC")
acoustics$Time_E <- as.POSIXct(acoustics$Time_E, format = "%H:%M:%OS", tz = "UTC")
# head(acoustics)


# subset annotations based on start and end times from acoustics


### bind the annotations and the acoustics by UTC

# bind just the times first?


# test <- cbindX(annotations_clean, acoustics) # literally just smushes them together

# now need to merge times and sort by time

# head(test)


  