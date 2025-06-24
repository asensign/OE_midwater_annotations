#Tests for package availability and installs missing packages that are needed
#in order to run this code:
if(!require('dplyr'))install.packages('dplyr')
if(!require('lubridate'))install.packages('lubridate')
if(!require('geosphere'))install.packages('geosphere')
if(!require('leaflet'))install.packages('leaflet')
if(!require('stringr'))install.packages('stringr')
if(!require('TTR'))install.packages('TTR')
if(!require('ggplot2'))install.packages('ggplot2')
library(data.table)
library(tidyr)

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

# dive number -- eventually this will be a loop (START IT HERE)
# this is going to drive me insane later
i = "02"
ii='2'


# reads in rov stuff
ROV_import_df <- ROV_import(paste0(ROV_filepath,expedition,"_DIVE",i,"_ROVtrack.csv"))
ROV_clean_df <- ROV_clean(ROV_import_df)
# print(ROV_clean_df)

ROV_transects <- data.frame()

# filter for ROV times (unix) within transect times (also unix)

#only do one dive at a time from transect_times (they're lumped together by EX)
transect_times <- filter(transect_times, dive_number==sprintf("%s",ii))

for(m in 1:nrow(ROV_clean_df)){                                             
  for (n in 1:nrow(transect_times)){                                              
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
print(transect_times$start_unix[1])
print(ROV_transects$unix_time[1])
# print(transect_times$start_UTC[1])
# print(ROV_transects$UTC[1])

# add the EXACT 13-digit Unix times of transect start/end to the ROV_transect dataframe

# add empty rows at the end
print(nrow(ROV_transects))
ROV_transects[nrow(ROV_transects) + (nrow(transect_times)), ] <- NA
print(nrow(ROV_transects))

# add transect start and end times to those empty rows as unix times
start_rows <- data.frame(transect_times$start_unix)
end_rows <- data.frame(transect_times$end_unix)
setnames(start_rows, "transect_times.start_unix", "unix_time")
setnames(end_rows, "transect_times.end_unix", "unix_time")

test <- arrange(bind_rows(ROV_transects, start_rows, end_rows), unix_time)
# View(test)

# now join the transect info
ROV_join1 <- dplyr::left_join(test, 
                             dplyr::select(transect_times, c(start_unix, transect_depth, dive_number, comment_start)),
                              dplyr::join_by("unix_time"=="start_unix"))
ROV_join <- dplyr::left_join(ROV_join1, 
                              dplyr::select(transect_times, c(end_unix, transect_depth, dive_number, comment_end)),
                              dplyr::join_by("unix_time"=="end_unix"))
ROV_join <- arrange(ROV_join, unix_time)
# View(ROV_join)

# THIS WORKS YES
# DO NOT join by dive number. Everything will explode. 
# there are extra cols fix it later don't worry right now


# create a loop by dive eventually on the outside (above)
# create a loop by transect here

# first, fill in transect depth all the way down
ROV_join <- ROV_join %>% 
  fill(transect_depth.x) 

# View(ROV_join)

# Goal is to run smoothing for each individual transect based on comments in ROV_join as proxy for start times and end times

# eventually, make this into a dynamic filter where you're grabbing all of the rows with same transect depth
# for now, as a trial:

ROV_benthic1 <- filter(ROV_join, transect_depth.x=="800") # rename transect-1 later
ROV_benthic <-  filter(ROV_benthic1, !is.na(latitude_dd))



# Now test out smoothing 
ROV_SMA_window <- seq(from = 1, to = nrow(ROV_benthic), by = 100) # by 100 is arbitrary !
ROV_SMA_distance <- c()

for(j in ROV_SMA_window){
  ROV_smooth <- ROV_benthic |> 
    dplyr::mutate(Lat_SMA = TTR::SMA(latitude_dd, n = j), # ttr is pkg, sma is simple moving avg
                  Lon_SMA = TTR::SMA(longitude_dd, n = j), # smoothes lat/lon/depth cols
                  Depth_SMA = TTR::SMA(depth_m, n = j))
  ROV_distance_smooth <- ROV_distance(ROV_smooth, lat = Lat_SMA, long = Lon_SMA)
  ROV_distance_m <- sum(ROV_distance_smooth$distance_3D_m, na.rm = TRUE)
  ROV_SMA_distance <- c(ROV_SMA_distance, ROV_distance_m)
}

#create data frame of smoothing window and total ROV distance traveled
ROV_SMA_df <- as.data.frame(cbind(ROV_SMA_window, ROV_SMA_distance))

#-------------------------------------------------------------------------------
#Outlier Detection

#add column with differences between pairs of distances across rows
ROV_SMA_df <- ROV_SMA_df |> 
  dplyr::mutate(Distance_diff = c(diff(ROV_SMA_distance),0)) #zero needed to make full column

#MAD-median outlier detection across differences. Uses function described in 
# Wilcox, R.R. (2022) "Introduction to Robust Estimation and Hypothesis Testing"
# Fifth Edition, Elsevier. https://osf.io/xhe8u/
MadMed_out_dist <- out(ROV_SMA_df$Distance_diff) #output is a list
ROV_SMA_df_outliers <- as.data.frame(MadMed_out_dist[[3]])
colnames(ROV_SMA_df_outliers) = c("distance")
summary(ROV_SMA_df_outliers) #visual check

outlier_threshold <- ROV_SMA_df |> # what is the point where you have the first non-outlier
  dplyr::filter(!Distance_diff %in% ROV_SMA_df_outliers$distance) |> 
  dplyr::first()

ROV_distance_traveled <- outlier_threshold$ROV_SMA_distance

ROV_distance_traveled_vec <- c(ROV_distance_traveled_vec, ROV_distance_traveled)

print(paste0("Dive",i," completed"))
# }

ROV_distance_df <- data.frame(expedition = expedition, 
                              dive_number = as.numeric(ROV_dive_numbers), 
                              distance_m = ROV_distance_traveled_vec)



# and show
#raw data
# ROV_benthic |>
#   leaflet::leaflet() |>
#   leaflet::addTiles() |>
#   leaflet::addPolylines(lng = ~longitude_dd, lat = ~latitude_dd)
# # 
#smoothed data
ROV_smooth_predicted |>
  leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::addPolylines(lng = ~Lon_SMA, lat = ~Lat_SMA)






