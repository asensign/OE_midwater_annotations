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

ROV_dive_numbers <- list.files(path = ROV_filepath, pattern = "[.]csv$", full.names = TRUE)
ROV_dive_numbers <- sapply(ROV_dive_numbers, ROV_dive_number_extract, USE.NAMES = FALSE)

ROV_distance_traveled_vec <- c() #this will become the vector of distances
#-------------------------------------------------------------------------------
transect_times <- read.csv(paste0(transect_times_wd,"midwater_transect_times_",expedition,".csv"), header = TRUE)
transect_times$start_UTC <-as.POSIXct(transect_times$start_UTC, tz = "UTC")
transect_times$end_UTC<-as.POSIXct(transect_times$end_UTC, tz = "UTC")
transect_depths <- data.frame(transect_times$transect_depth)
# print(transect_depths)
# print(transect_times)

# dive number -- eventually this will be a loop (START IT HERE)
# this is going to drive me insane later
i = "02"
ii='2'


# reads in rov stuff
ROV_import_df <- ROV_import(paste0(ROV_filepath,expedition,"_DIVE",i,"_ROVtrack.csv"))
# clean ROV df
ROV_clean_df <- ROV_clean(ROV_import_df)
#-------------------------------------------------------------------------------
# filter ROV_clean_df for only times falling within transects (both unix now) (13 digits, but only 10 show - be careful)
# Filter transect_times for only one dive at a time. 

ROV_transects <- data.frame()
transect_times <- filter(transect_times, dive_number==sprintf("%s",ii))

# use unix time stamps to filter ROV_clean_df for annotations that fall between each transect, one transect at a time:

for (n in 1:nrow(transect_times)){
# for (n in 1:1){         
  # for only the first transect of this dive
  # Create a new empty datafame to hold filtered ROV data by transect.
  #Will refresh for each transect as we loop thru n.
  
  # if ROV times from full ROV_clean_df fall within transect n, keep
  for(m in 1:nrow(ROV_clean_df)){                                             
    ROV_time<-ROV_clean_df$unix_time[m]

    t_start<- transect_times$start_unix[n]                                 
    t_end<- transect_times$end_unix[n]
    
    if (ROV_time > t_start && ROV_time < t_end){                                 
      ROV_transects <- rbind((ROV_clean_df[m,]), ROV_transects)   
    }
  }
  
  # View(ROV_transects)
}  
  
 #-------------------------------------------------------------------------------
# Add the EXACT 13-digit Unix times of transect start/end to the ROV_transect dataframe:
  
 # add empty rows at the end for later - these will contain transect start and end time
# ROV_transect[nrow(ROV_transect) + (nrow(transect_times)), ] <- NA
# ROV_transect[nrow(ROV_transect) + 2, ] <- NA
  
# Bind transect times and information with the ROV dataframe, for columns with the same datatype
  
start_rows <- data.frame(dplyr::select(transect_times, start_unix, lat_dd_start, lon_dd_start, expedition, dive_number, start_UTC, depth_m_start))
end_rows <- data.frame(dplyr::select(transect_times, end_unix, lat_dd_end, lon_dd_end, expedition, dive_number, end_UTC, depth_m_end))

# and rename columns to match for rbind()
setnames(start_rows, "start_unix", "unix_time")
setnames(start_rows, "lat_dd_start", "latitude_dd") 
setnames(start_rows, "lon_dd_start", "longitude_dd")
setnames(start_rows, "start_UTC", "UTC")
setnames(start_rows, "depth_m_start", "depth_m")

setnames(end_rows, "end_unix", "unix_time")
setnames(end_rows, "lat_dd_end", "latitude_dd")
setnames(end_rows, "lon_dd_end", "longitude_dd")
setnames(end_rows, "end_UTC", "UTC")
setnames(end_rows, "depth_m_end", "depth_m")

# flip sign of imported depths to match ROv data
start_rows$depth_m <- start_rows$depth_m * -1
end_rows$depth_m <- end_rows$depth_m * -1

##################################################                                  # the depths are off by a few m btwn. ROV and annotations - check

# test shows data columns that are the same btwn ROV and transect_times dfs (except alt)
test <- arrange(bind_rows(ROV_transects, start_rows, end_rows), unix_time)

# join remaining transect info that needs new columns to be created in the ROV df
# DO NOT join by dive number. Everything will explode. for now anyway
# extra columns -- take out later / needs cleaned
ROV_join1 <- dplyr::left_join(test, 
                             dplyr::select(transect_times, c(start_unix, transect_depth, comment_start)),
                              dplyr::join_by("unix_time"=="start_unix"))
ROV_join <- dplyr::left_join(ROV_join1, 
                              dplyr::select(transect_times, c(end_unix, comment_end)),
                              dplyr::join_by("unix_time"=="end_unix"))
ROV_join <- arrange(ROV_join, unix_time)

# fill in transect depth all the way down. Not actual depth, but an ID for which transect we're on.
ROV_midwater <- ROV_join %>% 
  fill(transect_depth) %>% 
  dplyr::filter(!is.na(latitude_dd))

# make sure all of that worked 
# View(ROV_midwater)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# create a loop by dive eventually on the outside (above)

# create a loop by transect 
df_list <- list()
  
for (n in 1:length(transect_times$transect_depth)) {
  
  td <- as.numeric(transect_times$transect_depth[n])
  df_name <- paste0("ROV_midwater_", td)

  df_list[[df_name]] <- ROV_midwater %>%
    dplyr::filter(transect_depth == sprintf("%s",td))
  
  
  write.csv(df_list[[df_name]], paste0(wd,"/exports/", expedition,"_dive", i, "_transect", td ,"_ROV_distance.csv"),row.names = FALSE)
}

# View(df_list$ROV_midwater_700)




stop()
# 6/25 working up to here. Issues with smoothing routine. Probably need to start from scratch
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Smoothing
#iterate generation of smooths across full dataset, calculate distance traveled 
#for each smooth, save into vector
ROV_SMA_window <- seq(from = 1, to = nrow(ROV_benthic), by = 100)
# print(ROV_SMA_window)
ROV_SMA_distance <- c()

for(j in ROV_SMA_window){
  ROV_smooth <- ROV_benthic |> 
    dplyr::mutate(Lat_SMA = TTR::SMA(latitude_dd, n = j),
                  Lon_SMA = TTR::SMA(longitude_dd, n = j),
                  Depth_SMA = TTR::SMA(depth_m, n = j))
  ROV_distance_smooth <- ROV_distance(ROV_smooth, lat = Lat_SMA, long = Lon_SMA) 
  ROV_distance_m <- sum(ROV_distance_smooth$distance_3D_m, na.rm = TRUE)
  ROV_SMA_distance <- c(ROV_SMA_distance, ROV_distance_m)
}

#create data frame of smoothing window and total ROV distance traveled
ROV_SMA_df <- as.data.frame(cbind(ROV_SMA_window, ROV_SMA_distance))
View(ROV_distance_smooth)
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

outlier_threshold <- ROV_SMA_df |> 
  dplyr::filter(!Distance_diff %in% ROV_SMA_df_outliers$distance) |> 
  dplyr::first()

ROV_distance_traveled <- outlier_threshold$ROV_SMA_distance

ROV_distance_traveled_vec <- c(ROV_distance_traveled_vec, ROV_distance_traveled)

# print(paste0("Dive",i," completed"))
# }

ROV_distance_df <- data.frame(expedition = expedition, 
                              dive_number = as.numeric(ROV_dive_numbers), 
                              distance_m = ROV_distance_traveled_vec)

# write.csv(ROV_distance_df, paste0(wd,"/exports/", expedition,"_ROV_distance.csv"),
          # row.names = FALSE)

#-------------------------------------------------------------------------------
#Visualize outlier detection results

# ggplot2::ggplot(ROV_SMA_df, ggplot2::aes(x = ROV_SMA_window, y = ROV_SMA_distance)) +
#   ggplot2::geom_point() +
#   ggplot2::labs(x = "Number of ROV position points used in simple moving average smooth",
#        y = "ROV distance traveled (m)",
#        title = "Change in predicted ROV distance traveled with increased smoothing",
#        subtitle = expedition) +
#   ggplot2::geom_vline(xintercept = ROV_distance_traveled, color = "#FF6C57", linewidth = 1.5) +
#   ggplot2::theme_bw()

#------------------------------------------------------------------------------
#Visualize raw and smoothed track lines

ROV_smooth_predicted <- ROV_benthic |>
  dplyr::mutate(Lat_SMA = TTR::SMA(latitude_dd), #, n = ROV_threshold$ROV_SMA_window),
                Lon_SMA = TTR::SMA(longitude_dd), #, n = ROV_threshold$ROV_SMA_window),
                Depth_SMA = TTR::SMA(depth_m)) #, n = ROV_threshold$ROV_SMA_window))

#raw data
# ROV_benthic |>
#   leaflet::leaflet() |>
#   leaflet::addTiles() |>
#   leaflet::addPolylines(lng = ~longitude_dd, lat = ~latitude_dd)
# 
#smoothed data
ROV_smooth_predicted |>
  leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::addPolylines(lng = ~Lon_SMA, lat = ~Lat_SMA)