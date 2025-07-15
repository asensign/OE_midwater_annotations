library(oceanmap)
library(ncdf4)
library(raster)
library(viridis)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)
library(spData)
library(oce)
library(dplyr)
library(lubridate)
library(ggpubr)
library(mgcv)
library(hms)
library(dplyr)
library(purrr)


wd <- paste0("C:/Users/Alexandra.Ensign/Documents/")
print(wd)
setwd(wd)


data_names <- list("EX1806", "EX1903L2") #, "EX2107"
stats_all_exs <- data.frame()

# read in all expeditions
for (j in seq(1, length(data_names))){
  data_name <- data_names[j]
  # read in each expedition by dive
  print(data_name)

  # using ROVCTD for 1903L2. To use CPCTD, manually move files in CTD folder
  # read in annotations
  annotations <- read.csv(paste0(wd, "midwater_R_files/", data_name, "/exports/midwater_annotations_", data_name, ".csv"))
  # read in summary_stats files for annotations/min
  wd3 <- paste0(wd,"midwater_R_files/", data_name, "/exports/summary_stats/")
  print(wd3)
  setwd(wd3)
  
  # for each summary_stats file by dive, loop thru and read and then concat
  summ_files <- list.files(pattern='*.csv')
  stats_all_dives <- data.frame()
  
  df_list <- list()

  for (i in 1:length(summ_files)) {
    filename <- summ_files[i]
    dive_numbern <- str_replace((c(((unlist(strsplit(filename, ("_")))[4])))), ".csv", "")
    print(filename)
    print(dive_numbern)
    
    df <- read.csv(paste0(filename))
    # View(df)
    df$expedition <- data_name
    df$dive_number <- dive_numbern
    df$transect_ID <- as.factor(paste(data_name, dive_numbern, df$depth_ID))
    df_list[[i]] <- df # will use this later for plotting by transect
    
    stats_all_dives <- rbind(stats_all_dives, df)
    
    
  }
  
  # # make one plot per expedition by depth/transect
  # scatter2 <- ggplot(stats_all_dives, aes(x=depth_ID, y=ann_per_min)) + geom_point(size=2, color="black") + xlab("Transect depth") + ylab("Average annotations per minute")
  # print(scatter2)
  
  # View(stats_all_dives)
  stats_all_exs <- rbind(stats_all_exs, stats_all_dives)
 
}
stats_all_exs$expedition <- str_remove(stats_all_exs$expedition, "EX")
# remove dive 14 freom 1903L2 !!!
stats_all_exs <- stats_all_exs %>% dplyr::filter(dive_number != "14") # also plot once before removing
View(stats_all_exs)

#----------------------------------------------------------------
# Create basic box/scatter plots of annotations per minute by ex/dive/transect
# no environmental data here yet

# box plot of annotations/min for each expedition
bp <- ggplot(stats_all_exs, aes(x=expedition, y=ann_per_min)) + geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE)
# color code to macth expeditions, remove grey background, fix labels
print(bp)         

# basic scatterplot, same deal
dive_num <- unique(stats_all_exs$dive_number)

# make an individual plot by transect depth for each dive
# for (k in 1:length(df_list)) {
#   scatter <- ggplot(df_list[[k]], aes(x=depth_ID, y=ann_per_min)) + geom_point(size=2, color="black") + xlab("Transect depth") + ylab("Average annotations per minute")
#   print(scatter)
# }

# make a scatter plot for all dives/expeditions by depth
# colors by expedition]
# stats_all_exs$expedition <- factor(stats_all_exs$expedition, levels = c("1806", "1903L2"))#, "2107"))

scatter_all <- ggplot(stats_all_exs, aes(x=ann_per_min, y=depth_ID, color=expedition)) + 
  geom_point(size=2) + xlab("Transect depth") + ylab("Average annotations per minute") +  
  scale_y_reverse() +
  scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF"))#, "2107" = '#FFD700')) 

# add line of best fit (linjear regression model)
# add shaded region for 95% CI

print(scatter_all)

#----------------------------------------------------------------
#----------------------------------------------------------------


# Filter CTD data and rbuind from dive files to expedition df to df with both expeditions

CTD_all_exs <- data.frame()
# CTD_avgs_allex <- data.frame()

for (k in seq(1, length(data_names))){
  data_name <- data_names[k]
  print(data_name)
  
  # read in environmental data
  wd2 <- paste0("C:/Users/Alexandra.Ensign/Documents/CTD/", data_name, "/")
  setwd(wd2)
  print(wd2)
  
  ### for each CTD dive file (.csv) in the expedition selected,
  files <- list.files(pattern = '*.csv')
  # CTD_file <- lapply(files, read.csv, header=T)
  
  CTD_all_dives <- data.frame()
  # CTD_avgs_all_dives <- data.frame()

  # add expedition and dive name columns to the CTD data based on the filenames
  # create a list of dataframes
  # end with a list of all CTD data for an expedition as dataframes by dive
  
  # set up[ transect depths and tolerance]
  stats_one_ex <- dplyr::filter(stats_all_exs, expedition==str_remove(data_name, "EX"))
  target_values <- c((unique(stats_one_ex$depth_ID)))
  tolerance <- 1e-10
  print(target_values)
  
  
  for (m in 1:length(files)) {
    # read in file and read dive number
    filename <- files[m]
    dive_numbern <- (c(((unlist(strsplit(filename, ("_")))[2]))))
    # print(filename)
    # print(dive_numbern)
    
    # read in CTD data from ONE DIVE AT A TIME
    CTD_df <- read.csv(paste0(filename))
    CTD_df$expedition <- data_name
    CTD_df$dive_number <- dive_numbern

    # pull one row per transect from CTD data
    #CTD_df <- CTD_df %>% dplyr::filter(depSM %in% transect_depths_list
    # CTD_df <- CTD_df %>% filter(map_lgl(depSM, (~ any(near(.x, target_values, tol = tolerance)))))
    # View(CTD_df)
    CTD_df <- CTD_df %>%
      filter(map_lgl(depSM, ~ any(near(.x, target_values, tol = tolerance)))) %>%
      group_by(depSM) %>%  
       # summarise(
       #  avg_prDE = mean(prDE),
       #  avg_temp = mean(t090C),
       #  avg_sbeox = mean(sbeox0Mg.L),
       #  avg_sal = mean(sal00))
        # expedition = expedition)
        # dive_number = dive_number)
      mutate(avg_prDE = mean(prDE)) %>%
      mutate(avg_temp = mean(t090C)) %>%
      mutate(avg_sbeox = mean(sbeox0Mg.L)) %>%
      mutate(avg_sal = mean(sal00)) %>%
      ungroup()
 

      
    # View(CTD_df)
    
    # bind that dive to a dataframe containing all dives for expedition
    CTD_all_dives <- rbind(CTD_all_dives, CTD_df)
  }
  # bind both expeditions' CTD data together for plotting
  CTD_all_exs <- rbind(CTD_all_exs, CTD_all_dives)
}

# setwd(wd3)
# View(CTD_avgs_all_exs)

View(CTD_all_exs)

# should have 14 rows




# CTD variables
prDE <- CTD_all_exs$prDE
temp <- CTD_all_exs$t090C
sbeox <- CTD_all_exs$sbeox0Mg.L
salinity <- CTD_all_exs$sal00



# Scatterplot for all vars for entire expedition (ALL DIVES)
#sv_time <- ggplot(data = echometrics, aes(x = Time, y = Sv_mean, color = TimeCategory)) + geom_line() + labs(y= "Sv (dB re 1 m^-1)", x = "Time") + 
# theme_bw() + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700')) 
# ggplot(, aes(x=, y=)) + geom_point(size=2, shape=23) 

# plot x as env data and y as ann/min based on func group (need to define)


# # parse by dive numberand plot scatterplot for each
dive_number<-unique(CTD_all_dives$dive_number)
dplyr::group_by(dive_number)






