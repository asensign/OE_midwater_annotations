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
library(DescTools)
library(diffr)
library(data.table)

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
  # View(stats_all_dives)
  stats_all_exs <- rbind(stats_all_exs, stats_all_dives)
}

stats_all_exs$expedition2 <- str_remove(stats_all_exs$expedition, "EX")
# remove dive 14 freom 1903L2 !!!
stats_all_exs <- stats_all_exs %>% dplyr::filter(dive_number != "14") # also plot once before removing
View(stats_all_exs)


#----------------------------------------------------------------
# Create basic box/scatter plots of annotations per minute by ex/dive/transect
# no environmental data here yet

# box plot of annotations/min for each expedition
bp <- ggplot(stats_all_exs, aes(x=expedition2, y=ann_per_min, fill=expedition, color=expedition2)) + theme_bw() +
  geom_boxplot(alpha=0.2, outlier.shape=16, outlier.size=3, notch=FALSE) + 
  xlab("Expedition") + ylab("Average annotations per minute") + scale_color_manual(values = c('1806' = "#eb16ca", '1903L2' = "#0000FF"))#, "2107" = '#FFD700')) 
# print(bp)         

#Shapiro test for normality
test <- shapiro.test(stats_all_exs$ann_per_min)
print(test)

#Wilcoxon-Mann Whitney test	
test2 <- wilcox.test(stats_all_exs$ann_per_min ~ stats_all_exs$expedition2)
print(test2)

# basic scatterplot, same deal
dive_num <- unique(stats_all_exs$dive_number)

scatter_all <- ggplot(stats_all_exs, aes(x=ann_per_min, y=depth_ID, color=expedition2)) + theme_bw() +
  geom_point(size=2) + ylab("Transect depth (m)") + xlab("Average annotations per minute") +  
  scale_y_reverse() +
  scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF"))#, "2107" = '#FFD700')) 

# print(scatter_all)
# make an individual plot by transect depth for each dive
# for (k in 1:length(df_list)) {
#   scatter <- ggplot(df_list[[k]], aes(x=depth_ID, y=ann_per_min)) + geom_point(size=2, color="black") + xlab("Transect depth") + ylab("Average annotations per minute")
#   print(scatter)

#----------------------------------------------------------------
#----------------------------------------------------------------

# Filter CTD data and rbind from dive files to expedition df to df with both expeditions

CTD_all_exs <- data.frame()

# loop by expedition
for (k in seq(1, length(data_names))){
# for (k in 1:1){
  data_name <- data_names[k]
  data_name2 <- str_remove(data_name, "EX")
  print(data_name)
  print(data_name2)
  # read in environmental data
  wd2 <- paste0("C:/Users/Alexandra.Ensign/Documents/CTD/", data_name, "/")
  setwd(wd2)
  print(wd2)
  ### for each CTD dive file (.csv) in the expedition selected,
  files <- list.files(pattern = '*.csv')
  CTD_all_dives <- data.frame()

   # Loop by dive file
  
  for (m in 1:length(files)) {
  # for (m in 1:1) {
      
    # read in file and read dive number
    filename <- files[m]
    dive_numbern <- (c(((unlist(strsplit(filename, ("_")))[2]))))
    # convert dive number to numeric
    dive_numbernn <- as.numeric(unlist(regmatches(dive_numbern, gregexpr("[0-9]+", dive_numbern))))

    print(filename)
    print(dive_numbern)
    print(dive_numbernn)
    
    # define empty data frame for CTD data by dive
    CTD_df <- data.frame()
    # read in CTD data from ONE DIVE AT A TIME
    CTD_df <- read.csv(paste0(filename))
    #declare ex and dive numbers based on what you read in w/in the loop
    CTD_df$expedition <- data_name
    CTD_df$dive_number <- dive_numbernn
    # View(CTD_df)
    
    # read in transect depths BY DIVE from stats, using the expedition and dive we're on in this loop
    # if you have non-midwater dives in your directory, transect depths will print as integer(0)
    
    filter_stats_bydive <- dplyr::filter(stats_all_exs, expedition2==data_name2 & dive_number==dive_numbernn)
    # View(filter_stats_bydive)
    transect_depths <- c(unique(filter_stats_bydive$depth_ID))
    print(transect_depths)
    print('outside loop')
    
    closest_depths <- list()
    
    # filter all CTD data to only have the rows of transect depths for this specific dive within the loop
    # first, find clsoest depSM value to each transect depth, and make a list of those, removing duplicates from Closest()
    for (w in 1:length(transect_depths)) {
      depth <- transect_depths[w]
      print(depth)
      closest_val <- Closest(CTD_df$depSM, depth)[1] # if there are duplicates, only print 1
      print(closest_val)
      closest_depths <- c(closest_depths, closest_val)
    }
    # print(closest_depths)
    
    # now filter CTD data using those closest values
    CTD_df_filtered <- CTD_df %>% 
      dplyr::filter(depSM %in% closest_depths) %>%
      dplyr::group_by(depSM) %>%
      dplyr::filter(row_number() == 1)
    
    CTD_df_filtered$transect_ID <- as.factor(paste(data_name, dive_numbernn, round(CTD_df_filtered$depSM)))
    
    # View(CTD_df_filtered)

    # this is another way to do it, but leaves multiple depths per transect
    # could probably be meshed with above and be much more efficient lol
    # CTD_df_filtered <- CTD_df  %>% 
      # dplyr::filter(map_lgl(depSM, ~ any(near(.x, transect_depths, tol = 0.05))))
    # View(CTD_df_filtered)
    
    
    #     CTD_df <- CTD_df %>%
    #       filter(map_lgl(depSM, ~ any(near(.x, target_values, tol = tolerance)))) %>%
    #       group_by(depSM) %>%  
    # summarise(
    #  avg_prDE = mean(prDE),
    #  avg_temp = mean(t090C),
    #  avg_sbeox = mean(sbeox0Mg.L),
    #  avg_sal = mean(sal00))
    # expedition = expedition)
    # dive_number = dive_number)
    # mutate(avg_prDE = mean(prDE)) %>%
    # mutate(avg_temp = mean(t090C)) %>%
    # mutate(avg_sbeox = mean(sbeox0Mg.L)) %>%
    # mutate(avg_sal = mean(sal00)) %>%
    # ungroup()
    
    # View(CTD_df)
    
    
    # bind that dive to a dataframe containing all dives for expedition
    CTD_all_dives <- rbind(CTD_all_dives, CTD_df_filtered)
  }
  # bind both expeditions' CTD data together for plotting
  CTD_all_exs <- rbind(CTD_all_exs, CTD_all_dives)
}

View(CTD_all_exs)


# now join stats to CTD dataframe...
joined <- dplyr::left_join(stats_all_exs, CTD_all_exs, by="transect_ID")
View(joined)

#----------------------------------------------------------------
#----------------------------------------------------------------

# plot CTD vs ann/min as scatter plots

# prDE <- ggplot(joined, aes(x=prDE, y=ann_per_min, color=expedition2)) + theme_bw() +
#   geom_point(size=2) + ylab("Average annotations per minute") + xlab("Average pressure (psi)") +  
#   scale_y_reverse() +
#   scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF"))#, "2107" = '#FFD700'))   
# print(prDE)
setwd(wd)
print(wd)
temp <- ggplot(joined, aes(x=t090C, y=ann_per_min, color=expedition2)) + theme_bw() + labs(color="Expedition") +
  geom_point(size=2) + ylab("Average annotations per minute") + xlab("Average temperature (deg C)") +  
  scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF")) + #, "2107" = '#FFD700'))
  geom_smooth(method = "lm", se = TRUE, level=0.95, linetype = "solid", linewidth = 1)   
print(temp)
ggsave("t090C_annotations.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

salinity <- ggplot(joined, aes(x=sal00, y=ann_per_min, color=expedition2)) + theme_bw() + labs(color="Expedition") +
  geom_point(size=2) + ylab("Average annotations per minute") + xlab("Average salinity (psu)") +  
  scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF"))+ #, "2107" = '#FFD700'))
  geom_smooth(method = "lm", se = TRUE, level=0.95, linetype = "solid", linewidth = 1)
print(salinity)
ggsave("sal00_annotations.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

sbeox <- ggplot(joined, aes(x=sbeox0Mg.L, y=ann_per_min, color=expedition2)) + theme_bw() + labs(color="Expedition") +
  geom_point(size=2) + ylab("Average annotations per minute") + xlab("Average oxygen (Mg/L)") +  
  scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF"))+ #, "2107" = '#FFD700'))
  geom_smooth(method = "lm", se = TRUE, level=0.95, linetype = "solid", linewidth = 1)
print(sbeox)
ggsave("sbeox_annotations.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

