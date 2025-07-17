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
#----------------------------------------------------------------
# read in echometrics data 
echometrics <- read.csv(paste0(wd, "echometrics.csv"))
echometrics$expedition_num <- echometrics$TimeCategory
View(echometrics)

##### roll stats to nearest echometric timestamp

echometrics_table <- data.table(echometrics)
stats_table <- data.table(stats_all_exs)

# make sure times are in compatible formats
echometrics_table$Datetime_S <- as.POSIXct(echometrics_table$Datetime_S)
stats_table$date_time <- as.POSIXct(stats_table$date_time)

# create a new column, RollDate, to use for the rolling join to preserve the original timestamps
stats_table[, RollDate := date_time]
echometrics_table[, RollDate := Datetime_S]

# set keys for data tables
setkey(stats_table, "RollDate")
setkey(echometrics_table, "RollDate")

# rolling join
roll_stats_to_echometrics <- stats_table[echometrics_table, roll=TRUE]
# View(roll_stats_to_echometrics)

##### flipped version: roll echometrics to join nearest stats times. 

# set keys for data tables
setkey(echometrics_table, "RollDate")
setkey(stats_table, "RollDate")

# rolling join
roll_echometrics_to_stats <- echometrics_table[stats_table, roll=TRUE]
View(roll_echometrics_to_stats)

#----------------------------------------------------------------
#----------------------------------------------------------------
# plot
wd <- paste0("C:/Users/Alexandra.Ensign/Documents/")
print(wd)
setwd(wd)

sv <- ggplot(roll_echometrics_to_stats, aes(x=Sv_mean, y=ann_per_min, color=expedition_num)) + theme_bw() + labs(color="Expedition") +
  geom_point(size=2) + ylab("Average annotations per minute") + xlab("Sv (dB ref 1 m2/m3") +  
  scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF")) + #, "2107" = '#FFD700'))
  geom_smooth(method = "lm", se = TRUE, level=0.95, linetype = "solid", linewidth = 1)   
print(sv)
ggsave("Figures/sv_annotations.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

com <- ggplot(roll_echometrics_to_stats, aes(x=Center_of_mass, y=ann_per_min, color=expedition_num)) + theme_bw() + labs(color="Expedition") +
  geom_point(size=2) + ylab("Average annotations per minute") + xlab("Center of mass (m)") +  
  scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF"))+ #, "2107" = '#FFD700'))
  geom_smooth(method = "lm", se = TRUE, level=0.95, linetype = "solid", linewidth = 1)
print(com)
ggsave("Figures/com_annotations.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

po <- ggplot(roll_echometrics_to_stats, aes(x=Proportion_occupied, y=ann_per_min, color=expedition_num)) + theme_bw() + labs(color="Expedition") +
  geom_point(size=2) + ylab("Average annotations per minute") + xlab("Proportion occupied (%)") +  
  scale_color_manual(values = c('1806' = "#CD34B5", '1903L2' = "#0000FF"))+ #, "2107" = '#FFD700'))
  geom_smooth(method = "lm", se = TRUE, level=0.95, linetype = "solid", linewidth = 1)
print(po)
ggsave("Figures/po_annotations.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

# fin
#----------------------------------------------------------------
#----------------------------------------------------------------

##########
# first attempt of rolling - tried to filter 
# first filter for echometrics in ann/min stats//transect start/end times
# filtered_echo <- data.frame()

# see if any annotation stats share timestamps (all ann timestamps are w/in midwater transects already) 
#    with echometrics. If yes, keep echometrics in new df

# 
# for(a in 1:nrow(echometrics)){
# # for(a in 1:1){
#   for (b in 1:nrow(stats_all_exs)){
#   # for (b in 1:2){
#     print(a)
#     echometrics_start <- echometrics$Datetime_S[a]
#     echometrics_end <- echometrics$Datetime_E[a]
#     stats_transect_start <- stats_all_exs$date_time[b]
#     stats_transect_end <- stats_all_exs$end[b]
#     
#     if (echometrics_start == stats_transect_start | echometrics_end == stats_transect_end) {
#       filtered_echo[a] <- rbind((echometrics[a,]), filtered_echo)
#     }
#   }
# }
# View(filtered_echo)

# if neither then do rolling join with closest start OR end time to the echometrics timestamp
