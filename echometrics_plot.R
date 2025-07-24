
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


wd <- paste0("C:/Users/Alexandra.Ensign/Documents/echoview_output/")
print(wd)
setwd(wd)

# bind all of the outputs from echoview into one csv file
files <- list.files(pattern = '*.csv')
tables <- lapply(files, read.csv, header =T)
echometrics <- do.call (rbind, tables)

# View(echometrics)

# Going to plot using start times for each bin
# reformat each date and time col into UTC
echometrics$Date_S <- as.POSIXct(strptime(echometrics$Date_S, format = "%Y%m%d", tz = "UTC"))
echometrics$Time_S <- as_hms(as.POSIXct(strptime(echometrics$Time_S, format = "%H:%M:%OS", tz = "UTC")))
echometrics$Date_E <- as.POSIXct(strptime(echometrics$Date_E, format = "%Y%m%d", tz = "UTC"))
echometrics$Time_E <- as_hms(as.POSIXct(strptime(echometrics$Time_E, format = "%H:%M:%OS", tz = "UTC")))

echometrics$Datetime_S <- with(echometrics, as.POSIXct(paste(Date_S, Time_S),
                                                       format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
echometrics$Datetime_E <- with(echometrics, as.POSIXct(paste(Date_E, Time_E),
                                                       format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# We need to check the distributions of our echometrics by creating histograms for them.
hist(echometrics$Sv_mean)
hist(echometrics$Center_of_mass)
hist(echometrics$Inertia)
hist(echometrics$Proportion_occupied)
hist(echometrics$Aggregation_index)
hist(echometrics$Equivalent_area)

# We can see that the aggregation index and equivalent area are very skewed, so we will transform them to log scale.
# Check their histograms.
echometrics$log_ea <- log(echometrics$Equivalent_area)
echometrics$log_ai <- log(echometrics$Aggregation_index)
hist(echometrics$log_ai)
hist(echometrics$log_ea)

# View(echometrics)
# print(length(echometrics$Datetime_S))
# print(is.na(echometrics))

# Although equivalent area is bimodal, we will accept this for now, as it is better than a skew. Now we can clean
# up our data. First, remove any rows with an NaN.
# echometrics <- na.omit(echometrics)

# Now delete rows where centre of mass is greater than the range. Range gives us the maximum depth from the transducer 
# whereas centre of mass gives us the average location of backscatter in the water column. If COM > range, then 
# that means seafloor backscatter was not completely removed and contributed highly to the data point. 
# We want to remove these inaccuracies.
echometrics <- echometrics[!(echometrics$Range<=echometrics$Center_of_mass),]

# Lastly, we will filter out rows in which Sv is greater than -55, as these data points were also highly affected
# by the seafloor noise. 
echometrics <- echometrics[echometrics$Sv_mean<=-55,]

View(echometrics)

#--------------------------------------------------------------------------------------
# Filter by expedition for color-coding purposes
# Pulled from echoview outputs manually
start_1806 <- as.POSIXct("2018-06-13 19:38:22.054")
end_1806 <- as.POSIXct("2018-07-02 02:55:55.834")
start_1903L2 <- as.POSIXct('2019-06-20 21:51:55.792')
end_1903L2 <- as.POSIXct('2019-07-06 22:06') # fix this one - it's not accurate yet
start_2107 <- as.POSIXct('2021-10-27 06:50:03.57')
end_2107 <- as.POSIXct('2021-11-15 07:10:29:00')

echometrics$expedition <- case_when(
  echometrics$Datetime_S >= start_1806 & echometrics$Datetime_S <= end_1806 ~ "1806",
  echometrics$Datetime_S >= start_1903L2 & echometrics$Datetime_S <= end_1903L2 ~ "1903L2",
  echometrics$Datetime_S >= start_2107 & echometrics$Datetime_S <= end_2107 ~ "2107",
)

echometrics$month <- format(echometrics$Datetime_S, "%B")

# drop na values from certain cols
# dropping all na values earlier caused problems for me, but this seems OK
echometrics <- echometrics %>% drop_na(expedition)
echometrics <- echometrics %>% drop_na(Lon_S)
echometrics <- echometrics %>% drop_na(Lat_S)

# set variable for plotting (must do this AFTER data cleaning!)
Time <- echometrics$Datetime_S
# print(length(Time))
Longitude <- echometrics$Lon_S
Latitude <- echometrics$Lat_S

View(echometrics)

#--------------------------------------------------------------------------------------
# read in transect times 
# (resetting the wd was the easier way... lapply throws a fit w/ the path option in list.files())
wd <- paste0("C:/Users/Alexandra.Ensign/Documents/echoview_output/transect_times/")
setwd(wd)
# bind all of the files together
files_transect <- list.files(pattern = '*.csv')
tables2 <- lapply(files_transect, read.csv, header = TRUE)
transect_times <- do.call (rbind, tables2)

# check time formats
transect_times$start_UTC <-as.POSIXct(transect_times$start_UTC, tz = "UTC")
transect_times$end_UTC<-as.POSIXct(transect_times$end_UTC, tz = "UTC")
transect_depths <- data.frame(transect_times$transect_depth)

# reset wd 
wd <- paste0("C:/Users/Alexandra.Ensign/Documents/")
setwd(wd)

# write out csv file of echometrics
write.csv(echometrics, paste0(wd, "echometrics.csv"))

#--------------------------------------------------------------------------------------
# Timeseries plots

# Sv vs. Time
sv_time <- ggplot(data = echometrics, aes(x = Time, y = Sv_mean, color = expedition)) + geom_line() + labs(y= "Sv (dB re 1 m^-1)", x = "Time") + 
  theme_bw() + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10)) + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700')) 
sv_time <- sv_time + facet_wrap(expedition ~ ., scales="free_x", nrow=3)
print(sv_time)
ggsave("Figures/sv_timeseries.png",plot = last_plot(),device=png(),dpi=200,width=3,height=3.65,units="in")
dev.off()

# Centre of Mass vs. Time
com_time <- ggplot(data = echometrics, aes(x = Time, y = Center_of_mass, color = expedition)) + geom_line() +
  labs(y= "Centre of Mass (m)", x = "Time") + theme_bw() + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10)) + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700'))
com_time <- com_time + facet_wrap(expedition ~ ., scales="free_x", nrow=3)
print(com_time)
ggsave("Figures/com_timeseries.png",plot = last_plot(),device=png(),dpi=200,width=3,height=3.65,units="in")
dev.off()

# Inertia vs. Time
# inertia_time <- ggplot(data = echometrics, aes(x = Time, y = Inertia, color = expedition)) + geom_line() +
  # labs(y="Inertia (m^-2)", x = "Time") + theme_bw() + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700'))
# intertia_time <- inertia_time + facet_wrap(expedition ~ ., scales="free_x", nrow=3)
# print(inertia_time)

# Proportion Occupied vs. Time
po_time <- ggplot(data = echometrics, aes(x = Time, y = Proportion_occupied, color = expedition)) + geom_line() + 
  labs(y="Proportion Occupied", x = "Time") + theme_bw() + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10)) + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700'))
po_time <- po_time + facet_wrap(expedition ~ ., scales="free_x", nrow=3)
print(po_time)
ggsave("Figures/po_timeseries.png",plot = last_plot(),device=png(),dpi=100,width=3,height=3.65,units="in")
dev.off()

# Aggregation vs. Time
ai_time <- ggplot(data = echometrics, aes(x = Time, y = log_ai, color = expedition)) + geom_line() +
  labs(y="Log Index Aggregation", x = "Time") + theme_bw() + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700'))
ai_time <- ai_time + facet_wrap(expedition ~ ., scales="free_x", nrow=3)
print(ai_time)
ggsave("Figures/ai_timeseries.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

# Equivalent Area vs. Time
ea_time <- ggplot(data = echometrics, aes(x = Time, y = log_ea, color = expedition)) + geom_line() +
  labs(y="Figures/Log Equivalent Area", x = "Time") + theme_bw() + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700'))
ea_time <- ea_time + facet_wrap(expedition ~ ., scales="free_x", nrow=3)
print(ea_time)
ggsave("Figures/ea_timeseries.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

# Putting all six plots on the same figure.
figure_1 <- ggarrange(sv_time, com_time,  po_time, ai_time, ea_time, #inertia_time,
                      labels = c("A", "B", "C", "D", "E", "F"),
                      align = c("v"),
                      ncol = 2, nrow = 3) 
print(figure_1)
ggsave("Figures/agg_timeseries.png",plot = last_plot(),device=png())
dev.off()

#-------------------------------------------------------------------------------

# Spatial plots

# base map plot
map <- ggplot() + geom_sf(data = spData::world, fill = "grey80", col = "black") + coord_sf(xlim = c(-82, -72), ylim = c(20, 40)) + theme_bw() + labs(y = "Latitude", x = "Longitude")
print(map)

# Sv vs. Coordinates
sv_position <- map + geom_point(data = echometrics, aes(x = Longitude, y = Latitude, color = Sv_mean)) + scale_color_viridis(name = "Sv (dB re 1 m^-1)", discrete = FALSE, option = "A") + labs(y="Latitude", x = "Longitude")
print(sv_position)
ggsave("Figures/sv_spatial.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

# Centre of Mass vs. Coordinates
com_position <- map + geom_point(data = echometrics, aes(x = Longitude, y = Latitude, color = Center_of_mass)) + scale_color_viridis(name = "Centre of Mass (m)", discrete = FALSE, option = "A", direction = -1) + labs(y="Latitude", x = "Longitude")
print(com_position)
ggsave("Figures/com_spatial.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

# Inertia vs. Coordinates
# inertia_position <- map + geom_point(data = echometrics, aes(x = Longitude, y = Latitude, color = Inertia)) + scale_color_viridis(name = "Inertia (m^-2)", discrete = FALSE, option = "A") + labs(y="Latitude", x = "Longitude")
# print(inertia_position)

# Proportion Occupied vs. Coordinates
po_position <- map + geom_point(data = echometrics, aes(x = Longitude, y = Latitude, color = Proportion_occupied)) + scale_color_viridis(name = "Proportion Occupied", discrete = FALSE, option = "A") + labs(y="Latitude", x = "Longitude")
print(po_position)
ggsave("Figures/po_spatial.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

# Aggregation vs. Coordinates
ai_position <- map + geom_point(data = echometrics, aes(x = Longitude, y = Latitude, color = log_ai)) + scale_color_viridis(name = "Log Index Aggregation", discrete = FALSE, option = "A") + labs(y="Latitude", x = "Longitude")
print(ai_position)
ggsave("ai_spatial.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

# Equivalent Area vs. Coordinates
echometrics <- na.omit(echometrics)
ea_position <- map + geom_point(data = echometrics, aes(x = Longitude, y = Latitude, color = log_ea)) + scale_color_viridis(name = "Log Equivalent Area", discrete = FALSE, option = "A") + labs(y="Latitude", x = "Longitude")
print(ea_position)
ggsave("Figures/ea_spatial.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

# Putting all six plots on the same figure.
figure_2 <- ggarrange(sv_position, com_position, po_position, ai_position, #inertia_position,
                      ea_position, labels = c("A", "B", "C", "D", "E"), #F
                      align = c("hv"),
                      ncol = 3, nrow = 3)
print(figure_2)
ggsave("Figures/agg_spatial.png",plot = last_plot(),device=png(),dpi=100)
dev.off()


# cruise track map
cruise_track_map <- map + geom_point(data = echometrics, aes(x = Longitude, y = Latitude, color = expedition)) + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700')) + labs(y="Latitude", x = "Longitude")
print(cruise_track_map)
ggsave("Figures/cruise_track_map.png",plot = last_plot(),device=png(),dpi=100)
dev.off()

## dive map
dive_lons <- c(-75.52429803,-77.32827961,-74.80059902, -79.445017,-79.581983,-78.087233,-77.15485,-74.81225, -77.41911,-78.25901)
dive_lats <- c(31.94937883,30.94027766,35.53850111, 29.110683,30.4344,30.9179,31.528833,35.73515, 30.70846,28.36854)
dive_expedition <- c("1806","1806","1806","1903L2","1903L2","1903L2","1903L2","1903L2","2107","2107")
dive_coords <- data.frame(Column1=dive_lons, Column2=dive_lats, Column3=dive_expedition)
View(dive_coords)

dive_map <- map + geom_point(data = dive_coords, aes(x = dive_lons, y = dive_lats, color = dive_expedition)) + scale_color_manual(values = c("1806" = "#CD34B5", "1903L2" = "#0000FF", "2107" = '#FFD700')) + labs(y="Latitude", x = "Longitude")
print(dive_map)
ggsave("Figures/dive_map.png",plot = last_plot(),device=png(),dpi=100)
dev.off()
