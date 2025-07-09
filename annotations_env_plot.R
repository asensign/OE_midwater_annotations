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


wd <- paste0("C:/Users/Alexandra.Ensign/Documents/")
print(wd)
setwd(wd)

data_name <- "EX2107"
dive_number = "01"

# read in annotations
annotations <- read.csv(paste0(wd, "midwater_R_files/", data_name, "/exports/midwater_annotations_", data_name, ".csv"))

# read in environmental data
wd2 <- paste0("C:/Users/Alexandra.Ensign/Documents/CTD/", data_name, "/")
setwd(wd2)

### for each CTD dive file (.csv) in the expedition selected,
files <- list.files(pattern = '*.csv')
# CTD_file <- lapply(files, read.csv, header=T)

CTD_all_dives <- data.frame()

# add expedition and dive name columns to the CTD data based on the filenames
# create a list of dataframes
# end with a list of all CTD data for an expedition as dataframes by dive
for (i in 1:length(files)) {
  
  filename <- files[i]
  dive_numbern <- (c(((unlist(strsplit(filename, ("_")))[2]))))
  print(filename)
  print(dive_numbern)
  
  CTD_df <- read.csv(paste0(filename))

  CTD_df$expedition <- data_name
  CTD_df$dive_number <- dive_numbern
  
  CTD_all_dives <- rbind(CTD_all_dives, CTD_df)
}

# View(CTD_all_dives)
setwd(wd)

prDE <- CTD_all_dives$prDE
temp <- CTD_all_dives$t090C
sbeox <- CTD_all_dives$sbeox0Mg.L
salinity <- CTD_all_dives$sal00

# Scatterplot for all vars for entire expedition (ALL DIVES)

ggplot(CTD_all_dives, aes(x=wt, y=mpg)) + geom_point(size=2, shape=23)

# plot x as env data and y as ann/min based on func group (need to define)


# # parse by dive numberand plot scatterplot for each
dive_number<-unique(CTD_all_dives$dive_number)
dplyr::group_by(dive_number)




