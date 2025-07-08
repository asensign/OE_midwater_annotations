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
print(files)

CTD_all <- list()

for (i in 1:length(files)) {
  CTD_file <- data.frame(read.csv(files[i]))

  filename <- files[i]
  dive_numbern <- (c(((unlist(strsplit(filename, ("_")))[2]))))

  CTD_file$expedition <- data_name
  CTD_file$dive_number <- dive_numbern
  
  rbind(CTD_file, CTD_all[i])
}

View(CTD_all)


  
  
CTD_file$dive_number[j] <- (c(((unlist(strsplit(filename, ("_")))[2]))))

  

View(CTD_all)


setwd(wd)



