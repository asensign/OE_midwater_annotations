#Tests for package availability and installs missing packages that are needed
#in order to run this code:
if(!require('purrr'))install.packages('purrr')
if(!require('dplyr'))install.packages('dplyr')
if(!require('tidyr'))install.packages('tidyr')
if(!require('stringr'))install.packages('stringr')
if(!require('ggplot2'))install.packages('ggplot2'); library('ggplot2')
if(!require('tidyverse'))install.packages('tidyverse'); library('tidyverse')
if(!require('lubridate'))install.packages('lubridate'); library('lubridate')
if(!require('stringr'))install.packages('stringr'); library('stringr')    

#-------------------------------------------------------------------------------
#source functions
#need to manually set the file path for the functions folder within your local repository
# ASE mac function_names <- list.files(path = "/Users/alexandraensign/Hollings_remote/OER_annotations_reporting-main/Functions/midwater",
                             #pattern = "[.]R$", full.names = TRUE)
function_names <- list.files(path = "C:/Users/Alexandra.Ensign/Documents/GitHub/OE_midwater_annotations/Functions",
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)
#-------------------------------------------------------------------------------

# wd <- "C:/Users/Alexandra.Ensign/Documents/EX2107"
# setwd(wd)

#set standard name to refer to your data, using the naming convention
#"EX","expedition number", e.g.:
data_name <- "EX2107"
# data_name <- "EX1806"
# data_name <- "EX1903L2" # this one takes about 35 minutes

#create vector of dive numbers for your dataset. 
#dive_number<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
#dive_names <- c("DIVE01", "DIVE02","DIVE03", "DIVE04", "DIVE05", "DIVE06", "DIVE07", "DIVE08", "DIVE09", "DIVE10", "DIVE11", "DIVE12", "DIVE13", "DIVE14", "DIVE15", "DIVE16", "DIVE17", "DIVE18", "DIVE19")

if (data_name == "EX1806") {
  dive_number<-c(2,4,15)
  dive_names <- c("DIVE02", "DIVE04", "DIVE15")
} else if (data_name=="EX2107") {
  dive_number<-c(3,13)
  dive_names <- c("DIVE03", "DIVE13")
} else if (data_name=="EX1903L2") {
  dive_number<-c(2,6,8,9,14)
  dive_names <- c("DIVE02", "DIVE06", "DIVE08", "DIVE09", "DIVE14")
}

# print(data_name)
# print(dive_number)
#wd <- "C:/Users/Alexandra.Ensign/Documents/"
wd <- paste0("C:/Users/Alexandra.Ensign/Documents/midwater_R_files/",data_name)
print(wd)
setwd(wd)
#------------------------------------------------------------------------------

#Read in SeaTube .csv annotation file(s) and apply clean_annotation function.
#Annotations can be downloaded from https://data.oceannetworks.ca/SeaTubeSearch.
#This code can accommodate a single .csv containing all dives or a group of annotation 
#files saved locally as .csv exports from SeaTube. Uses the number of files
#in the annotation folder to determine which import process to execute.
#If only one file is present in the folder, this code assumes that the file
#follows the naming convention "SeaTubeAnnotations_data_name.csv".

annotation_paths<-list.files(paste0(wd, "/annotations"), 
                             pattern = "[.]csv$", full.names = TRUE)

if (length(annotation_paths > 1)) {
  annotation_list<-purrr::map(annotation_paths, 
                              \(x) read.csv(x, header = TRUE, colClasses = 
                                              c("Common.Count" = "character",
                                                "Component" = "character",
                                                "Substrate.Origin" = "character",
                                                "Substrate.Class" = "character",
                                                "Substrate.Subclass" = "character",
                                                "Substrate.Group" = "character",
                                                "Substrate.Subgroup" = "character",
                                                "Geoform.Origin" = "character",
                                                "Geoform" = "character",
                                                "Geoform.Type" = "character"), 
                                            na.strings = ""))
  
  annotation_clean<- annotation_list |> 
    purrr::map(clean_annotation) |> 
    purrr::list_rbind()
} else {
  annotation_import <- read.csv(paste0(wd, "/annotations/SeaTubeAnnotations_", 
                                       data_name, ".csv"), header = TRUE, 
                                colClasses = c("Common.Count" = "character",
                                               "Component" = "character",
                                               "Substrate.Origin" = "character",
                                               "Substrate.Class" = "character",
                                               "Substrate.Subclass" = "character",
                                               "Substrate.Group" = "character",
                                               "Substrate.Subgroup" = "character",
                                               "Geoform.Origin" = "character",
                                               "Geoform" = "character",
                                               "Geoform.Type" = "character"),
                                na.strings = "")
  annotation_clean <- clean_annotation(annotation_import)
}
#str(annotation_clean)
#View(annotation_clean)
#-------------------------------------------------------------------------------
#Download dive summary text files for use in extracting the benthic portion of
#the dive, and ROV tracks .csv files to calculate distance traveled metric.
#Save to two new subdirectories within the existing expedition directory
data_name_lower <- tolower(data_name)
dir.create(paste0(wd,"/dive_summaries/"))
dir.create(paste0(wd,"/ROV_tracks"))

#This downloads available dive summary .txt files based on the dive name vector
#above and prints an error if one is missing (UCH dives do not have dive summary
#.txt files)
dive_summary_file_QAQC(dive_names)

#stop here and see if there are any missing dive summaries based on the output
#of the above code; update dive_names and dive_number if needed or else the code
#below will be interrupted by a missing zip folder

dive_ancillary_file_extraction(dive_names)
#-------------------------------------------------------------------------------

transect_start<-filter(annotation_clean, (str_detect(comment, regex("start transect", ignore_case = T))))
transect_end<-filter(annotation_clean, (str_detect(comment, regex("end transect", ignore_case = T))))
# print(transect_start)
#-------------------------------------------------------------------------------
# TRANSECT ID NUMBER 
#  set transect numbers based on transect start times. When the dive changes between consecutive start times, transect number resets at 1 and then increases sequentially from 1.
# this may not be necessary anymore rip

transect_number = list()
counter <- 1

for (i in seq(1, (nrow(transect_start)))) { # for every row in the start times,
  
  if (i != 1) {

    dive1 <- as.numeric(transect_start$dive_number[i]) # read current and following dive number.
    dive2 <- as.numeric(transect_start$dive_number[i-1])

    if (dive1 == dive2) { # while dives are the same, label the transect number sequentially based on row number
      print('same as last')
      counter = counter+1
      print(counter)
      transect_number[i] <- counter
    } 
    else {
      print(' below is different')
      counter <- 1
      print(counter)
      transect_number[i] <- counter 
    }
  }
  
  else {                        # set first row of table to always be transect #1
    transect_number[i] <- 1     
    print('last row')
    }
}

transect_start$transect_num <- unlist(transect_number)
transect_end$transect_num <- transect_start$transect_num # duplicate the transect number labels to transect-end times

transect_info <- arrange(rbind(transect_start,transect_end), date_time) # and merge with start times, sort by date_time
# print(transect_info)
#-------------------------------------------------------------------------------


# add label based on depth to each transect (just filtering out the depth from the comment and giving it a tidy column)
transect_start$depth_ID <- NA
transect_end$depth_ID <- NA

for (i in 1:(nrow(transect_start))) {
  transect_start$depth_ID[i] <- parse_number(transect_start$comment[i])
}
for (i in 1:(nrow(transect_end))) {
  transect_end$depth_ID[i] <- parse_number(transect_end$comment[i])
}

print(transect_start)
print(transect_end)
#-------------------------------------------------------------------------------


# create a reformatted dataframe that has start, end, and depth as columns to use in ROV distance traveled script

times_reformat<- data.frame(matrix(ncol=4, nrow=nrow(transect_start)))
col_names= c('dive_number', 'depth_ID', 'start_time', 'end_time')
colnames(times_reformat) <- col_names
# print(times_reformat)

times_reformat$dive_number <- transect_start$dive_number
times_reformat$depth_ID <- transect_start$depth_ID
times_reformat$start_time <- transect_start$date_time
times_reformat$end_time <- transect_end$date_time

print(times_reformat) # make sure the dates, times, and dives are in order :D

write.csv(transect_info, paste0(wd,"/exports/midwater_transect_times_as-annotations_", data_name, ".csv"),row.names = FALSE)
write.csv(times_reformat, paste0(wd,"/exports/midwater_transect_times_", data_name, ".csv"),row.names = FALSE)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------

# check for match between commented transect depth and actual measured depth
# tolerance of 3 m
for (i in 1:nrow(transect_info)) {
  upper_bound <- as.numeric(transect_info$depth_ID[i]) + 3
  lower_bound <- as.numeric(transect_info$depth_ID[i]) - 3
  if (isTRUE(transect_info$depth[i] > upper_bound | transect_info$depth[i] < lower_bound)) {
    print('Measured depth +/-3 m outside of transect depth')
  }
}
# this gets written out into csv format below
#-------------------------------------------------------------------------------
# Filter for annotations falling within midwater transects

filtered <- data.frame()

print(nrow(annotation_clean))

for(i in 1:nrow(annotation_clean)){                                               # for each annotation
  for (j in 1:nrow(transect_start)){                                              # check each transect start/end time 
    
    ann_time<-ymd_hms(annotation_clean$date_time[i])
    t_start<- ymd_hms(transect_start$date_time[j])                                  # set nicer var names & iterate thru annotations and transect times
    t_end<- ymd_hms(transect_end$date_time[j])
    
    if (ann_time > t_start && ann_time < t_end){                                 # if annotation time is within transect times
      filtered <- rbind((annotation_clean[i,]), filtered)                         # append each annotation that falls under the if statement to the new df, filtered, and rename
    }
  }
}
#-------------------------------------------------------------------------------
filtered$transect_num <- NA         
filtered$depth_ID <- NA                                                                        # declare a new column in the annotations for transect # (so rbind doesn't freak out)
annotations_with_transects <- arrange(rbind(filtered,transect_info), date_time)      # append the transect rows (full anns) with the full annotations

clean_df <- annotations_with_transects %>% 
  fill(depth_ID) %>% 
  fill(transect_num)

# write out all clean annotations
dir.create(paste0(wd,"/exports/"))
write.csv(clean_df, paste0(wd,"/exports/midwater_annotations_", data_name, ".csv"),row.names = FALSE)
