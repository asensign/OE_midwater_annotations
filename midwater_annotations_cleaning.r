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
#data_name <- "EX1806"
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

#-------------------------------------------------------------------------------
# TRANSECT ID NUMBER 
#  set transect numbers based on transect start times. When the dive changes between consecutive start times, transect number resets at 1 and then increases sequentially from 1.

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

# copy depth from row over to new column tpo use for name instead of transect_num
# 
# print(transect_start)
# print(transect_end)

transect_start$transect_num <- unlist(transect_number)
transect_end$transect_num <- transect_start$transect_num # duplicate the transect number labels to transect-end times
transect_info <- arrange(rbind(transect_start,transect_end), date_time) # and merge with start times, sort by date_time
# print(transect_info)

transect_info$depth_ID <- NA

for (i in 1:(nrow(transect_info))) {
  transect_info$depth_ID[i] <- extract_numeric(transect_info$comment[i])
}
# for (i in 1:(nrow(transect_start))) {
#   transect_start$depth_ID[i] <- extract_numeric(transect_start$comment[i])
# }
# for (i in 1:(nrow(transect_end))) {
#   transect_end$depth_ID[i] <- extract_numeric(transect_end$comment[i])
# }
# print(transect_start)
# print(transect_end)
# print(transect_info)

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
write.csv(transect_info, paste0(wd,"/exports/midwater_transect_times_", data_name, ".csv"),row.names = FALSE)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# 
# benthic_join<-dplyr::inner_join(annotation_clean, benthic_times, 
#                                 dplyr::join_by("dive_number" == "dive_number"))
# 
# benthic_annotations<- benthic_join |> 
#   dplyr::group_by(dive_number) |> 
#   dplyr::filter(date_time>=benthic_start & date_time<=benthic_end) |> 
#   dplyr::ungroup()
# View(benthic_annotations)
# 
# 
# 
# #-------------------------------------------------------------------------------
# # List of file paths to each dive
# dive_summary_paths<-list.files(paste0(wd, "/dive_summaries"), 
#                                pattern = "[.]txt$", full.names = TRUE)
# 
# # to avoid df size issues, loop through transect info df for EACh dive.
# 
# print(dive_summary_paths)
# for (i in length(dive_summary_paths)) {
#   
#   each_dive<-dive_summary_paths[i]
#   print(each_dive)
#   # full dives``
#   #midwater_start_list<-purrr::map(dive_summary_paths, 
#   #                               \(x) import_midwater_start_post2020(x))
#   #midwater_end_list<-purrr::map(dive_summary_paths, 
#   #                              \(x) import_midwater_end_post2020(x))
#   
#   transect_start_list<-purrr::map(each_dive, 
#                                   \(x) import_midwater_transect_start_post2020(x))
#   transect_end_list<-purrr::map(each_dive, 
#                                 \(x) import_midwater_transect_end_post2020(x))
#   # in case I need these later 
#   #transect_duration_list<-purrr::map(dive_summary_paths, 
#   #                                \(x) import_midwater_transect_duration_post2020(x))
#   transect_depth_list<-purrr::map(each_dive, 
#                                   \(x) import_midwater_transect_depth_post2020(x))
#   
#   #Collapse lists to vectors, add to dataframe
#   # need to figure out how to read dates in from text file as well - rn they are the current date
#   transect_start <- as.POSIXlt(unlist(transect_start_list), tz = "UTC", format = "%H:%M:%OS")
#   print(transect_start)
#   
#   transect_end <- as.POSIXlt(unlist(transect_end_list), tz = "UTC", format = "%H:%M:%OS")
#   print(transect_end)
#   
#   transect_depth <- unlist(transect_depth_list)
#   print(transect_depth)
#   
#   transect_n <- 1:length(transect_start)
#   print(transect_n)
#   
#   transect_df<-data.frame(transect_n, transect_start, transect_end, transect_depth) # does NOT separate by dive
#   print(transect_df)
# }
# 
# 
# 
# #-------------------------------------------------------------------------------
# #Download dive summary text files for use in extracting the benthic portion of
# #the dive, and ROV tracks .csv files to calculate distance traveled metric.
# #Save to two new subdirectories within the existing expedition directory
# data_name_lower <- tolower(data_name)
# dir.create(paste0(wd,"/dive_summaries/"))
# dir.create(paste0(wd,"/ROV_tracks"))
# 
# #This downloads available dive summary .txt files based on the dive name vector 
# #above and prints an error if one is missing (UCH dives do not have dive summary 
# #.txt files)
# dive_summary_file_QAQC(dive_names)
# 
# #stop here and see if there are any missing dive summaries based on the output
# #of the above code; update dive_names and dive_number if needed or else the code 
# #below will be interrupted by a missing zip folder
# 
# dive_ancillary_file_extraction(dive_names)
# 
# #-------------------------------------------------------------------------------
# 
# 
# 
# #transect_times<-data.frame(dive_number,transect_start,transect_end)
# 
# #midwater_join<-dplyr::inner_join(annotation_clean, transect_times, 
#                                 #  dplyr::join_by("dive_number" == "dive_number"))
# 
# 
# #transect_times1<-data.frame(dive_number,transect_start,transect_end)
# #transect_times2<- data.frame(dive_number,transect_start,transect_end)
# 
# 
# #-------------------------------------------------------------------------------
# #Joins the clean annotations dataframe to the benthic times dataframe 
# #and then filters the annotations data to only include the benthic portion of
# #each dive. This join also removes any dives with no corresponding dive summary
# #file (e.g. test dives, UCH dives, mid-water-only dives).
# 
# benthic_join<-dplyr::inner_join(annotation_clean, benthic_times, 
#                                dplyr::join_by("dive_number" == "dive_number"))
# 
# benthic_annotations<- benthic_join |> 
#   dplyr::group_by(dive_number) |> 
#   dplyr::filter(date_time>=benthic_start & date_time<=benthic_end) |> 
#   dplyr::ungroup()
# View(benthic_annotations)
# 
# 
# 
# 
# # write out all clean annotations
# dir.create(paste0(wd,"/exports/"))
# write.csv(annotation_clean, paste0(wd,"/exports/clean_annotations_all_", data_name, ".csv"),
# row.names = FALSE)
# 
# write.csv(midwater_annotations, paste0(wd, "/exports/midwater_annotations_", 
#                                       data_name, ".csv"), row.names = FALSE)
# 
# write.csv(midwater_times, paste0(wd,"/exports/midwater_times_", data_name, ".csv"),
#           row.names = FALSE)
# 
# 
# 
