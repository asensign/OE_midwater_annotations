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
function_names <- list.files(path = "C:/Users/Alexandra.Ensign/Documents/OE_midwater_annotations/Functions",
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)
#-------------------------------------------------------------------------------

wd <- "C:/Users/Alexandra.Ensign/Documents/EX2107"
setwd(wd)

#set standard name to refer to your data, using the naming convention
#"EX","expedition number", e.g.:
data_name <- "EX2107"

#create vector of dive numbers for your dataset. The dive landing pages are a 
#dive_number<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
dive_number<-c(3,13)

#create a vector of character descriptors of dives, which will be used to 
#download the dive summary text files
#dive_names <- c("DIVE01", "DIVE02","DIVE03", "DIVE04", "DIVE05", "DIVE06", "DIVE07", "DIVE08", "DIVE09", "DIVE10", "DIVE11", "DIVE12", "DIVE13", "DIVE14", "DIVE15", "DIVE16", "DIVE17", "DIVE18", "DIVE19")
dive_names <- c("DIVE03", "DIVE13")
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


# Filter based on COMMENTED start/end transect times from seatube (not from text files for midwater)

transect_start2<-filter(annotation_clean, (str_detect(comment, regex("start transect", ignore_case = T))))
transect_start<-(select(transect_start2, expedition, dive_number, date_time, comment, latitude_deg, longitude_deg, depth_m))

transect_end2<-filter(annotation_clean, (str_detect(comment, regex("end transect", ignore_case = T))))
transect_end<-(select(transect_end2, expedition, dive_number, date_time, comment, latitude_deg, longitude_deg, depth_m))

transect_times <- arrange(rbind(transect_start,transect_end), date_time) # combine transect start and end times and sort by date_time
#print(transect_times)

#-------------------------------------------------------------------------------
# TRANSECT ID NUMBER 

#  set transect numbers based on transect start times. When the dive bchanges between consecutive start times, transect number resets at 1 and then inrcreases sequentially from 1.

transect_number = list()
#print(transect_start)

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
print(transect_start)


# print(transect_pairs)
# rep(1:(nrow(transect_times) / 2), each = 2, length.out = nrow(transect_times))
# new <- rbind(transect_times$transect_numbers, transect_pairs)
# print(new)

# 
# transect_times$transect_number <- transect_numbers
 
#for (i in seq(1, nrow(transect_times), by=2)) {
  # transect_times$transect_number[i] <- row_number(transect_times)
  # transect_times$transect_number[i+1] <- row_number(transect_times)
# }


# counter <- 0
# for (i in 1:nrow(annotation_clean)) {
#   print(annotation_clean$dive_number[i])
#   print(annotation_clean$dive_number[i-1])
# }
# 
# for (i in 1:nrow(annotation_clean)) {
#   if (annotation_clean$dive_number[i] == annotation_clean$dive_number[i-1]) { # this needs debugged
#     annotation_clean$transect_number[i] <- counter + 1
#   }
#   else {
#     counter <- 1
#     filtered$transect_number[i] <- counter + 1
#   }
#   print(counter)
# }


relocate(filtered$transect_number, .after = filtered$dive_number)
print(filtered)

#-------------------------------------------------------------------------------

# Filter for annotations falling within midwater transects

filtered <- data.frame()

for(i in 1:nrow(annotation_clean)){      # for each annotation
  for (j in 1:nrow(transect_start)){     # check each transect start/end time 
    
    ann_time<-ymd_hms(annotation_clean$date_time[i])
    t_start<- ymd_hms(transect_start$date_time[j])    # set nicer var names & iterate thru annotations and transect times
    t_end<- ymd_hms(transect_end$date_time[j])
    
    if (ann_time > t_start && ann_time < t_end){    # if annotation time is within transect times
      filtered <- rbind(filtered, (annotation_clean[i,]))  # append each annotation that falls under the if statement to the new df, filtered, and rename
    }
  }
}



#-------------------------------------------------------------------------------

# check for match between commented transect depth and actual measured depth

# is there a way to dynamically pull the depth from each comment? it's a string so idk if that's possible
#-------------------------------------------------------------------------------

benthic_join<-dplyr::inner_join(annotation_clean, benthic_times, 
                                dplyr::join_by("dive_number" == "dive_number"))

benthic_annotations<- benthic_join |> 
  dplyr::group_by(dive_number) |> 
  dplyr::filter(date_time>=benthic_start & date_time<=benthic_end) |> 
  dplyr::ungroup()
View(benthic_annotations)



#-------------------------------------------------------------------------------
# List of file paths to each dive
dive_summary_paths<-list.files(paste0(wd, "/dive_summaries"), 
                               pattern = "[.]txt$", full.names = TRUE)

# to avoid df size issues, loop through transect info df for EACh dive.

print(dive_summary_paths)
for (i in length(dive_summary_paths)) {
  
  each_dive<-dive_summary_paths[i]
  print(each_dive)
  # full dives``
  #midwater_start_list<-purrr::map(dive_summary_paths, 
  #                               \(x) import_midwater_start_post2020(x))
  #midwater_end_list<-purrr::map(dive_summary_paths, 
  #                              \(x) import_midwater_end_post2020(x))
  
  transect_start_list<-purrr::map(each_dive, 
                                  \(x) import_midwater_transect_start_post2020(x))
  transect_end_list<-purrr::map(each_dive, 
                                \(x) import_midwater_transect_end_post2020(x))
  # in case I need these later 
  #transect_duration_list<-purrr::map(dive_summary_paths, 
  #                                \(x) import_midwater_transect_duration_post2020(x))
  transect_depth_list<-purrr::map(each_dive, 
                                  \(x) import_midwater_transect_depth_post2020(x))
  
  #Collapse lists to vectors, add to dataframe
  # need to figure out how to read dates in from text file as well - rn they are the current date
  transect_start <- as.POSIXlt(unlist(transect_start_list), tz = "UTC", format = "%H:%M:%OS")
  print(transect_start)
  
  transect_end <- as.POSIXlt(unlist(transect_end_list), tz = "UTC", format = "%H:%M:%OS")
  print(transect_end)
  
  transect_depth <- unlist(transect_depth_list)
  print(transect_depth)
  
  transect_n <- 1:length(transect_start)
  print(transect_n)
  
  transect_df<-data.frame(transect_n, transect_start, transect_end, transect_depth) # does NOT separate by dive
  print(transect_df)
}







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



#transect_times<-data.frame(dive_number,transect_start,transect_end)

#midwater_join<-dplyr::inner_join(annotation_clean, transect_times, 
                                #  dplyr::join_by("dive_number" == "dive_number"))


#transect_times1<-data.frame(dive_number,transect_start,transect_end)
#transect_times2<- data.frame(dive_number,transect_start,transect_end)


#-------------------------------------------------------------------------------
#Joins the clean annotations dataframe to the benthic times dataframe 
#and then filters the annotations data to only include the benthic portion of
#each dive. This join also removes any dives with no corresponding dive summary
#file (e.g. test dives, UCH dives, mid-water-only dives).

benthic_join<-dplyr::inner_join(annotation_clean, benthic_times, 
                               dplyr::join_by("dive_number" == "dive_number"))

benthic_annotations<- benthic_join |> 
  dplyr::group_by(dive_number) |> 
  dplyr::filter(date_time>=benthic_start & date_time<=benthic_end) |> 
  dplyr::ungroup()
View(benthic_annotations)




# write out all clean annotations
dir.create(paste0(wd,"/exports/"))
write.csv(annotation_clean, paste0(wd,"/exports/clean_annotations_all_", data_name, ".csv"),
row.names = FALSE)

write.csv(midwater_annotations, paste0(wd, "/exports/midwater_annotations_", 
                                      data_name, ".csv"), row.names = FALSE)

write.csv(midwater_times, paste0(wd,"/exports/midwater_times_", data_name, ".csv"),
          row.names = FALSE)



