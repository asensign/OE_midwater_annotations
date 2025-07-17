#Tests for package availability and installs missing packages that are needed
#in order to run this code:
if(!require('purrr'))install.packages('purrr')
if(!require('dplyr'))install.packages('dplyr')
if(!require('tidyr'))install.packages('tidyr')
if(!require('readr'))install.packages('readr')
library(data.table)
library(tidyr)
library(stringr)
#-------------------------------------------------------------------------------
#source functions
#need to manually set the file path for the functions folder within your local repository
function_names <- list.files(path = "C:/Users/Alexandra.Ensign/Documents/OE_midwater_annotations/Functions", 
                             pattern = "[.]R$", full.names = TRUE)
lapply(function_names, source)

#set working directory
wd <- "C:/Users/Alexandra.Ensign/Documents/midwater_R_files/"
setwd(wd)

#set standard name to refer to your data
data_name <- "EX1903L2"

midwater_annotations<-readr::read_csv(paste0(wd, data_name, "/exports/midwater_annotations_", 
                       data_name, ".csv"), col_names = TRUE)

# distance_traveled <- read.csv(paste0(wd,"/exports/",data_name,"_ROV_distance.csv"))
# View(distance_traveled)

dive_numbers<-unique(midwater_annotations$dive_number)
#QC check to make sure the benthic annotations dive numbers match the ROV track
#dive numbers
# all(dive_number == distance_traveled$dive_number)

dive_numbers #stop here and cross-reference with dive summary text files - remove
#text files that have no annotations from the folder or else the ROV_metrics 
#code below will fail
# for (i in dive_number) {
#   print(i)
#-------------------------------------------------------------------------------

# create column for transect duration and calculate it 

transect_start<-filter(midwater_annotations, (str_detect(comment, regex("start transect", ignore_case = T))))
transect_end<-filter(midwater_annotations, (str_detect(comment, regex("end transect", ignore_case = T))))

transect_times<- data.frame(matrix(ncol=5, nrow=nrow(transect_start)))
col_names= c("expedition","dive_number","depth_ID","start","transect_duration")
colnames(transect_times) <- col_names

for (i in 1:nrow(transect_start)) {
  # for (i in 1:2) {
  start<- transect_start$date_time[i]
  end<- transect_end$date_time[i]
  depth_ID <- transect_start$depth_ID[i]
  duration<- difftime(transect_end$date_time[i], transect_start$date_time[i])
  
  transect_times$start[i] <- start
  transect_times$end[i] <- end
  transect_times$depth_ID[i] <- depth_ID
  transect_times$transect_duration[i] <- duration
  transect_times$expedition[i] <- transect_start$expedition[i]
  transect_times$dive_number[i] <- transect_start$dive_number[i]
}

# View(transect_times)
midwater_annotations$unix_datetime <- as.numeric(midwater_annotations$date_time)
# View(midwater_annotations)

# join transect duration times to the midwater annotations
midwater_annotations <- dplyr::left_join(midwater_annotations,
                          dplyr::select(transect_times, start, end, transect_duration),
                          dplyr::join_by("unix_datetime"=="start"))
View(midwater_annotations)


#-------------------------------------------------------------------------------

# Now loop to select rows from annotations by dive number, filter, and perform calculations by TRANSECT

for (j in seq(1, length(dive_numbers))){
# for (j in seq(1, 2)){


  print(dive_numbers[j])
  midwater_by_dive <- filter(midwater_annotations, dive_number==dive_numbers[j]) # NOW SUBSET BY DIVE
  # View(midwater_by_dive)
  
  # This SHOULD hopefully work so long as no transect depths are repeated within one dive
  depth_ID <- unique(midwater_by_dive$depth_ID)
  print(depth_ID)
  
  transect_duration_df <- na.omit(dplyr::select(midwater_by_dive, depth_ID, transect_duration, date_time, end))
  # View(transect_duration_df)


  #Overall summary statistics for substrate annotations
  # substrate_annotations <- midwater_annotations |> 
  #   dplyr::filter(taxonomy %in% c("CMECS", "Simplified CMECS")) |> 
  #   dplyr::select("dive_number", "component") |> 
  #   dplyr::group_by(dive_number) |> 
  #   dplyr::summarize(geoform_or_substrate = sum(!is.na(component)))
  # View(substrate_annotations)
  
  #count number of biological annotations that are identified as animals but have
  #no phylum-level identification
  unidentified_animalia <- midwater_by_dive |> 
    dplyr::group_by(depth_ID) |> 
    dplyr::filter(biota == "Biota", is.na(phylum), kingdom == "Animalia") |> 
    dplyr::summarize(Unidentified_Biota = dplyr::n())
  # View(unidentified_animalia)
  
  #sum of biological annotations by taxonomic level
  biological_annotations <- midwater_by_dive |>
    dplyr::filter(biota == "Biota") |> 
    dplyr::select("dive_number","depth_ID","species","genus","family","order","class","phylum") |> 
    dplyr::group_by(depth_ID) |>
    dplyr::summarize(across(phylum:species, \(x) sum(!is.na(x))))
  # View(biological_annotations)

  #Overall summary statistics for biological annotations, counts total biota as
  #the sum of total phylum-level annotations plus the unidentified biota
  biological_annotations <- biological_annotations |> 
    dplyr::left_join(unidentified_animalia, by = dplyr::join_by(depth_ID)) |> 
    dplyr::mutate(Unidentified_Biota = tidyr::replace_na(Unidentified_Biota, 0)) |> 
    dplyr::mutate(total_biota = phylum + Unidentified_Biota)
  
  # ASE calculate number of annotations per minute in each transect 
  # based on number of total biota and transect duration times
  
  transect_duration_df$expedition <- data_name
  transect_duration_df$dive_number <- (midwater_by_dive$dive_number[1])
  transect_duration_df$total_biota <- biological_annotations$total_biota
  transect_duration_df$ann_per_min <- biological_annotations$total_biota / transect_duration_df$transect_duration
  # transect_duration_df$start <- dplyr::filter(transect_times$start, dive_number == dive_numbers[j])
  # transect_duration_df$end <- dplyr::filter(transect_times$end,  dive_number == dive_numbers[j])

  #percentage of annotations flagged for review
  percent_flagged <- midwater_by_dive |> 
    dplyr::group_by(depth_ID) |> 
    dplyr::summarize(percent_flagged = sum(flagged_for_review)/dplyr::n()*100)
  
  #count annotations by dive for major phyla of interest to OER
  interesting_phyla_count <- midwater_by_dive |> 
    dplyr::group_by(depth_ID) |> 
    dplyr::summarize(Echinodermata = sum(phylum == "Echinodermata", na.rm = TRUE),
                     Porifera = sum(phylum == "Porifera", na.rm = TRUE))
  
  #count annotations of Chordata within the Vertebrata subphylum  
  Vertebrata <- midwater_by_dive |>
    dplyr::group_by(depth_ID) |> 
    dplyr::filter(phylum == "Chordata") |>   
    dplyr::filter(! class %in% c("Thaliacea","Ascidiacea", "Appendicularia", "Larvacea")) |>
    tidyr::drop_na(class) |> 
    dplyr::summarize(Vertebrata = dplyr::n())
  
  #count coral annotations using the Deep Sea Coral Program code found here:
  #https://github.com/RobertMcGuinn/deepseatools/blob/master/code/143469.R
  Deep_sea_corals <- midwater_by_dive |> 
    dplyr::group_by(depth_ID) |> 
    dplyr::filter(phylum == "Cnidaria") |>
    dplyr::filter(order == "Scleractinia" |
                    order == "Antipatharia" |
                    genus == "Savalia" |
                    genus == "Kulamanamana" |
                    genus == "Gerardia" |
                    family == "Stylasteridae" |
                    order  == "Alcyonacea" |
                    order ==  "Gorgonacea" |
                    order ==  "Helioporacea" |
                    order == "Pennatulacea" |
                    order == "Scleralcyonacea" |
                    genus == "Solanderia" |
                    genus == "Janaria" |
                    genus == "Hydrocorella" |
                    genus == "Hydrodendron" |
                    order == "Malacalcyonacea") |> 
    dplyr::summarize(Deep_sea_corals = dplyr::n())
  
  #compare relative contributions of observed phyla to counts of total biological
  #annotations
  phyla_frequency <- midwater_by_dive |> 
    dplyr::filter(biota == "Biota") |>
    tidyr::drop_na(phylum) |> 
    dplyr::group_by(depth_ID,phylum) |> 
    dplyr::summarize(count = dplyr::n()) |> 
    dplyr::left_join(y=biological_annotations, by = "depth_ID") |> 
    dplyr::mutate(percent = count/phylum.y*100) |> 
    dplyr::select(depth_ID, phylum = phylum.x, count, percent)
  
  #creates a data frame with every phylum observed in the overall expedition for 
  #each dive with value = 0 filled in - need this for the heatmap visual
  phyla_frequency_percent_all <- phyla_frequency |> 
    dplyr::select(!count) |> 
    tidyr::pivot_wider(names_from = depth_ID, values_from = c(percent), 
                       values_fill = 0) |> 
    tidyr::pivot_longer(!phylum, names_to = "depth_ID", values_to = "percent")
  
  #calculate time on bottom based on benthic start and benthic end columns from
  #the midwater_annotations data frame
  # bottom_time_hours <- midwater_annotations |> 
  #   dplyr::group_by(dive_number) |> 
  #   dplyr::reframe(bottom_time_hours = difftime(benthic_end, benthic_start, 
  #                                         units = "hours")) |> 
  #   dplyr::distinct()
  
  # bottom_time_hours$bottom_time_hours <- as.numeric(bottom_time_hours$bottom_time_hours)
  
  #calculate mean depth during ROV time on bottom
  mean_benthic_depth <- midwater_by_dive |> 
    dplyr::select(dive_number, depth_ID, depth_m) |> 
    dplyr::filter(!is.na(depth_m)) |> 
    dplyr::group_by(depth_ID) |> 
    dplyr::summarize(mean_depth = mean(depth_m))
  

  # species richness per minute
  
  
  
  
  # taxonomic abundance (per minute?)
  
  
  
  #Join counts of biological annotations by taxonomy, counts of interesting phyla,
  #counts of substrate annotations, and ROV dive information based on dive number
  
  summary_statistics <- list(biological_annotations, percent_flagged, 
                             interesting_phyla_count, Vertebrata, Deep_sea_corals, mean_benthic_depth) |> 
    # benthic_start, mean_benthic_depth,bottom_time_hours,substrate_annotations, distance_traveled) |> 
    purrr::reduce(dplyr::left_join, by = c("depth_ID"))
  
  #replace NA with 0 across whole data frame
  summary_statistics[is.na(summary_statistics)] = 0
  summary_statistics <- dplyr::left_join(summary_statistics, transect_duration_df, by = c("depth_ID"))
  #move expedition column
  # summary_statistics <- summary_statistics |> 
  # dplyr::relocate(expedition, .before = dive_number)
  
  # View(summary_statistics)

  write.csv(summary_statistics, paste0(wd, data_name, "/exports/summary_stats/summary_statistics_dive_", dive_numbers[j],
                                       ".csv"),row.names = FALSE)

  write.csv(phyla_frequency_percent_all, paste0(wd, data_name, "/exports/phyla_freq/phyla_frequency_percent_all_dive_", dive_numbers[j],
                                                ".csv"),row.names = FALSE)


}



