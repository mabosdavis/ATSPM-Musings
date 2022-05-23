library(tidyverse)

###Read in and clean vms data
# Import VMS Locations
rwis <- read.csv("data/csv/rwis_5-9-22.csv", na.strings = "")

# Extract the route number from the PrimaryLocation
rwis <- rwis %>%
  mutate(route = as.numeric(gsub(".*?([0-9]+).*", "\\1", RoadwayName))) %>%
  mutate(route = str_trim(route, "both"))
# Count the number of characters in the route number
rwis$route_nchar <- nchar(rwis$route)
# Add zeros in front of route number to match the formatting in the RC_editing file
rwis$route_full <- case_when(rwis$route_nchar == 1 ~ str_c("000",rwis$route),
                                    rwis$route_nchar == 2 ~ str_c("00",rwis$route),
                                    rwis$route_nchar == 3 ~ str_c("0",rwis$route))
# Select and reorder useful columns
rwis <- rwis %>%
  select(Name, LinearReference, RoadwayName, route_full, Latitude, Longitude, YearConstructed)

write_rds(rwis, "data/output/rwis_output.rds")


### Combining the vms and segmented files into one. Adds columns from the 
## segments file to the vms file by matching the segmnets row with the given 
## vms row
library(dplyr)

# Read in .rds files
vms <- read_rds("data/output/rwis_output.rds")
segments <- read_rds("data/output/RC_edited.rds")

# Add empty Columns to the vms file
vms$Urban_Description <- 0
vms$Access_Category <- 0
vms$Functional_Class <- 0
vms$Speed_Limit <- 0
vms$Lanes <- 0
vms$AADT2020 <- 0
vms$AADT2019 <- 0
vms$AADT2018 <- 0
vms$SUTRK2020 <- 0
vms$SUTRK2019 <- 0
vms$SUTRK2018 <- 0
vms$CUTRK2020 <- 0
vms$CUTRK2019 <- 0
vms$CUTRK2018 <- 0
vms$error <- FALSE

# Add counters to track any errors
errors <- 0
after_errors <- 0

# Loops through vms to find which rows from segments file match with given row
# in the vms file
for (i in 1:nrow(vms)){
  vmsroute <- vms[["route_full"]][i]
  vmsmp <- vms[["LinearReference"]][i]
  # Stores row in segments that matches vms row based on criteria listed
  rtrow <- which(segments$ROUTE == vmsroute & 
                   segments$BEG_MP < vmsmp  & 
                   segments$END_MP > vmsmp)
  # Only uses the last integer in rtrow. Not sure why rtrow stores more than 1 integer
  rtrow <- tail(rtrow, 1) 
  # Errors accumulate if NA is present in the route_full column of vms_install
  if(length(rtrow) == 0){ 
    errors <- errors + 1
    vms[["error"]][i] <- TRUE
  } else{
    # Adds data from segments to vms based on i and rtrow
    print(rtrow)
    vms[["Urban_Description"]][i] <- segments[["Urban_Description"]][rtrow]
    vms[["Access_Category"]][i]   <- segments[["ACCESS_CATEGORY"]][rtrow]
    vms[["Functional_Class"]][i]  <- segments[["FUNCTIONAL_CLASS"]][rtrow]
    vms[["Speed_Limit"]][i]       <- segments[["SPEED_LIMIT"]][rtrow]
    vms[["Lanes"]][i]          <- segments[["THRU_CNT"]][rtrow]
    vms[["AADT2020"]][i]          <- segments[["AADT2020"]][rtrow]
    vms[["AADT2019"]][i]          <- segments[["AADT2019"]][rtrow]
    vms[["AADT2018"]][i]          <- segments[["AADT2018"]][rtrow]
    vms[["SUTRK2020"]][i]         <- segments[["SUTRK2020"]][rtrow]
    vms[["SUTRK2019"]][i]         <- segments[["SUTRK2019"]][rtrow]
    vms[["SUTRK2018"]][i]         <- segments[["SUTRK2018"]][rtrow]
    vms[["CUTRK2020"]][i]         <- segments[["CUTRK2020"]][rtrow]
    vms[["CUTRK2019"]][i]         <- segments[["CUTRK2019"]][rtrow]
    vms[["CUTRK2018"]][i]         <- segments[["CUTRK2018"]][rtrow]
    # Tracks if the data in the i row and rtrow are the same
    if(segments$ROUTE[rtrow] == vmsroute & 
       segments$BEG_MP[rtrow] < vmsmp  & 
       segments$END_MP[rtrow] > vmsmp){
    } else{
      after_errors <- after_errors + 1
    }
  }
}
# Writes the vms file to the output folder
write_csv(vms, "data/output/RWIS_RC.csv")


errors_list <- vms %>% 
  filter(error == TRUE)
