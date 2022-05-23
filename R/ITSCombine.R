library(tidyverse)

###Read in and clean vms data
 # Import VMS Locations
 vms_install <- read.csv("data/VMS_Install.csv", na.strings = "")
 # Create column that lists if a Dash is present
 vms_install <- vms_install %>%
   mutate(Dash = str_detect(PrimaryLocation,"-"))
 # Extract the route number from the PrimaryLocation
 vms_install <- vms_install %>%
   mutate(route = ifelse(Dash == TRUE, sub('.+-(.+)', '\\1', PrimaryLocation), "")) %>%
   mutate(route = substr(route, 1, 3)) %>%
   mutate(route = str_replace(route, "/", "")) %>%
   mutate(route = str_trim(route, "both"))
 # Count the number of characters in the route number
 vms_install$route_nchar <- nchar(vms_install$route)
 # Add zeros in front of route number to match the formatting in the RC_editing file
 vms_install$route_full <- case_when(vms_install$route_nchar == 1 ~ str_c("000",vms_install$route),
                                     vms_install$route_nchar == 2 ~ str_c("00",vms_install$route),
                                     vms_install$route_nchar == 3 ~ str_c("0",vms_install$route))
 # Select and reorder useful columns
 vms_install <- vms_install %>%
   select(route_full, Milepost, PrimaryLocation, AIMSLocationName, Latitude, Longitude, DateInstalled)
 
 
 
### Read in and clean segments data if barriga_code_copy.R has just been run
 # Converts Urban Code numbers to a Description
 library(tidyverse)
 RC_edited <- RC %>%
   mutate(Urban_Description = case_when(URBAN_CODE == "99999" ~ "Rural", 
                                        URBAN_CODE == "99998" ~ "Small Urban",
                                        URBAN_CODE == "78499" ~ "Salt Lake City",
                                        URBAN_CODE == "77446" ~ "St. George",
                                        URBAN_CODE == "72559" ~ "Provo-Orem",
                                        URBAN_CODE == "64945" ~ "Ogden-Layton",
                                        URBAN_CODE == "50959" ~ "Logan")) %>%
   # Removes the PM or NM from the Route name
   mutate(ROUTE = substr(ROUTE,1,nchar(ROUTE)-2)) %>%
   # Selects useful columns
   select(ROUTE, BEG_MP, END_MP, URBAN_CODE, Urban_Description, ACCESS_CATEGORY,
          FUNCTIONAL_CLASS, RouteDir, RouteType, SPEED_LIMIT, THRU_CNT, 
          AADT2020, AADT2019, AADT2018, SUTRK2020, SUTRK2019, SUTRK2018, 
          CUTRK2020, CUTRK2019, CUTRK2018)

 # Outputs both as .rds and RC_edited as .csv
 write_csv(RC_edited, "data/output/RC_edited.csv")
 write_rds(RC_edited, "data/output/RC_edited.rds")
 write_rds(vms_install, "data/output/vms_install.rds")
 
 
 
 
 
### Combining the vms and segmented files into one. Adds columns from the 
 ## segments file to the vms file by matching the segmnets row with the given 
 ## vms row
 library(dplyr)
 
 # Read in .rds files
 vms <- read_rds("data/output/vms_install.rds")
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
   vmsmp <- vms[["Milepost"]][i]
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
write_csv(vms, "data/output/VMS_RC.csv")


errors_list <- vms %>% 
  filter(error == TRUE)
