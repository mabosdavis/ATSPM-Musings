library(tidyverse)

# Import VMS Locations
vms <- read.csv("data/VMS.csv", na.strings = "")
vmsinstall <- read.csv("data/VMS_Install.csv", na.strings = "")

# Select where install dates are present
vms_install <- vmsinstall %>%
  filter(DateInstalled != "")

# Combine the two .csv files
 vmsnew <- left_join(vms_install, vms, by = c("AIMSLocationName" = "Name"))
 write_csv(vmsnew, "data/VMS_LeftJoin.csv")

# Eliminating blanks from CSVs
 write_csv(vms, "data/VMS.csv")
 write_csv(vmsinstall, "data/VMS_Install.csv")



ggplot(cams, aes(x= LastUpdated, y = EntityIntId)) +
  geom_point() +
  xlab("Time") +
  ylab("Cameras")
