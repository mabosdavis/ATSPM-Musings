library(tidyverse)

wdog <- read.csv("data/WatchdogReport.csv")

# List of Unique Watchdog Messages
wdog_list <- as.tibble(unique(wdog$Message))

# Make TimeStamp column a date
wdog <- wdog %>%
  mutate(TimeStamp = as.Date(TimeStamp))

# Add 'types' column as a category for the types of Watchdog Messages
wdog_consolidate <- wdog %>%
  mutate(types = ifelse(str_sub(Message, 1, 7) == "Missing", "Too Few Records", 
                        ifelse(str_sub(Message, 1, 5) == "Force", "Force Offs", 
                               ifelse(str_sub(Message, 1, 3) == "Max", "Max Outs",
                                      ifelse(str_sub(Message, -11, -1) == "Activations", "Pedestrian Activations",
                                             ifelse(str_sub(Message, 1, 3) == "CH:", "Low Advanced Detection Counts",
                                                    ifelse(str_sub(Message, 1, 5) == "Could", "Could not download",
                                                           ifelse(str_sub(Message, 1, 3) == "FTP", "FTP Not able to Delete File on Controller",
                                                           ""))))))))
