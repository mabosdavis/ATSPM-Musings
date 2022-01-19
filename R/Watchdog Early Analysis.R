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

# Filter for just the signals we are using
Workable_Signals <- c(4024, 4028, 4029, 4090, 4165, 4301, 4388, 4389, 4704, 
                      4705, 4706, 6305, 6306, 6307, 6308, 6311, 6313, 6395, 
                      6407, 6408, 6409, 6410, 6411, 6416, 6421, 6423, 6427, 
                      6465, 7207)
wdog_filter <- wdog_consolidate %>%
  filter(wdog_consolidate$SignalID %in% Workable_Signals) %>%
  as.tibble()

# Filter for applicable watchdog messages
Watchdog_Messages <- c("Too Few Records", "Low Advanced Detection Counts", "Could not download", "FTP Not able to Delete File on Controller")
wdog_messages <- wdog_filter %>%
  filter(wdog_filter$types %in% Watchdog_Messages) %>%
  as.tibble()
