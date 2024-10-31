# Set up environment

# import libraries
source("key_functions.R")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)


# get and print working directory
currentDir <- getwd() 
print(currentDir)

# list file in the working DIR
list.files(currentDir)

# we are interested in the csv files in 'mturkfitbit_export_3.12.16-4.11.16'
# and 'Fitabase Data 3.12.16-4.11.16'
csv_files_Dir <- file.path(
  currentDir, 'mturkfitbit_export_3.12.16-4.11.16', 'Fitabase Data 3.12.16-4.11.16'
)

csv_files <- list.files(csv_files_Dir)
len_cvs = length(csv_files)



