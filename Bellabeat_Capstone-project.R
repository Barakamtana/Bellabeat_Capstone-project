# Set up environment

# import libraries
library(tidyverse)

# print working directory
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

# csv_files <- list.files(csv_files_Dir, pattern = "\\.csv$", full.names = TRUE)
# print(csv_files) 

# Initialize an empty list to store data frames
dfs <- list()


for (file in csv_files) {
  
  print(paste("Working on", file))
  
  # create df names
  # split the file name str character
  df_name <- strsplit(file, split = '\\.')[[1]] #Access the first and only string
  
  # get the first part of the string character which is basically the name 
  # without the csv extension
  df_name <- df_name[1]
  
  # concatenate the df_name with df
  df_name <- paste0(df_name, "_df") # Use paste0 for no space between parts
  
  # create full path for each file so that we can import them
  filepath <- file.path(csv_files_Dir, file)
  
  # read csv 
  df <- read.csv((filepath))
  
  # append dfs and their names
  dfs[[df_name]] <- df  # Store the data frame in the list, keyed by file path

}


# Confirm that all df were read successfully
if (len_cvs == length(dfs)) {
  print("All files read successfully")
} else {
  print("Some files were not read correctly")
}


dailyActivity_merged_df <-dfs[["dailyActivity_merged_df"]]
str(dailyActivity_merged_df)

# we'll have a look at all unique value in if non of them is repeated
# change ActivityDate from character to datetime
# Assuming TotalSteps is cadance we'll see the average length of a step per person
# See if TotalDistance and TrackerDistance distance record the same data
# Calculate average TotalDistance, TrackerDistance, VeryActiveDistance, 
# ModeratelyActiveDistance, LightActiveDistance, SedentaryActiveDistance,
# Average callories lost per day

heartrate_seconds_merged_df <- dfs[["heartrate_seconds_merged_df"]]
str(heartrate_seconds_merged_df)

# Check if the unique Ids are similar to the ones in dailyActivity_merged_df
# change time column to datetime
# see the average heart rate per unique id, see the correlation between the average heart rate and calories lost
# see if there is any negative hr values

hourlyCalories_merged_df <- dfs[['hourlyCalories_merged_df']]
str(hourlyCalories_merged_df)

# Check if the unique Ids are similar to the ones in dailyActivity_merged_df
# see average calories lost each hour.
# Check which our are people most active
# change ActivityHour to datetime

hourlyIntensities_merged_df <- dfs[["hourlyIntensities_merged_df"]]
str(hourlyIntensities_merged_df)

# Check if the unique Ids are similar to the ones in dailyActivity_merged_df 
# and hourlyCalories_merged_df and hourlyCalories_merged_df since they have similar number row
# merge with hourlyCalories_merged_df see intensity and calories correlation on the same plot
# see AverageIntensity and Calories corelaion

hourlySteps_merged_df <- dfs[["hourlySteps_merged_df"]]
str(hourlySteps_merged_df)

# Check if the unique Ids are similar to the ones in hourlyCalories_merged_df
# merge with  hourlyCalories_merged_df
# see the at what time are people most active 
# see correlation between steps and calories

minuteCaloriesNarrow_merged_df <-  dfs[["minuteCaloriesNarrow_merged_df"]]
str(minuteCaloriesNarrow_merged_df)
# see if this is is a reflection heartrate_seconds_merged_df by deviding the 
# seconds occurrences and see of they will be equivalent to minuteCaloriesNarrow_merged_df
# occurrences

minuteIntensitiesNarrow_merged_df <- dfs[["minuteIntensitiesNarrow_merged_df"]]
str(minuteIntensitiesNarrow_merged_df)
# similarly check if the id are similar to those in minuteCaloriesNarrow_merged_df
# check the correlation between Intensity and calories from minuteCaloriesNarrow_merged_df
# merge this dfs as well
# see then people are most active

minuteMETsNarrow_merged_df <-  dfs[["minuteMETsNarrow_merged_df"]]
str(minuteMETsNarrow_merged_df)

# I'd like to assume MET stands for metabolic equivalent ie
# where One metabolic equivalent (MET) 
# is defined as the amount of oxygen consumed while sitting at rest 
# and is equal to 3.5 ml O2 per kg body weight x min.

minuteSleep_merged_df <- dfs[["minuteSleep_merged_df"]]
str(minuteSleep_merged_df)

# See that hour are people most asleep
# change the date column to time date

minuteStepsNarrow_merged_df <- dfs[["minuteStepsNarrow_merged_df"]]
str(minuteStepsNarrow_merged_df)

# merge with minuteSleep_merged_df

weightLogInfo_merged_df <- dfs[["weightLogInfo_merged_df"]]
str(weightLogInfo_merged_df)
# see the correlation between weight and fat and and BMI

