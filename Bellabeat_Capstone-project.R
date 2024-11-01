# Set up environment

# import libraries
source("Functions.R")
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



### Read dataframes
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
  df <- read.csv(filepath)
  
  #Check if df has missing value
  missing_value(df = df, file = file)
  
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


# call function on dailyActivity_merged_df
unique_value(dailyActivity_merged_df, "Id")


#See the number of character in each character of the ActivityDate column
# since we've already seen there are inconsistencies in the ActivityDate, -->
# let's see if there are some date characters saved with hrs,min,and secs



dailyActivity_merged_df <- change_to_dateTime(dailyActivity_merged_df,
                                              "ActivityDate")


# call identical_columns on dailyActivity_merged_df
identical_columns(dailyActivity_merged_df, "TotalDistance", "TrackerDistance" )


error_df <-  dailyActivity_merged_df[, c('Id', 'TotalDistance',
                                         'TrackerDistance')]
Id_s <- c() 
difference <- c()
total_distance <- c()

for (i in 1:nrow(error_df)){
  
  Id_i <-  error_df[["Id"]][i]
  total_dist_i <- error_df[["TotalDistance"]][i]
  tracker_dist_i <- error_df[["TrackerDistance"]][i]
  
  
  error <-total_dist_i - tracker_dist_i
  if(error != 0){
    Id_s <- c(Id_s, Id_i)
    difference <- c(difference, error)
    total_distance <- c(total_distance, total_dist_i)
    
  }
  
}

# see how many of all the athletes have faulty devices
print(length(unique(Id_s)))

# If 5 out of all athletes are the only ones with faulty devices
# see what percentage of Bellabeat devices are faulty
Number_of_clients <- length(unique(dailyActivity_merged_df[['Id']]))
Client_with_faulty_devices <- length(unique(Id_s))


print(
  paste(
    "Percentage of devices with errors",
    round((Client_with_faulty_devices/Number_of_clients) * 100, 2)
  )
)

# calculate error percentage
percentage_error <- (sum(abs(difference))/ sum(total_distance)) *100
print(
  paste(
    "Faulty Devices have a percentahe error of",
    round(percentage_error, 2)
  )
)


# Call the Avgs function to get the averages of some columns in dailyActivity_merged_df
column_names <- c(names(dailyActivity_merged_df)[3:10], tail(names(dailyActivity_merged_df), 1))

averages_df = Avgs(dailyActivity_merged_df, column_names)

# Write the data frame to a CSV file
write.csv(averages_df, file = "customer_averages.csv", row.names = FALSE)



# Check the correlation between TotalDistance and Calories
# call function on dailyActivity_merged_df 
line_plot(df = dailyActivity_merged_df, x_col= "Calories", y_col = "TotalDistance")


heartrate_seconds_merged_df <- dfs[["heartrate_seconds_merged_df"]]
str(heartrate_seconds_merged_df)

head(heartrate_seconds_merged_df, 5)
tail(heartrate_seconds_merged_df, 5)



# Confirm if we are dealing with the same athletes.
# Check if the unique Ids are similar to the ones in dailyActivity_merged_df
IDs <- uniqueIDs_Comparison(df1 = dailyActivity_merged_df, column1 = "Id",
                            df2 = heartrate_seconds_merged_df, column2 = "Id")


# Change the time columns to datetime
heartrate_seconds_merged_df <- change_to_dateTime(heartrate_seconds_merged_df, "Time")



# see the average heart rate per unique id, see the correlation between the average heart rate and calories lost
# calculate averages
average_hr_per_athlete <- Avgs(heartrate_seconds_merged_df, c(names(heartrate_seconds_merged_df)[3]))


new_col_name = names(average_hr_per_athlete)[2]

print(average_hr_per_athlete)
print(max(average_hr_per_athlete[[new_col_name]]))  
print(min(average_hr_per_athlete[[new_col_name]])) 
print(median(ceiling(average_hr_per_athlete[[new_col_name]]), na.rm = TRUE))
(mean(ceiling(average_hr_per_athlete[[new_col_name]])))



# Increase bottom margin to fit x-axis labels(Clients/Athletes) better
par(mar = c(8, 5, 4, 2) + 0.1)  


boxplot(Value ~ Id,
        data = heartrate_seconds_merged_df,
        main =" Different boxplots for each athlete's heartrate",
        xlab = " ",
        ylab = "Hearteate Frequency",
        col = "orange",
        #main = "Perpendicular",
        las = 2 
        #horizontal = TRUE
        #border="brown"
)


hourlyCalories_merged_df <- dfs[['hourlyCalories_merged_df']]
str(hourlyCalories_merged_df)


# Check if the unique Ids are similar to the ones in dailyActivity_merged_df
uniqueIDs_Comparison(dailyActivity_merged_df, "Id",  hourlyCalories_merged_df, "Id")


# Change to datetime
hourlyCalories_merged_df <-  change_to_dateTime(hourlyCalories_merged_df, "ActivityHour")


hourlyIntensities_merged_df <- dfs[["hourlyIntensities_merged_df"]]
str(hourlyIntensities_merged_df)


head(hourlyIntensities_merged_df, 5)


column1 = "Id"
column2 = "Id"
uniqueIDs_Comparison(hourlyCalories_merged_df, column1,
                     hourlyIntensities_merged_df,
                     column2)

hourlyIntensities_merged_df <- change_to_dateTime(hourlyIntensities_merged_df, "ActivityHour")


# confirm that the ActivityHour in both hourlyCalories_merged_df and hourlyIntensities_merged_df are similar
column1 = "ActivityHour"
column2 = "ActivityHour"
non_unifom_dates =uniqueIDs_Comparison(hourlyCalories_merged_df, column1,
                                       hourlyIntensities_merged_df,column2)


head_values <-  (head(non_unifom_dates, 5))
tail_values <- (tail(non_unifom_dates, 5))

print(head_values)
print(tail_values)


# Join dataframes
CaloriesIntensities_df <- hourlyCalories_merged_df%>%inner_join(hourlyIntensities_merged_df, 
                                                                by=c('Id','ActivityHour'))


head(CaloriesIntensities_df, 5)


if(nrow(hourlyCalories_merged_df) == nrow(CaloriesIntensities_df) &&
   (nrow(hourlyCalories_merged_df) == nrow(hourlyIntensities_merged_df))){
  
  print("Dataframes have same number of rows")
  
}

hourlySteps_merged_df <- dfs[["hourlySteps_merged_df"]]
str(hourlySteps_merged_df)


hourlySteps_merged_df <- change_to_dateTime(hourlySteps_merged_df, "ActivityHour")


CaloriesIntensitiesSteps_df <- CaloriesIntensities_df%>%inner_join(
  hourlySteps_merged_df, by=c('Id', 'ActivityHour'))

head(CaloriesIntensitiesSteps_df, 5)


if(nrow(CaloriesIntensitiesSteps_df) == nrow(CaloriesIntensities_df) &&
   (nrow(CaloriesIntensitiesSteps_df) == nrow(hourlySteps_merged_df))){
  
  print("Dataframes have same number of rows")
  
}

# plot a line plot see see the correlation between calories, intensity and steps
line_plot(CaloriesIntensitiesSteps_df, "Calories", "TotalIntensity")


line_plot(CaloriesIntensitiesSteps_df, "Calories", "StepTotal")


line_plot(CaloriesIntensitiesSteps_df, "Calories", "AverageIntensity")


CaloriesIntensitiesSteps_Daily_df <-
  weekDate_grouping(CaloriesIntensitiesSteps_df,"date")
head(CaloriesIntensitiesSteps_Daily_df,5)


random_plots(df = CaloriesIntensitiesSteps_Daily_df,
             fraction_of_all_len_df = 0.3,
             X_col = "TotalIntensity_Total",
             Y_col ="Calories_Total")



random_plots(df = CaloriesIntensitiesSteps_Daily_df,
             fraction_of_all_len_df = 0.3,
             X_col = "StepTotal_Total",
             Y_col ="Calories_Total")



CaloriesIntensitiesSteps_Weekly_df <-
  weekDate_grouping(CaloriesIntensitiesSteps_df, "week")


random_plots(df = CaloriesIntensitiesSteps_Weekly_df,
             fraction_of_all_len_df = 0.3,
             X_col = "TotalIntensity_Total",
             Y_col = "Calories_Total")


random_plots(df = CaloriesIntensitiesSteps_Weekly_df,
             fraction_of_all_len_df = 0.1,
             X_col = "StepTotal_Total",
             Y_col = "Calories_Total")


# Loop through the list and see if key has minute in its name
minutes_dfs = list()

for (key in names(dfs)) {
  
  if (grepl("minute", key, ignore.case = TRUE)){
    
    print(paste("working on", key))
    
    col_Name <- colnames(dfs[[key]])
    column_of_interest <- col_Name[2]
    
    df <- change_to_dateTime(dfs[[key]], column_of_interest)
    
    minutes_dfs[[key]] <- df
  }
  
}



minuteSleep_merged_df <- minutes_dfs[["minuteSleep_merged_df"]]
print(head(minuteSleep_merged_df))

# See that hour are people most asleep
# change the date column to time date
unique(minuteSleep_merged_df[['value']])
