# Set up environment
# import libraries
library(tidyverse)
# install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(dplyr)

# Check for missing values
missing_value <- function(df, file){
  
  # Initialize an empty list that will be a key value pair
  # It will hold df_name with missing values and the specific column with the missing values
  df_with_missing_values <- list()
  
  if (sum(is.na(df)) == 0){
    
    print(paste(file, "Has no missing values"))
    
  }else if (sum(is.na(df)) > 0){
    
    
    #append df_name with missing vales to df_with_missing_values
    df_with_missing_values[[file]] <- c() # initialize an empty vector to store
    #column names
    
    
    #get df columns with missing values
    colNames = colnames(df)
    
    for (col in colNames){
      if (sum(is.na(df[[col]]))  > 0){
        
        df_with_missing_values[[file]] <- c(df_with_missing_values[[file]], col)
        
        print(paste("Total missing values in", file, col, "is",
                    sum(is.na(df[col]))))
      }
    }
    
  }
  
  return(df_with_missing_values)
}



# get number of unique values
unique_value <- function(df, column_of_interest) {
  #get the unique values
  uniques <- unique(df[[column_of_interest]])
  
  # Get the unique values from the specified column
  n_unique <- length(uniques)
  
  print(paste(column_of_interest, "has",  n_unique, "values"))
  
}


# Let's check if we are dealing with the same clients
uniqueIDs_Comparison <- function (df1, column1, df2, column2){
  missing_ids = c()
  
  initial_list = unique(df1[[column1]])
  comparison_list = unique(df2[[column2]])
  
  
  for (i in initial_list){
    if (!(i %in% comparison_list)){
      missing_ids <- c(missing_ids, i)
      
    }
  }
  
  for (i in comparison_list){
    if (!(i %in% initial_list)){
      missing_ids <- c(missing_ids, i)
      
    }
    
  }
  
  if (length(missing_ids) == 0){
    print("Dataframes have similar Id values")
    
    
  }else{
    print("Dataframes have different Id values")  
    print(paste("No. of missing values", length(unique(missing_ids))))
  }
  
  return(unique(missing_ids))
}





# Get the length of values in a column
N_unique_char <- function(df, column_of_interest){
  # Initialize/ pre-allocate a numeric vector of a specific length, initialized with   zeros
  n_char <- numeric(nrow(df)) 
  
  for (i in seq_along(df[[column_of_interest]])){
    n_char[i] <- nchar(df[[column_of_interest]][i])
  }
  
  # get the number of unique characters
  character_lens <- unique(n_char)
  
  return(character_lens)
}





change_to_dateTime <- function(df, column_of_interest){
  
  Values_Ncha_Column = N_unique_char(df, column_of_interest)
  
  #Check if all elements in the vector Values_Ncha_Column are either 8 or 9
  if (all(Values_Ncha_Column %in% c(8, 9))){
    df[[column_of_interest]] <- 
      as.POSIXct(
        df[[column_of_interest]],
        format="%m/%d/%Y",
        tz=Sys.timezone()
      )
    
  }else if (!all(Values_Ncha_Column
                 %in% c(8, 9))){
    
    df[[column_of_interest]] <- 
      as.POSIXct(
        df[[column_of_interest]],
        # %I: Hour (01–12, for 12-hour clock)
        # %p: AM/PM marker
        format = "%m/%d/%Y %I:%M:%S %p", 
        tz=Sys.timezone()
      )
  }
  
  # confirm the datatype of the column of interest
  #print(paste(column_of_interest, "has" ,class(df[[column_of_interest]]),
  #"datatype"))
  
  print(head(df[column_of_interest], 5))
  print(tail(df[column_of_interest], 5))
  
  return(df)
  
}



# Check if the columns are identical
identical_columns <- function(df, col1, col2){
  if (identical(df[[col1]], df[[col2]])) {
    print("The columns are identical.")
  } else {
    print("The columns are NOT identical.")
  } 
}



# Merge fucntion which we'll use with the reduce inbuilt function
merge_dfs <- function(df1, df2){
  merged_df <- merge(df1, df2, by = "Id")
  
  return(merged_df)
}

# calculate column averages
Avgs <- function(df, column_names_vector){
  
  # initialize and empty list
  column_Avgs = list()
  
  for (i in column_names_vector){
    # get the data of the column
    column_data <- df[, c("Id", i)]
    
    
    avg_per_athlete <- column_data %>% 
      group_by(Id) %>% 
      summarise(
        # use "get" to retrieves the variable from key which is i and 
        # Rename the column_Avgs keys to avoid conflicts
        !!paste0("Avg_", i) := mean(get(i), na.rm = TRUE) 
      )
    
    
    column_Avgs[[i]] = avg_per_athlete
    
    
  }
  
  # Merge DataFrames on the 'ID' column
  # How the reduce function works
  # 1.Reduce will take the first two data frames from column_Avgs and
  #pass them to merge_dfs.
  # 2. The result of the first merge will be passed as the first argument
  # to merge_dfs for the next data frame in the list, continuing this process until
  #all data frames have been merged.
  
  # Here the reduce function and
  merged_df <- Reduce(merge_dfs, column_Avgs)
  
  return(merged_df)
}




# line plot function 
line_plot <- function(df, x_col, y_col, i = NULL) {
  # Determine the title based on whether 'i' is provided
  plot_title <- if (!is.null(i)) {
    paste(i, x_col, "and", y_col, "relationship")
  } else {
    paste(x_col, "and", y_col, "relationship")
  }
  
  # Create the plot
  plot <- ggplot(data = df) +
    geom_line(mapping = aes(x = .data[[x_col]], y = .data[[y_col]])) +
    labs(title = plot_title)
  
  # Print the plot
  print(plot)
}



weekDate_grouping <- function(df, weekOrdate){
  # get group by the 1st 2 columns which will be "Id and column with POSIXct
  #datatype"
  grouping_Cols <- c(colnames(df)[1:2])
  summarise_Cols <- c(colnames(df)[-1:-2])
  
  # Check if the first column is "Id" and the second column is of type POSIXct
  if (grouping_Cols[1] == "Id" && inherits(df[[grouping_Cols[2]]], "POSIXct")){
    
    
    # Determine grouping based on the 'weekOrdate' parameter
    if(weekOrdate == "week"){
      # Group by week number
      df <- df %>%
        mutate(Group = format(.data[[grouping_Cols[2]]], "%Y-%U"))
    }else if(weekOrdate == "date"){
      df <- df %>% 
        mutate(Group = as.Date(.data[[grouping_Cols[2]]]))
    }else {
      stop("Invalid string value for 'weekOrdate'. Use 'week' or 'date'.")
    }
    
    # If the above run successfully perform grouping and summarization
    df <- df %>%
      group_by(.data[[grouping_Cols[1]]], Group) %>%
      summarize(
        across(
          all_of(summarise_Cols),
          list(
            Total = ~sum(.x, na.rm = TRUE),
            Avg = ~mean(.x, na.rm = TRUE)
          ),
          #{col} acts as a Placeholder for the name of the original column being
          #processed
          #{fn}: Placeholder for the name of the function applied (e.g., "Total"
          #or "Avg").
          .names = "{col}_{fn}"
        ),
        .groups = "drop"
      )
    
  }else{
    stop("Please check the name and datatype of the 1st 2 columns.")
  }
  
  return(df)
}



random_plots <- function(df, fraction_of_all_len_df, X_col, Y_col){
  
  IDS <- c(unique(df[["Id"]]))
  
  num_samples <- ceiling(length(IDS) * fraction_of_all_len_df)
  random_Ids <- sample(IDS, size = num_samples, replace = FALSE)
  
  for (i in random_Ids){
    filtered_df <- df %>%
      filter(Id == i)
    line_plot(filtered_df,
              X_col, Y_col, i
    )
  }
}
