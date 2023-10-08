setwd("~/Documents")
getwd()
jun_22 <- read_csv("Work/Data Analysis/Bike Share Case Study/202206-divvy-tripdata.csv")
jul_22 <- read_csv("Work/Data Analysis/Bike Share Case Study/202207-divvy-tripdata.csv")
aug_22 <- read_csv("Work/Data Analysis/Bike Share Case Study/202208-divvy-tripdata.csv")
sep_22 <- read_csv("Work/Data Analysis/Bike Share Case Study/202209-divvy-publictripdata.csv")
oct_22 <- read_csv("Work/Data Analysis/Bike Share Case Study/202210-divvy-tripdata.csv")
nov_22 <- read_csv("Work/Data Analysis/Bike Share Case Study/202211-divvy-tripdata.csv")
dec_22 <- read_csv("Work/Data Analysis/Bike Share Case Study/202212-divvy-tripdata.csv")
jan_23 <- read_csv("Work/Data Analysis/Bike Share Case Study/202301-divvy-tripdata.csv")
feb_23 <- read_csv("Work/Data Analysis/Bike Share Case Study/202302-divvy-tripdata.csv")
mar_23 <- read_csv("Work/Data Analysis/Bike Share Case Study/202303-divvy-tripdata.csv")
apr_23 <- read_csv("Work/Data Analysis/Bike Share Case Study/202304-divvy-tripdata.csv")
may_23 <- read_csv("Work/Data Analysis/Bike Share Case Study/202305-divvy-tripdata.csv")


# PREPARE AND PROCESS DATA
#=====================================

#Prints unique values in a specified column
values_in_column <- unique(jul_22$rideable_type)
print(values_in_column)

#Vector containing all the datasets
datasets <- c(
  "jun_22", "jul_22", "aug_22", "sep_22", "oct_22", "nov_22",
  "dec_22", "jan_23", "feb_23", "mar_23", "apr_23", "may_23" 
  )

# Loop to apply sorting operation to each dataset
sorted_datasets <- lapply(datasets, function(name) {
  dataset <- get(name)
  sorted_dataset <- dataset %>%
    select(ride_id, rideable_type, started_at, ended_at, start_station_name, end_station_name, member_casual) %>%
    arrange(member_casual)
  return(sorted_dataset)
})

# Naming the list elements for easier reference
names(sorted_datasets) <- datasets

#Accessing all sorted datasets
sorted_jun_22 <- sorted_datasets$jun_22
sorted_jul_22 <- sorted_datasets$jul_22
sorted_aug_22 <- sorted_datasets$aug_22
sorted_sep_22 <- sorted_datasets$sep_22
sorted_oct_22 <- sorted_datasets$oct_22
sorted_nov_22 <- sorted_datasets$nov_22
sorted_dec_22 <- sorted_datasets$dec_22
sorted_jan_23 <- sorted_datasets$jan_23
sorted_feb_23 <- sorted_datasets$feb_23
sorted_mar_23 <- sorted_datasets$mar_23
sorted_apr_23 <- sorted_datasets$apr_23
sorted_may_23 <- sorted_datasets$may_23

#Arrange Jun Data by descending date in the ended at column to locate the max dates
ended_at_sorted_jun_22 <- sorted_jun_22 %>%
  arrange(desc(ended_at))

#Empty list to store modified dataframes
modified_datasets <- list()

for(name in datasets) {
  #Access the sorted dataset from sorted_datasets list
  dataset <- sorted_datasets[[name]]
  
  #Prints data summary for all sorted datasets
  #print(skim_without_charts(dataset))
  
  # Calculate ride length as a period object
  dataset$ride_length_period <- seconds_to_period(dataset$ended_at - dataset$started_at)
  
  # Extract hours, minutes, and seconds components
  hours <- hour(dataset$ride_length_period)
  minutes <- minute(dataset$ride_length_period)
  seconds <- second(dataset$ride_length_period)
  
  # Format ride length as HH:MM:SS with leading zeros
  dataset$ride_length_hms <- paste(
    sprintf("%02d", hours),
    sprintf("%02d", minutes),
    sprintf("%02d", seconds),
    sep = ":"
  )
  
  # Remove the original ride_length_period column
  dataset <- dataset[, -which(names(dataset) == "ride_length_period")]
  
  #Calculate day of the week each ride started and add it as a new column
  dataset$day_of_week <- wday(dataset$started_at)
  
  #Store modified dataframe in the list
  modified_datasets[[name]] <- dataset
}



view(modified_datasets$jun_22) -> modified_jun22
view(modified_datasets$jul_22) -> modified_jul22
view(modified_datasets$aug_22) -> modified_aug22
view(modified_datasets$sep_22) -> modified_sep22
view(modified_datasets$oct_22) -> modified_oct22
view(modified_datasets$nov_22) -> modified_nov22
view(modified_datasets$dec_22) -> modified_dec22
view(modified_datasets$jan_23) -> modified_jan23
view(modified_datasets$feb_23) -> modified_feb23
view(modified_datasets$mar_23) -> modified_mar23
view(modified_datasets$apr_23) -> modified_apr23
view(modified_datasets$may_23) -> modified_may23


# Combine "member_casual" columns from all datasets into one vector
member_casual_combined <- unlist(lapply(modified_datasets, function(dataset) dataset$member_casual))

# Create a frequency table for the combined "member_casual" vector
frequency_table <- table(member_casual_combined)

# Print the frequency table
print(frequency_table)

#Function that adds date, month, day, year of each ride
#and modifies ride_length_hms column
modified_dates_function <- function(element) {
  
  #Adds date, monthm day, year columns
  element$date <- as.Date(element$started_at)
  element$month <- format(as.Date(element$date), "%m")
  element$day <- format(as.Date(element$date), "%d")
  element$year <- format(as.Date(element$date), "%A")
  
  #Modifies ride_length_hms column
  element$ride_length_period <- hms(element$ride_length_hms)
  
  return(element)
}

#modified_dates_function applied to modified_dates dataframe list
modified_dates <- lapply(modified_datasets, modified_dates_function)



#Function to find HQ QR in start_station_name for each dataframe
find_value_in_column <- function(df_list, column_name, value) {
  # Create an empty list to store results
  results <- list()
  
  # Loop through each data frame in the list
  for (i in seq_along(df_list)) {
    # Extract the current data frame
    current_df <- df_list[[i]]
    
    # Find rows where the specified value exists in the specified column
    matching_rows <- current_df[current_df[[column_name]] == value, ]
    
    # Store the matching rows in the results list
    results[[i]] <- matching_rows
  }
  
  # Return the list of matching rows for each data frame
  return(results)
}

matching_rows_list <- find_value_in_column(modified_dates, "start_station_name", "HQ QR")
print(matching_rows_list)


# Combine all data frames into one
combined_df <- bind_rows(modified_dates)

#New dataframes with removed ride length negative periods
combined_df_v2 <- combined_df[!(combined_df$ride_length_period<=0),]

# Finding rows with 0 in ride_length_period
rows_with_zero_length <- combined_df_v2[combined_df_v2$ride_length_period == as.period(0), ]
print(rows_with_zero_length)

## Inspecting columns with 23 hour or longer ride length period
sorted_long_ride_length <- arrange(combined_df_v2, desc(ride_length_period))

View(sorted_long_ride_length)

  
# DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length_seconds (all figures in seconds)

#ride_length_seconds numberic column for calculations
combined_df_v2$ride_length_seconds <- as.numeric(combined_df_v2$ride_length_period)

# Calculates summary statistics on the combined data frame
summary(combined_df_v2$ride_length_seconds)

# Compares members and casual users
aggregate(combined_df_v2$ride_length_seconds ~ combined_df_v2$member_casual, FUN = mean)
aggregate(combined_df_v2$ride_length_seconds ~ combined_df_v2$member_casual, FUN = median)
aggregate(combined_df_v2$ride_length_seconds ~ combined_df_v2$member_casual, FUN = max)
aggregate(combined_df_v2$ride_length_seconds ~ combined_df_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(combined_df_v2$ride_length_seconds ~ combined_df_v2$member_casual + combined_df_v2$day_of_week, FUN = mean)

# Analyzes ridership data by type and weekday
combined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_seconds)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

# Let's visualize the number of rides by rider type
combined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_seconds)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
combined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_seconds)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


