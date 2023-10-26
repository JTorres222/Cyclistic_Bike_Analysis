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

# Combine all data frames into one
combined_df <- bind_rows(jun_22, jul_22, aug_22, sep_22, oct_22, nov_22,
                         dec_22, jan_23, feb_23, mar_23, apr_23, may_23)

combined_df %>%
  arrange(member_casual, rideable_type) %>%
  subset(select = -c(start_lat, start_lng, end_lat, end_lng)) %>%
  mutate(date = as.Date(started_at), #The default format is yyyy-mm-dd
  month = format(as.Date(date), "%m"),
  day = format(as.Date(date), "%d"),
  year = format(as.Date(date), "%Y"),
  day_of_week = format(as.Date(date), "%A"),
  ride_length = difftime(ended_at,started_at))

# Inspect the structure of the columns
str(combined_df)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
combined_df$ride_length <- as.numeric(as.character(combined_df$ride_length))
is.numeric(combined_df$ride_length)



#New dataframe with removed ride length negative periods
combined_df_v2 <- combined_df[!(combined_df$ride_length<=0),]


# Inspecting columns with 23 hour or longer ride length period
sorted_long_ride_length <- arrange(combined_df_v2, desc(ride_length))

View(sorted_long_ride_length)



#=====================================  
# DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)


# Calculates summary statistics on the combined data frame
summary(combined_df_v2$ride_length)

# Compares members and casual users
aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual, FUN = mean)
aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual, FUN = median)
aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual, FUN = max)
aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual + combined_df_v2$day_of_week, FUN = mean)


# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual + combined_df_v2$day_of_week, FUN = mean)

# Analyzes ridership data by type and weekday
combined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

# Let's visualize the number of rides by rider type
combined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length),
            .groups = "drop") %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  labs(title = "Number of Rides by Usertype per Weekday") +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
combined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  labs(title = "Average Duration by Usertype per Weekday") +
  geom_col(position = "dodge")

# Count all bike types that casual and members used. Which type of bike do they most likely use?

combined_df_v2 %>%
  group_by(rideable_type, member_casual) %>%
  summarise(count = n())


## What popular stations are used among casual and member types?

#Finding the popular stations for all user types, grouped by station and user type
grouped_by_stations <- combined_df_v2 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(count = n()) %>%
  filter(!is.na(start_station_name)) %>% #Removes NA values from start_station_name
  arrange(factor(member_casual, levels = c("casual", "member")), desc(count))

#Find top ten stations for casual users from grouped_by_stations
casual_top_ten_stations <- grouped_by_stations %>%
  filter(member_casual == "casual") %>%
  arrange(desc(count)) %>%
  head(10)

# Graph for popular stations for casual users
ggplot(data = casual_top_ten_stations, aes(x = reorder(start_station_name, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "coral1") +
  theme_minimal() +
  labs(title = "Top 10 Popular Stations for Casual Users", x = "Start Station", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Finding the top ten stations for members from grouped_by_stations
member_top_ten_stations <- grouped_by_stations %>%
  filter(member_casual == "member") %>%
  arrange(desc(count)) %>%
  head(10)

# Graph for popular stations for member users
ggplot(data = member_top_ten_stations, aes(x = reorder(start_station_name, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "darkturquoise") +
  theme_minimal() +
  labs(title = "Top 10 Popular Stations for Member Users", x = "Start Station", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

##Analyze ride counts and average durations for each month

#Group by month and member type, calculate each ride count for each month
ride_counts_by_type <- combined_df_v2 %>%
  group_by(month, member_casual) %>%
  summarise(total_rides = n())

# Ensures 'month' is recognized as a factor
ride_counts_by_type$month <- factor(ride_counts_by_type$month, levels = c("06", "07", "08", "09", "10", "11", "12", "01", "02", "03", "04", "05"))

#Graph for ride counts by users per month
ggplot(data = ride_counts_by_type, aes(x = month, y = total_rides, color = member_casual, size = member_casual)) + 
  geom_line(aes(group = member_casual)) +
  labs(title = "Total Rides by User Types per Month", x = "month", y = "number_of_rides" ) +
  scale_x_discrete() +
  scale_color_manual(values = c("casual" = "coral", "member" = "darkturquoise")) +
  scale_size_manual(values = c("casual" = 2, "member" = 2))



#Group by month and member type, calculate avg duration for each month
avg_ride_length_by_type <- combined_df_v2 %>%
  group_by(month, member_casual) %>%
  summarise(average_duration = mean(ride_length))

# Ensures 'month' is recognized as a factor
avg_ride_length_by_type$month <- factor(avg_ride_length_by_type$month, levels = c("06", "07", "08", "09", "10", "11", "12", "01", "02", "03", "04", "05"))

#Graph for ride counts by users per month
ggplot(data = avg_ride_length_by_type, aes(x = month, y = average_duration, color = member_casual, size = member_casual)) + 
  geom_line(aes(group = member_casual)) +
  labs(title = "Average Duration by User Types per Month", x = "month", y = "average_duration (seconds)" ) +
  scale_x_discrete() +
  scale_color_manual(values = c("casual" = "coral", "member" = "darkturquoise")) +
  scale_size_manual(values = c("casual" = 2, "member" = 2))


#=================================================
# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Csv file that we will visualize in Excel, Tableau, or any presentation software
counts <- aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual + combined_df_v2$day_of_week, FUN = mean)
write.csv(counts, file = '/Users/jtorres/Documents/Work/Data Analysis/avg_ride_length.csv')

