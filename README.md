<h1>Cyclistic Bike Share Analysis</h1>



<h2>Business Task</h2>
The goal of this project is to analyze how annual members and casual riders of a bike share company in Chicago use cyclistic bikes differently in order to design marketing strategies aimed at converting casual riders to annual members. Data over a period of 12 months would be explored and sourced from Cyclistic's historical bike trip records. A detailed report with desired deliverables, which includes data cleaning and manipulation documentation, analysis summary, supporting visualizations, key findings, and top three recommendations, will be produced.

<br />


<h2>Description of Data Source</h2>
The data source I used is historical trip data made available by Motivate International Inc. I focused on data from a period of 12 months dating from June 2022 to May 2023. Each monthly dataset contains ride ids, type of bike used, start and end datetimes, start and end station names, start and end station ids, latitude and longitude coordinates for start and end stations, and the type of user (either casual or member). The data is public. Data privacy issues prohibit the use of riders personal information such as credit card numbers. 

<h2>Documentation of Data Cleaning/Transformation </h2>

## Version 1.0.0 (09-29-2023)

New

- Added new seperate columns; date, month, day, year, day_of_week and ride_length (difference between ended_at and started_at columns)

Changes

- Combined all dataframes into one large dataframe, combined_df
- Arranged dataframe by type of member_casual (casual or member) and rideable_type (classic, electric or docked)
- ride_length column converted from Factor to numeric so we can run calculations

Removed

- Removed latitude and longitude columns for start and end stations (start_lat, start_lng, end_lat, end_lng))
- Removed ride_lengths with negative periods, stored dataframe into combined_df_v2

Rcode

```r
setwd("~/Documents") #Setting work directory
getwd() #Getting current work directory

#Reading and importing datasets into R dataframes
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


# Combine all data frames into one
combined_df <- bind_rows(jun_22, jul_22, aug_22, sep_22, oct_22, nov_22,
                         dec_22, jan_23, feb_23, mar_23, apr_23, may_23)

#View combined dataframe
View(combined_df)

#Cleaning and Manipulation of the combined dataframe
combined_df <- combined_df %>%
  arrange(member_casual, rideable_type) %>%
  subset(select = -c(start_lat, start_lng, end_lat, end_lng)) %>%
  mutate(date = as.Date(started_at), #The default format is yyyy-mm-dd
  month = format(as.Date(date), "%m"),
  day = format(as.Date(date), "%d"),
  year = format(as.Date(date), "%Y"),
  day_of_week = format(as.Date(date), "%A"),
  ride_length = difftime(ended_at,started_at))

# Inspecting the structure of the columns
str(combined_df)

# Converting "ride_length" from Factor to numeric so we can run calculations on the data
combined_df$ride_length <- as.numeric(as.character(combined_df$ride_length))
is.numeric(combined_df$ride_length)

#Result is TRUE


#New dataframe with removed ride length negative periods
combined_df_v2 <- combined_df[!(combined_df$ride_length<=0),]


# Inspecting columns with 23 hour or longer ride length period
sorted_long_ride_length <- arrange(combined_df_v2, desc(ride_length))

View(sorted_long_ride_length)



```

<h2>Analysis</h2>

```r
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
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual + combined_df_v2$day_of_week, FUN = mean)

# Analyzes ridership data by type and weekday
combined_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()		                   #calculates the number of rides and average duration 
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
# EXPORT SUMMARY FILE
#=================================================
# Csv file
counts <- aggregate(combined_df_v2$ride_length ~ combined_df_v2$member_casual + combined_df_v2$day_of_week, FUN = mean)
write.csv(counts, file = '/Users/jtorres/Documents/Work/Data Analysis/avg_ride_length.csv')



```

<h2>Results</h2>
<img width="703" alt="Screen Shot 2023-10-28 at 3 50 15 PM" src="https://github.com/JTorres222/Cyclistic_Bike_Analysis/assets/81725586/afdc241c-e413-4975-bb90-b30ee8b2a99c">

Average ride length in seconds. Average ride length is the same as average ride duration, which is shown in the graph below.

![average_duration_weekday](https://github.com/JTorres222/Cyclistic_Bike_Analysis/assets/81725586/b86ad2ff-43d5-4b1f-921c-16279a1a4ce8)

Average ride durations visualized through this graph show that casual members typically have a higher average duration within all days during the week. In other words, they ride longer on average. Both Users show longer durations during the weekends with members just slightly above the average range within weekdays. The data suggests the importance of understanding these user-specific weekly trends to optimize bike-sharing services and effectively allocate resources based on varying usage patterns.

![Number_rides_weekday](https://github.com/JTorres222/Cyclistic_Bike_Analysis/assets/81725586/96cf44a7-7ce6-4af8-bb81-b1a000f06604)

Based on the graph above, members ride more frequently than casual riders, especially on weekdays. On weekends, both users ride about the same amount, with just a tiny bit difference. During the weekdays, members ride a lot more frequently than casual users, above 400,000 rides per week day. Casual users ride below 200,000 per week day. This data displays the significance of customizing marketing strategies and operational planning to accomodate distinct usage patterns between these two user types, ultimately optimizing service efficiency and user experience.

<img width="298" alt="Screen Shot 2023-10-28 at 8 14 07 PM" src="https://github.com/JTorres222/Cyclistic_Bike_Analysis/assets/81725586/9c10feda-4dba-4e6d-a130-f9a19841afe7">

Based on results above, electric bikes are the most popular type of bike for both user types. It's interesting to note that docked bikes are only used by casual members. No data shows members using docked bikes, which could indicate an error in data collection or some other reason why docked bikes were never picked by members. One possible reason is since members frequently used the service during weekdays, which indicates usage for work, they typically used electrical or classical bikes (these bikes can be dropped off at multiple locations where the Cyclisis operates). Casuals more frequently used the service during weekends, which indicates leisure, and thus docked bikes more used for their sporadic needs.


![top_10_stations_casual](https://github.com/JTorres222/Cyclistic_Bike_Analysis/assets/81725586/8a09eff4-76e9-4d10-9621-62c690873c43)

This graph displays the top ten stations in Chicago that were frequently used to carry out a ride for casual users. The most popular station was Streeter Dr & Grand Ave, displaying more than 40000 ride counts. 

![top_10_stations_member](https://github.com/JTorres222/Cyclistic_Bike_Analysis/assets/81725586/e585a91c-7354-4293-b034-3e2b29a3cc56)

This graph displays the top ten stations in Chicago that were frequently used to carry out a ride for members. The most popular station was Kingsbury St & Kinzie St, displaying about 25000 ride counts. All the other stations don't have much of a difference, just a steady decrease in count. 

![average_duration_month](https://github.com/JTorres222/Cyclistic_Bike_Analysis/assets/81725586/d9e72f61-dfa4-4c18-869e-74b03d834885)

Based on the graph above, casuals have a higher average duration, around the 1500 second range, which is the 25 minute range, It follows a similar pattern to counts for both user types, shown below, whre the average duration lowers as winter approaches and goes up as summer approaches. Average duration for members is below 1000 seconds (about 17 minutes)

![total_rides_month](https://github.com/JTorres222/Cyclistic_Bike_Analysis/assets/81725586/46e9dcc0-2190-4a73-ad66-c4030e3bae1a)

Based on total rides by user types per month, both user types follow a similar ride count pattern, where max ride counts is in the summer and as autumn and winter approaches, rides decrease, then they start picking up again as spring and summer approach. As expected, members members have more counts which means they ride bikes more often than casual riders.

<h2>Top 3 Recommendations</h2>

1. Based on the analysis, Cyclistic could offer promotions or discounts during seasonal peaks (spring and summer) that could encourage casual riders to sign up for a membership. Furthermore, offering a free trial for a week or a month could be effective so that the user could see if they would have a need to use rides through out the week.
2. Another best recommendation is to make a rewards program for reaching certain milestones or distances. This takes into account the fact that casual riders are more likely to use rides longer than members, most likely because of leisure. The rewards could be anything from membership discounts, to free prizes to local business discounts.
3. A marketing campaign strategy could be implemented catered towards casual users that use any of the ten popular stations shown in the data above. The campaign could highlight the stations convenience, safety and any unique features or offers made exclusively for these stations
