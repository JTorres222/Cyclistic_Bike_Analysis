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

#Accessing all sorted datasetse
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
  
  #Calculate ride length and add it as a new column
  dataset$ride_length <- dataset$ended_at - dataset$started_at
  
  #Calculate day of the week each ride started and add it as a new column
  dataset$day_of_week <- wday(dataset$started_at)
  
  #Store modified dataframe in the list
  modified_datasets[[name]] <- dataset
}

view(modified_datasets$jun_22)

