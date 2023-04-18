# load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(forcats)
library(geosphere)
library(knitr)
library(tidytext)

#Import the data
path <- "~\\Google Capstone Project\\csv\\"
file_list <- list.files(path)
file_list


months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
for (file_name in file_list) {
  # extract month and year from file_name
  month <- substr(file_name, 5, 6)
  year <- substr(file_name, 3, 4)
  
  # create variables
  var_name <- paste(months[as.numeric(month)], year, "_tripdata", sep = "")
  assign(var_name, read.csv(paste(path, file_name, sep = "")))
  
  #OPTIONAL: print out a statement to inform the user the file has been imported
  print(paste(var_name, "has been imported"))


#list all objects in the environment
obj_list <- ls()

#filter the data frames from the object list
df_list <- obj_list[sapply(obj_list, function(x) is.data.frame(get(x)))]
df_list

for (df_name in df_list[1]){
  cat(paste0("Information for ", df_name, ":\n"))
  glimpse(df_name)
}



#Combine data
all_trips <- bind_rows(mget(df_list))
glimpse(all_trips)
rm(list = df_list)

#check for duplicates
duplicated_entries <- sum(duplicated(all_trips))
duplicated_entries

#Check for missing values
na_values <- colSums(is.na(all_trips))
na_values

#Check for empty strings
empty_values <- all_trips %>%
  summarise_all(~sum(. %in% c("", " ")))
empty_values

#check for valid data-typing*
str(all_trips)

## Cleaning the data

#check if end lat/lng can be filled
all_trips %>%
  filter(is.na(end_lat) | is.na(end_lng)) %>%
  filter(end_station_id != "" | end_station_name != "")

#NA omit
all_trips_v2 <- na.omit(all_trips)

#Create station data set
station_data <- all_trips_v2 %>%
  select(start_station_id, start_station_name, start_lat, start_lng) %>%
  bind_rows(all_trips_v2 %>% #row_bind end_station information to be thorough
              select(start_station_id = end_station_id, 
                     start_station_name = end_station_name, 
                     start_lat = end_lat, 
                     start_lng = end_lng)) %>% 
  arrange(start_lat, start_lng, desc(start_station_name)) %>% #desc(start_station_name) to prioritize non-empty values when using first()
  group_by(start_lat, start_lng) %>%
  summarize(start_station_id = first(start_station_id), 
            start_station_name = first(start_station_name), .groups = "drop") %>%
  ungroup()


#First join - Fill Start Station ID/Name
all_trips_v3 <- all_trips_v2 %>% #fill start_station_id, start_station_name
  left_join(station_data, by=c("start_lat", "start_lng"), suffix = c("","_dup")) %>%
  mutate(start_station_id = ifelse(start_station_id_dup == "", "", start_station_id_dup), 
         start_station_name = ifelse(start_station_name_dup == "", "", start_station_name_dup)) %>%
  select(-c(start_station_id_dup, start_station_name_dup))

#Second join - Fill End Station ID/Name

#rename so we can repeat with end_stations
station_data <- station_data %>%
  rename(end_lat = start_lat, end_lng = start_lng, end_station_id = start_station_id, end_station_name = start_station_name) 

all_trips_v3 <- all_trips_v3 %>% #fill end_station_id, end_station_name - 5,829,084
  left_join(station_data, by=c("end_lat", "end_lng"), suffix = c("","_dup")) %>%
  mutate(end_station_id = ifelse(end_station_id_dup == "", "", end_station_id_dup), 
         end_station_name = ifelse(end_station_name_dup == "", "", end_station_name_dup)) %>%
  select(-c(end_station_id_dup, end_station_name_dup))


#Remove empty strings*
all_trips_v4 <- all_trips_v3 %>%
  filter(start_station_name != "", start_station_id !="", end_station_name != "", end_station_id != "") #850,550)


#double check for missing values/empty strings
any(is.na(all_trips_v4) | all_trips_v4 == "")#returns TRUE if NA/empty strings are present

all_trips_v4 %>%
  summarise_all(~sum(. %in% c("", " ")))




#Correct data-typings*
all_trips_v4 <- all_trips_v4 %>%
  mutate(
    rideable_type = factor(rideable_type),
    member_casual = factor(member_casual), 
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"))
summary(all_trips_v4[c("rideable_type", "member_casual", "started_at", "ended_at")])

#Calculate ride_length
all_trips_v5 <- all_trips_v4 %>%
  mutate(ride_length_minutes = as.numeric(difftime(ended_at, started_at, units = "secs"))/60)


#Infer Date Information
all_trips_v5 <- all_trips_v5 %>%
  mutate(date = as.Date(started_at),
         month = as.numeric(format(date, "%m")),
         day = as.numeric(format(date, "%d")),
         year = as.numeric(format(date, "%Y")),
         day_of_week = factor(wday(started_at, label = TRUE), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

#Append Hour of Day
all_trips_v5 <- all_trips_v5 %>%
  mutate(started_at_hour = hour(started_at))


#Calculate Distance Between Stations
m_to_miles_converstion_rate = 0.000621371

all_trips_v5 <- all_trips_v5 %>%
  mutate(ride_distance_miles = distHaversine(
    p1 = cbind(start_lng, start_lat),
    p2 = cbind(end_lng, end_lat)
  ) * m_to_miles_converstion_rate)


all_trips_v5 %>%
  sample_n(15)


#Verify readiness
summary(all_trips_v5)
#We notice that ride_length has some negative values. 
#We learn that these are because quality assurance was being performed
#These should be omit. Since we are removing data we will create a new data frame


### Investigation
all_trips_v5 <- all_trips_v5 %>%
  mutate(rideable_type = fct_recode(rideable_type, "classic_bike" = "docked_bike"))

summary(all_trips_v5["rideable_type"])

#find problem
all_trips_v5 %>%
  select(c(end_station_id, end_station_name, end_lat, end_lng)) %>%
  filter(end_station_id == "chargingstx07")

#create values to fill
chargingstx07 <-  all_trips_v5 %>%
  filter(end_station_id == "chargingstx07" & end_lat != 0 & end_lng != 0 )

chargingstx07_lat_mean = mean(chargingstx07$end_lat)
chargingstx07_lng_mean = mean(chargingstx07$end_lng)

#fill the 0 values
all_trips_v6 <- all_trips_v5 %>%
  mutate(end_lat = if_else(end_lat == 0, chargingstx07_lat_mean, end_lat),
         end_lng = if_else(end_lng == 0, chargingstx07_lng_mean, end_lng))

#check if end_lat/end_lng changed from all_trips_v5 to minimize re-calculations
all_trips_v6 <- all_trips_v6 %>%
  mutate(ride_distance_miles = 
           if_else(end_lat != all_trips_v5$end_lat | end_lng != all_trips_v5$end_lng,
                   distHaversine(p1 = cbind(start_lng, start_lat),p2 = cbind(end_lng, end_lat)) * m_to_miles_converstion_rate,
                   ride_distance_miles))

#double check for 0 values
summary(all_trips_v6[c("end_lat", "end_lng")])

#Deal with negative values
all_trips_v6 %>%
  select(c(start_station_id, start_station_name, end_station_id, end_station_name, ride_length_minutes)) %>%
  filter(ride_length_minutes < 0 & start_station_id == "Hubbard Bike-checking (LBS-WH-TEST)") %>%
  bind_rows(all_trips_v6 %>%
              select(c(start_station_id, start_station_name, end_station_id, end_station_name, ride_length_minutes)) %>%
              filter( end_station_id == "Hubbard Bike-checking (LBS-WH-TEST)") %>%
              sample_n(3)
  ) %>%
  bind_rows(all_trips_v6 %>%
              select(c(start_station_id, start_station_name, end_station_id, end_station_name, ride_length_minutes)) %>%
              filter(start_station_id == "Hubbard Bike-checking (LBS-WH-TEST)") %>%
              sample_n(3)
  )

#remove negatives
inspection_site = "Hubbard Bike-checking (LBS-WH-TEST)"
all_trips_v6 <- all_trips_v6 %>%
  filter(start_station_id != inspection_site, end_station_id != inspection_site, ride_length_minutes > 0)

all_trips_v6 %>%
  filter(is.na(ride_length_minutes))

all_trips_v6 %>%
  filter(ride_length_minutes<0)


#sample outliers
outliers_ride_length <- boxplot(all_trips_v6$ride_length_minutes, plot = FALSE)$out

sort(round(outliers_ride_length/60/24,2), decreasing=TRUE)[1:50]

#remove outliers
all_trips_v6 <- all_trips_v6 %>%
  filter(ride_length_minutes<32035)

#double check
max(all_trips_v6$ride_length_minutes)
dim(all_trips_v6) 
summary(all_trips_v6[c("ride_length_minutes", "ride_distance_miles")])


# Analyze
member_casual_table <- table(all_trips_v6$member_casual)
member_casual_prop <- prop.table(member_casual_table)*100
member_casual_prop

## Ride Duration Summary


#Let's take a look at these values by comparing members vs casual users
all_trips_v6 %>%
  filter(ride_length_minutes>0) %>%
  group_by(member_casual) %>%
  summarise(
    mean_ride_length_minutes = mean(ride_length_minutes),
    median_ride_length_minutes = median(ride_length_minutes),
    min_ride_length_seconds = min(ride_length_minutes)*60,
    max_ride_length_hours = max(ride_length_minutes)/60
  )

all_trips_v6 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(mean_ride_length_minutes = mean(ride_length_minutes)) %>%
  arrange(member_casual, day_of_week)


#Let's analyze ridership data by type and weekday
all_trips_v6 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    mean_ride_length_minutes = mean(ride_length_minutes),
    number_of_rides = n()) %>%
  arrange(member_casual, day_of_week)


#Let's analyze ridership data by type, weekday, and phase of day
#Might be better as a graph
all_trips_v6 %>%
  group_by(member_casual, started_at_hour) %>%
  summarise(
    mean_ride_length_minutes = mean(ride_length_minutes),
    number_of_rides = n()) %>%
  arrange(member_casual, started_at_hour)


# Distance Summary

#Let's take a look at these values by comparing members vs casual users
all_trips_v6 %>%
  filter(ride_distance_miles>0) %>%
  group_by(member_casual) %>%
  summarise(
    mean_ride_distance_miles = mean(ride_distance_miles),
    median_ride_distance_miles = median(ride_distance_miles),
    min_ride_distance_miles = min(ride_distance_miles),
    max_ride_distance_miles = max(ride_distance_miles)
  )


## Bike Type Summary
all_trips_v6 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(
    number_of_rides = n(),
    mean_ride_length_minutes = mean(ride_length_minutes),
    mean_ride_distance_miles = mean(ride_distance_miles)
  )



# Share


#Set graph colors
theme_colors = c("#4095A5", "plum2")

all_trips_v6 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week)  %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + #'dodge'  positions the columns adjacent to each other rather than stacked
  labs(title = "Increased Ride Usage Among Member Customers During Weekdays",
       subtitle = "Number of Rides by Weekday", 
       x = "Weekday", y = "Number of Rides", 
       fill = "Customer Type"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = theme_colors)


all_trips_v6 %>%
  group_by(member_casual, day_of_week, started_at_hour) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = started_at_hour, y = number_of_rides, fill = member_casual)) +
  geom_area(position = "stack") +
  facet_wrap(~day_of_week, ncol = 4, scales = "free_x") +
  labs(title = "Dual Peaks in Rides during Weekdays",
       subtitle = "Breakdown of Rides by Hour", 
       x = "Hour of Day", 
       y = "Number of Rides", 
       fill = "Customer Type") +
  scale_x_continuous(breaks = seq(0,23, by=4)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = theme_colors)


all_trips_v6 %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = dplyr::n(),
            mean_ride_length_minutes = mean(ride_length_minutes)) %>% 
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = mean_ride_length_minutes, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Casual Customers Ride Nearly Twice as Long as Annual Members",
       subtitle = "Average Ride Duration by Weekday", 
       x = "Weekday", y = "Number of Rides", 
       fill = "Customer Type"
  ) +
  scale_y_continuous(labels = scales::comma_format())+
  scale_fill_manual(values = theme_colors)


all_trips_v6 %>%
  group_by(member_casual, started_at_hour, day_of_week) %>%
  summarise(average_duration = mean(ride_length_minutes)) %>%
  ggplot(aes(x = started_at_hour, y = average_duration, fill = member_casual)) +
  geom_area(position = "stack") +
  facet_wrap(~day_of_week, ncol = 4, scales = "free_x") + 
  labs(title = "Casual Customer Rides Decrease between 3-10 AM",
       subtitle = "Average Hourly Ride Duration",
       x = "Hour of Day", 
       y = "Average Duration (in mins)",
       fill = "Customer Type") +
  scale_fill_manual(values = theme_colors)


# Top 10 Start Stations by Member_Casual
all_trips_v6 %>%
  group_by(member_casual, start_station_name) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, desc(number_of_rides)) %>%
  top_n(10) %>%
  mutate(start_station_name = factor(start_station_name),
         start_station_name = reorder_within(start_station_name, number_of_rides, member_casual)) %>%
ggplot(start_stations, aes(x = number_of_rides, y =  start_station_name, fill = member_casual)) +
  geom_col() +
  facet_grid(member_casual ~ ., scales = "free_y", switch = "y") +
  scale_y_reordered()+
  labs(title = "Casual Customer Heavily Prefer Navy Pier Bike Station",
       subtitle = "Top 10 Start Stations by Customer Type",
       x = "Number of Rides",
       y = "",
       fill = "Customer Type") +
  scale_fill_manual(values = theme_colors)


# Top 10 End Stations by Member_Casual
all_trips_v6 %>%
  group_by(member_casual, end_station_name) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, desc(number_of_rides)) %>%
  top_n(10) %>%
  mutate(end_station_name = factor(end_station_name),
         end_station_name = reorder_within(end_station_name, number_of_rides, member_casual)) %>%
ggplot(end_stations, aes(x = number_of_rides, y =  end_station_name, fill = member_casual)) +
  geom_col() +
  facet_grid(member_casual ~ ., scales = "free_y", switch = "y") +
  scale_y_reordered()+
  labs(title = "Navy Pier Remains the Most Popular Station",
       subtitle = "Top 10 End Stations by Customer Type",
       x = "Number of Rides",
       y = "",
       fill = "Customer Type") +
  scale_fill_manual(values = theme_colors)



#We want to add weekday to this
all_trips_v6 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(
    number_of_rides = n(),
    mean_ride_length_minutes = mean(ride_length_minutes),
    mean_ride_distance_miles = mean(ride_distance_miles)
  ) %>%
  ggplot(aes(rideable_type, number_of_rides, fill=member_casual)) +
  geom_col(position="dodge")+
  scale_y_continuous()+
  scale_fill_manual(values = theme_colors)


#Export a summary file
to_csv <- all_trips_v6

if (!dir.exists("processed_csv")) {
  dir.create("processed_csv")
}
write_csv(to_csv , file = paste0(getwd(), "/processed_csv/trip_data_cleaned.csv"))

