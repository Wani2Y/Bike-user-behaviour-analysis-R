# Cyclistic BikeShare data reveal bussiness opportunities
#load all necessary library for this report
library(stringi)
library(tidyverse)
library(lubridate)
library(dplyr)
library(hms)
library(knitr)
library(coin)
library(caret)
library(reshape2)
library(geosphere)
library(ggplot2)
library(pwr)
library(rstatix)
library(stats)
library(scales)
library(sf)
library(tmap)
library(tmaptools)

# read the file names of all csv files without extensions.
filenames <- tools::file_path_sans_ext(
  list.files(path ="./",
             pattern ="*.csv"))  

# read in all csv files from the subfolder.
raw_csv <-  list()
for(i in filenames){
  filepath <- file.path("./",paste(i,".csv",sep =""))
  raw_csv[[i]] <- read.csv(filepath)
}

# read in the shape file of Chicago for subsequent spatial mapping.
chicago <- st_read("chicago.shp") 

# convert columns of data sets into appropriate datatypes before 
# combining data sets, which can avoid potential joining issues. 
# longitudes and lattitudes are already in dbl with six decimal figures, 
# so we can convert them into geospatial format later.
for (i in 1: length(raw_csv)){
  raw_csv[[i]]$bike_type <- as.factor(raw_csv[[i]]$bike_type)
  raw_csv[[i]]$start_station_id <- as.factor(raw_csv[[i]]$start_station_id)
  raw_csv[[i]]$start_station_name <- as.factor(raw_csv[[i]]$start_station_name)
  raw_csv[[i]]$end_station_id <- as.factor(raw_csv[[i]]$end_station_id)
  raw_csv[[i]]$end_station_name <- as.factor(raw_csv[[i]]$end_station_name)
  raw_csv[[i]]$membership <- as.factor(raw_csv[[i]]$membership)
}

# datetime has different formats so they need to be converted into 
# the same format consistently. '%Y-%m-%d %H:%M:%S' is chosen here.
# datetime in format '%Y-%m-%d %H:%M:%S'.
raw_csv[[1]]$start_datetime <- 
  strptime(raw_csv[[1]]$start_datetime, 
           format = '%Y-%m-%d %H:%M:%S', tz = "UTC")
raw_csv[[1]]$end_datetime <- 
  strptime(raw_csv[[1]]$end_datetime, 
           format = '%Y-%m-%d %H:%M:%S', tz = "UTC")

# datetime in format '%m/%d/%Y %H:%M:%S'.
for (i in c(2:15)){
  raw_csv[[i]]$start_datetime <- 
    strptime(raw_csv[[i]]$start_datetime, 
             format = '%m/%d/%Y %H:%M', tz = "UTC")
  raw_csv[[i]]$end_datetime <- 
    strptime(raw_csv[[i]]$end_datetime, 
             format = '%m/%d/%Y %H:%M', tz = "UTC")
}

# datetime in format '%m/%d/%Y %H:%M:%S'.
for (i in c(16:24)){
  raw_csv[[i]]$start_datetime <- 
    strptime(raw_csv[[i]]$start_datetime, 
             format = '%m/%d/%Y %H:%M:%S', tz = "UTC")
  raw_csv[[i]]$end_datetime <- 
    strptime(raw_csv[[i]]$end_datetime, 
             format = '%m/%d/%Y %H:%M:%S', tz = "UTC")
}

# datetime in format '%Y-%m-%d %H:%M'.
for (i in c(25:74)){
  raw_csv[[i]]$start_datetime <- 
    strptime(raw_csv[[i]]$start_datetime, 
             format = '%Y-%m-%d %H:%M', tz = "UTC")
  raw_csv[[i]]$end_datetime <- 
    strptime(raw_csv[[i]]$end_datetime, 
             format = '%Y-%m-%d %H:%M', tz = "UTC")
}
ride_data <- bind_rows(raw_csv, .id = "csv_names") %>% as_tibble()

# we consider rows without station ids, start datetime, 
# and end datetime as unreliable/incomplete data, 
# which we can use as indicators of data loss. 
# Accordingly, we can evaluate some patterns regarding data loss.
lost_date <- 
  ride_data[(is.na(ride_data$start_datetime) | 
               is.na(ride_data$end_datetime)),]
lost_station <- ride_data[(is.na(ride_data$start_station_id) | 
                             is.na(ride_data$end_station_id)), ]

lost_station$start_year <- 
  year(lost_station$start_datetime)
lost_station$start_month <- 
  month(lost_station$start_datetime,
        label = TRUE, abbr = TRUE)

lost_station <- 
  lost_station %>% 
  select(c("membership", "start_year", "start_month",
           "start_station_lon", "start_station_lat",
           "end_station_lon", "end_station_lat")) %>%
  drop_na()

# examine the date at which the data is incomplete.
unique(lost_date$csv_names)
# all incomplete data come from Q4 of 2017, 
# which suggests that this data loss may be a one time incident.

unique(lost_station$start_year)
# all data without station id come from 2020, 
# which again may be a stand alone incident.

unique(lost_station$start_month)
# note that July, August, Setember, October, and November are the only months in 2020 that does not experience station id loss.

lost_plot_data <- data.frame(
  membership = as.factor(c("Subscriber", "Customer")),
  number_of_rides = c(sum(ride_data$membership == "Subscriber", na.rm=TRUE),
                      sum(ride_data$membership == "Customer", na.rm=TRUE))
)


# bar graph
p_lost_data <- 
  ggplot(lost_plot_data, 
         aes(x = membership, y = number_of_rides, fill = membership)) + 
  geom_bar(position="dodge", stat="identity") +  
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()), 
                     expand = c(0, 0), limits = c(0, NA)) +
  xlab("Membership") +
  ylab("Lost station id count") +
  ggtitle("Data Loss Counted by Stations") +
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_lost_data

# remove the large csv object to save RAM
rm(raw_csv)

# my pC has limited memory 
# so I release unused RAM where appropriate throughout the script
invisible(gc())

# start station
# convert geospatial data from dbl format to geometry format.
l_start_map <- lost_station[, c(1, 4, 5)]
l_start_map <- st_as_sf(l_start_map, coords = c("start_station_lon", "start_station_lat"))
l_start_map$membership <- recode_factor(l_start_map$membership, casual = "Customer",
                                        member = "Subscriber")
l_start_map$membership <- droplevels(l_start_map$membership)

# summarise data.
l_start_map <- l_start_map %>%  
  group_by_all() %>%  
  count
l_start_map$n <- (l_start_map$n)
colnames(l_start_map) <- c("membership", "geometry", "Number of Ride")
invisible(gc())

# mapping start stations.
l_start_map_plot <- tm_shape(shp = chicago) + 
  tm_polygons(col = "gray") + 
  tm_shape(l_start_map) + 
  tm_bubbles(col = "membership", palette = c("#E4002B", "#41B6E6")  ,size = "Number of Ride") +
  tm_layout(legend.outside = TRUE) +
  tm_layout(legend.position = c("right", "bottom"), 
            title= "Locations of End Stations", 
            title.position = c("right", "top"))
l_start_map_plot
invisible(gc())

# end station.
# convert geospatial data from dbl format to geometry format.
l_end_map <- lost_station[, c(1, 6, 7)]
l_end_map <- st_as_sf(l_end_map, coords = c("end_station_lon", "end_station_lat"))
l_end_map$membership <- recode_factor(l_end_map$membership, casual = "Customer",
                                      member = "Subscriber")
l_end_map$membership <- droplevels(l_end_map$membership)

# summarise data.
l_end_map <- l_end_map %>%  
  group_by_all() %>%  
  count
l_end_map$n <- (l_end_map$n)
colnames(l_end_map) <- c("membership", "geometry", "Number of Ride")
invisible(gc())

# mapping end stations
l_end_map_plot <- tm_shape(shp = chicago) + 
  tm_polygons(col = "gray") + 
  tm_shape(l_end_map) + 
  tm_bubbles(col = "membership", palette = c("#E4002B", "#41B6E6")  ,size = "Number of Ride") +
  tm_layout(legend.outside = TRUE) +
  tm_layout(legend.position = c("right", "bottom"), 
            title= "Locations of End Stations", 
            title.position = c("right", "top"))
l_end_map_plot
invisible(gc())

# as we could not determine what membership "Dependent" refers to without consulting Cyclistic, those data will be excluded for this analysis.
ride_data <- filter(ride_data, membership != "Dependent")

# we can assume "customer" is the same as "casual" and "Subscriber" equals "member".
# we will replace the membership descriptions by "Customer" and "Subscriber".
ride_data$membership <- as.character(ride_data$membership)
ride_data$membership[ride_data$membership == "casual"] <- "Customer"
ride_data$membership[ride_data$membership == "member"] <- "Subscriber"

# remove rows without station ids as unreliable data.
ride_data <- ride_data[!is.na(ride_data$start_station_id) & !is.na(ride_data$end_station_id),]
# remove rows without start and end datetime.
ride_data <- ride_data[!is.na(ride_data$start_datetime) & !is.na(ride_data$end_datetime),]

# create a dataframe for start stations and end stations.
start_station_info <- ride_data %>% select(start_station_id, start_station_name, 
                                           start_station_lon, start_station_lat) %>%
  drop_na() %>%
  distinct(start_station_id, .keep_all = TRUE)

end_station_info <- ride_data %>% select(end_station_id, end_station_name, 
                                         end_station_lon, end_station_lat)  %>%
  drop_na() %>%
  distinct(end_station_id, .keep_all = TRUE)

# fill in longitude and latitude based on matching station ids.
ride_data <- inner_join(ride_data, start_station_info, by = c("start_station_id", "start_station_name"))
ride_data <- ride_data %>% select(-c(start_station_lon.x, start_station_lat.x)) %>% rename(
  start_station_lon = start_station_lon.y,
  start_station_lat = start_station_lat.y
)

ride_data$start_year <- year(ride_data$start_datetime)
ride_data$start_month <- month(ride_data$start_datetime, label = TRUE, abbr = TRUE)
ride_data$start_ym <- as_factor(format(ride_data$start_datetime, "%Y-%m"))

ride_data <- inner_join(ride_data, end_station_info, by = c("end_station_id", "end_station_name"))
ride_data <- ride_data %>% select(-c("csv_names", "end_station_lon.x", "end_station_lat.x")) %>% rename(
  end_station_lon = end_station_lon.y,
  end_station_lat = end_station_lat.y
)

# get day of week from datetime column.
ride_data$start_day <- wday(ride_data$start_datetime, label = TRUE, abbr = TRUE)
ride_data$end_day <- wday(ride_data$end_datetime, label = TRUE, abbr = TRUE)

# compute the trip duration 
# some of the start time are later than the end time, which are likely input errors. We can take the absolute values of the difference between two dates to circumvent the potential erroneous inputs.
ride_data$trip_duration <- as_hms(abs(difftime(ride_data$end_datetime, ride_data$start_datetime)))

# compute the direct distance between start and end stations.
ride_data$rdistance <- distGeo(ride_data[ , 9:10], ride_data[ , 14:15])


# some users have trip duration of 0, which suggest they may be switching among bikes for functional or better fitting bikes.
# we can filter out those rows as they don't necessary tell us the difference between subscribers and customers.
ride_data <- ride_data %>%
  filter(start_datetime != end_datetime)

invisible(gc())

# counting the number of rides using the entire data set.
num_ride_all <- data.frame(
  membership = c("Subscriber", "Customer"),
  number_of_rides = c(sum(ride_data$membership == "Subscriber", na.rm=TRUE),
                      sum(ride_data$membership == "Customer", na.rm=TRUE))
)
kable(num_ride_all, style = "rmarkdown")

p_num_ride_all <- ggplot(num_ride_all, aes(x = membership, y = number_of_rides, fill = membership)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()), expand = c(0, 0), limits = c(0, NA)) +
  xlab("Year") +
  ylab("Number of Rides") +
  ggtitle("Number of Rides by Subscribers and Customers (2013 - 2023)") +
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_num_ride_all

invisible(gc())
# counting the number of rides annually.
num_ride_year <- data.frame(
  year = as.factor(c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)),
  subscriber = c(sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2013, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2014, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2015, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2016, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2017, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2018, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2019, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2020, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2021, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2022, na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$start_year == 2023, na.rm=TRUE)),
  
  customer = c(sum(ride_data$membership == "Customer" & ride_data$start_year == 2013, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2014, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2015, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2016, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2017, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2018, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2019, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2020, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2021, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2022, na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$start_year == 2023, na.rm=TRUE))
)

num_ride_year <- melt(num_ride_year, id.vars = "year")
colnames(num_ride_year) <- c("year", "membership", "number_of_rides")
kable(num_ride_year, style = "rmarkdown")

p_num_ride_year <- ggplot(num_ride_year, aes(x = year, y = number_of_rides, fill = membership)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()), 
                     expand = c(0, 0), limits = c(0, NA)) +
  xlab("Year") +
  ylab("Number of Rides") +
  ggtitle("Number of Rides by Subscribers and Customers (annually 2013 - 2023)") +
  scale_fill_manual(name = "membership", values = c("#E4002B", "#41B6E6")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_num_ride_year

invisible(gc())

num_ride_wday <- data.frame(
  weekday = as.factor(c(rep("Mon", 2), rep("Tue", 2), rep("Wed", 2), rep("Thu", 2), rep("Fri", 2), rep("Sat", 2), rep("Sun", 2))),
  membership = as.factor(c("Subscriber", "Customer")),
  year_2013 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2013 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2013 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2013 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2013 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2013 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2013 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2013 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2013 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2013 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2013 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2013 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2013 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2013 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2013 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2014 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2014 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2014 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2014 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2014 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2014 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2014 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2014 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2014 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2014 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2014 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2014 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2014 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2014 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2014 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2015 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2015 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2015 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2015 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2015 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2015 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2015 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2015 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2015 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2015 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2015 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2015 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2015 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2015 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2015 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2016 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2016 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2016 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2016 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2016 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2016 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2016 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2016 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2016 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2016 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2016 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2016 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2016 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2016 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2016 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2017 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2017 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2017 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2017 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2017 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2017 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2017 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2017 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2017 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2017 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2017 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2017 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2017 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2017 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2017 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2018 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2018 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2018 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2018 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2018 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2018 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2018 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2018 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2018 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2018 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2018 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2018 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2018 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2018 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2018 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2019 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2019 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2019 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2019 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2019 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2019 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2019 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2019 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2019 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2019 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2019 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2019 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2019 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2019 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2019 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2020 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2020 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2020 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2020 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2020 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2020 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2020 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2020 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2020 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2020 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2020 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2020 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2020 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2020 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2020 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2021 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2021 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2021 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2021 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2021 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2021 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2021 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2021 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2021 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2021 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2021 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2021 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2021 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2021 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2021 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2022 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2022 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2022 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2022 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2022 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2022 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2022 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2022 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2022 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2022 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2022 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2022 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2022 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2022 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2022 & ride_data$membership == "Customer", na.rm=TRUE)),
  
  year_2023 = c(sum(ride_data$start_day == "Mon" & ride_data$start_year == 2023 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Mon" & ride_data$start_year == 2023 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2023 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Tue" & ride_data$start_year == 2023 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2023 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Wed" & ride_data$start_year == 2023 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2023 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Thu" & ride_data$start_year == 2023 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2023 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Fri" & ride_data$start_year == 2023 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2023 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sat" & ride_data$start_year == 2023 & ride_data$membership == "Customer", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2023 & ride_data$membership == "Subscriber", na.rm=TRUE),
                sum(ride_data$start_day == "Sun" & ride_data$start_year == 2023 & ride_data$membership == "Customer", na.rm=TRUE))
)
kable(num_ride_wday, style = "rmarkdown")

num_ride_wday <- melt(num_ride_wday, id.vars = c("weekday", "membership"))
p_num_ride_wday <- ggplot(num_ride_wday, aes(x = factor(weekday, level = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), y = value, fill = membership)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()), expand = c(0, 0), limits = c(0, NA)) +
  xlab("Weekdays") +
  ylab("Number of Rides") +
  ggtitle("Number of Rides by Subscribers and Customers (daily Monday - Sunday)") +
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_num_ride_wday

invisible(gc())

# counting the number of rides based on bike type, which is only available from 2020 to 2023.
num_ride_bike_type <- data.frame(
  bike_type = as.factor(c("docked_bike", "electric_bike", "classic_bike")),
  subscriber = c(sum(ride_data$membership == "Subscriber" & ride_data$bike_type == "docked_bike", na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$bike_type == "electric_bike", na.rm=TRUE),
                 sum(ride_data$membership == "Subscriber" & ride_data$bike_type == "classic_bike", na.rm=TRUE)),
  
  customer = c(sum(ride_data$membership == "Customer" & ride_data$bike_type == "docked_bike", na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$bike_type == "electric_bike", na.rm=TRUE),
               sum(ride_data$membership == "Customer" & ride_data$bike_type == "classic_bike", na.rm=TRUE))
)
num_ride_bike_type <- melt(num_ride_bike_type, id.vars = "bike_type")
colnames(num_ride_bike_type) <- c("bike_type", "membership", "number_of_rides")
kable(num_ride_bike_type, style = "rmarkdown")

p_num_ride_bike_type <- ggplot(num_ride_bike_type, aes(x = bike_type, y = number_of_rides, fill = membership)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Bike Type") +
  ylab("Number of Ride") +
  ggtitle("Types of Bikes Preferred by Subscribers and Customers") +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale()), expand = c(0, 0), limits = c(0, NA)) +
  scale_fill_manual(name = "membership", values = c("#E4002B", "#41B6E6")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_num_ride_bike_type

invisible(gc())

# compute the max, min, mean, median, and mode of trip duration for subscribers and customers 
tri_du_all <- data.frame(
  membership = c("Subscriber", "Customer"),
  max_duration = c(seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer"]))),
  
  min_duration = c(seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer"]))),
  
  mean_duration = c(round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber"])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer"])), digits = 2)),
  
  median_duration = c(seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer"]))),
  
  standard_deviantion = c(sd(ride_data$trip_duration[ride_data$membership == "Subscriber"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer"]))
)
kable(tri_du_all, style = "rmarkdown")
# from the table, subscribers and customers have decent contrasts for means, median, and sd.

# create boxplot to visualise the descriptive statistics
p_tri_du_all_box <- ggplot(ride_data, aes(x =  membership, y = trip_duration, colour = membership, fill = membership)) + 
  geom_boxplot(stat = "boxplot", alpha = 0.3) +
  scale_y_time(limits = quantile(ride_data$trip_duration, c(0.1, 0.9))) +
  xlab("Membership") +
  ylab("Trip Duration") +
  ggtitle("Trip Duration by Subscribers and Customers") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_du_all_box

# as the values beyond quantiles are cut off for better visuals on the boxplot, a scatter plot is created as a supplement.
p_tri_du_all_point <- ggplot(ride_data, aes(x =  membership, y = trip_duration, colour = membership)) + 
  geom_jitter() +
  scale_y_time(expand = c(0, 0), limits = c(0, NA)) +
  xlab("Membership") +
  ylab("Trip Duration") +
  ggtitle("Trip Duration by Subscribers and Customers") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_du_all_point

invisible(gc())

# examine whether the entire data set is normally distributed.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber"])
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer"])

# as the data is non-normally distributed, we use a Two-sample Mann–Whitney U test from the coin package.
# normality may not be necessary as real-world data tends to be non-normal as the sample size becomes huge.
# convert the datetime format to dbl.
u_test_tri_du_all <- ride_data[,c(8,18)]
u_test_tri_du_all$membership <- as.factor(u_test_tri_du_all$membership)
u_test_tri_du_all$trip_duration <- as.numeric(u_test_tri_du_all$trip_duration)
u_test_tri_du_all_result <- coin :: wilcox_test(trip_duration ~ membership, data = u_test_tri_du_all, distribution = "approximate")
u_test_tri_du_all_result
invisible(gc())

tri_du_year <- data.frame(
  year = c(rep(2013, 2), rep(2014, 2), rep(2015, 2), rep(2016, 2), rep(2017, 2), rep(2018, 2),
           rep(2019, 2), rep(2020, 2), rep(2021, 2), rep(2022, 2), rep(2023, 2)),
  membership = c("Subscriber", "Customer"),
  max_duration = c(seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2013])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2013])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2014])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2014])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2015])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2015])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2016])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2016])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2017])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2017])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2018])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2018])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2019])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2019])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2020])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2020])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2021])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2021])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2022])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2022])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2023])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2023]))),
  
  min_duration = c(seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2013])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2013])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2014])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2014])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2015])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2015])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2016])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2016])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2017])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2017])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2018])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2018])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2019])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2019])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2020])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2020])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2021])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2021])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2022])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2022])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2023])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2023]))),
  
  mean_duration = c(round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2013])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2013])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2014])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2014])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2015])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2015])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2016])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2016])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2017])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2017])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2018])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2018])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2019])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2019])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2020])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2020])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2021])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2021])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2022])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2022])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2023])), digits = 2),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2023])), digits = 2)),
  
  median_duration = c(seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2013])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2013])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2014])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2014])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2015])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2015])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2016])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2016])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2017])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2017])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2018])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2018])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2019])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2019])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2020])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2020])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2021])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2021])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2022])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2022])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2023])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2023]))),
  
  standard_deviantion = c(sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2013]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2013]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2014]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2014]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2015]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2015]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2016]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2016]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2017]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2017]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2018]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2018]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2019]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2019]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2020]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2020]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2021]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2021]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2022]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2022]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2023]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2023]))
)
kable(tri_du_year, style = "rmarkdown")

# visualise the trip durations as scatter plots organised by years
# create a boxplot to compare the mean differences from 2013 to 2023.
p_tri_du_year_box <- ggplot(ride_data, aes(x =  as_factor(start_year), y = trip_duration, colour = membership, fill = membership)) + 
  geom_boxplot(stat = "boxplot", alpha = 0.3) +
  scale_y_time(limits = quantile(ride_data$trip_duration, c(0.1, 0.9))) +
  xlab("Year") +
  ylab("Trip Duration") +
  ggtitle("Trip Duration by Subscribers and Customers (annually 2013 - 2023)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_du_year_box

# scatter points to visualise the entire data set.
ride_data$membership <- as.factor(ride_data$membership)
p_tri_du_point_year <- ggplot(ride_data, aes(x =  start_year, y = trip_duration, colour = membership)) + 
  geom_jitter() +
  scale_y_time(expand = c(0, 0), limits = c(0, NA)) +
  xlab("Year") +
  ylab("Trip Duration") +
  ggtitle("Trip Duration by Subscribers and Customers (annually 2013 - 2023)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_du_point_year
invisible(gc())

# make a line plot to show changes from 2013 to 2023 explicitly.
tri_du_year <- ride_data %>% 
  group_by(month = floor_date(start_datetime, 'month'), membership) %>%  
  summarize(mean_tri_du = mean(trip_duration))
tri_du_year$membership <- as.factor(tri_du_year$membership)
tri_du_year$mean_tri_du <- as.numeric(tri_du_year$mean_tri_du)

p_tri_du_line_year <- ggplot(data = tri_du_year, aes(x = month, y = mean_tri_du, group = membership, color = membership)) +  
  geom_line() +
  xlab("Year") +
  ylab("Trip Duration") +
  ggtitle("Trip Duration by Subscribers and Customers (annually 2013 - 2023)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_y_time() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_du_line_year
# we can see the mean trip durations are very close between subscriberse and customers in 2021

invisible(gc())

# from the table, subscribers and customers have decent contrasts for means, median, and sd
# let's use a statistic test to see if we should be suprise to see the level of difference among years for subscribers and customers
# as we are interested in only the difference among years within subscribers and customers, we segregate the full data set into subscribers and customers.
# this makes computation a bit easier on my PC. You could perform two way anova or its non-parametric equivalent if you have a powerful PC.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2013])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2013])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2014])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2014])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2015])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2015])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2016])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2016])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2017])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2017])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2018])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2018])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2019])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2019])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2020])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2020])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2021])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2021])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2022])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2022])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_year == 2023])
# not normal.
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_year == 2023])
# not normal.

# as the data is not normally distributed, we can use a non-parametric ANOVA to test whether the observed difference is statistically significant for subscribers and customers, respectively.
# for subscribers
kruskal_sub_result <- kruskal.test(data = ride_data[ride_data$membership == "Subscriber", ], trip_duration ~ start_year)
kruskal_sub_result
# p-value < 2.2e-16, and the difference among years are significant!

# let's follow up with a pairwise comparison to find out between which years, the difference is significant.
# we can achieve this with Wilcoxon signed ranking test
wsr_sub_data <- ride_data[ride_data$membership == "Subscriber", ] %>%
  select(start_year, trip_duration)
wsr_sub_data$start_year <- as.factor(wsr_sub_data$start_year)
wsr_sub_result <- dunn_test(data = wsr_sub_data, trip_duration ~ start_year, p.adjust.method = "bonferroni")
wsr_sub_result
invisible(gc())
# difference between most years are statistically significant, except for the pairs of following years: 2014 and 2015.

# for customers
kruskal_cus_result <- kruskal.test(data = ride_data[ride_data$membership == "Customer", ], trip_duration ~ start_year)
kruskal_cus_result
# p-value < 2.2e-16, and the difference among years are significant!

# let's follow up with a Wilcoxon signed ranking test
wsr_cus_data <- ride_data[ride_data$membership == "Customer", ] %>%
  select(start_year, trip_duration)
wsr_cus_data$start_year <- as.factor(wsr_cus_data$start_year)
wsr_cus_result <- dunn_test(data = wsr_cus_data, trip_duration ~ start_year, p.adjust.method = "bonferroni")
wsr_cus_result
# difference between most years are statistically significant, except for the pairs of following years: 2013 and 2014.
invisible(gc())

tri_du_wday <- data.frame(
  weekday = c(rep("Mon", 2), rep("Tue", 2), rep("Wed", 2), rep("Thu", 2), rep("Fri", 2), rep("Sat", 2), rep("Sun", 2)),
  membership = c("Subscriber", "Customer"),
  max_duration = c(seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Mon"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Tue"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Wed"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Thu"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Fri"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sat"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun"])),
                   seconds_to_period(max(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sun"]))),
  
  min_duration = c(seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Mon"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Tue"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Wed"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Thu"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Fri"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sat"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun"])),
                   seconds_to_period(min(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sun"]))),
  
  mean_duration = c(round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Mon"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Tue"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Wed"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Thu"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Fri"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sat"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun"])), digits = 0),
                    round(seconds_to_period(mean(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sun"])), digits = 0)),
  
  median_duration = c(seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Mon"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Tue"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Wed"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Thu"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Fri"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sat"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun"])),
                      seconds_to_period(median(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sun"]))),
  
  standard_deviantion = c(sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Mon"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Tue"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Wed"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Thu"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Fri"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sat"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun"]),
                          sd(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sun"]))
)
kable(tri_du_wday, style = "rmarkdown")

# visualise 
# create a box plot.
p_tri_du_wday_box <- ggplot(ride_data, aes(x =  factor(start_day,
                                                       level = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                           y = trip_duration, colour = membership, fill = membership)) + 
  geom_boxplot(stat = "boxplot", alpha = 0.3) +
  scale_y_time(limits = quantile(ride_data$trip_duration, c(0.1, 0.9))) +
  xlab("Weekdays") +
  ylab("Trip Duration") +
  ggtitle("Trip Duration by Subscribers and Customers (daily Monday - Friday)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_du_wday_box
invisible(gc())
# subscribers consistently have shorter mean trip duration than customers.

# scatter points to visualise the entire data set.
ride_data$membership <- as.factor(ride_data$membership)
p_tri_du_point_wday <- ggplot(ride_data, aes(x =  start_day, y = trip_duration, colour = membership)) + 
  geom_jitter() +
  scale_y_time(expand = c(0, 0), limits = c(0, NA)) +
  xlab("Weekdays") +
  ylab("Trip Duration") +
  ggtitle("Trip Duration by Subscribers and Customers (daily Monday - Friday)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_du_point_wday
invisible(gc())

# create a line plot to see fluctuation of means during a given week explicitly.
p_tri_du_line_wday <- ggplot(data = tri_du_wday) +  
  geom_line(aes(x = factor(weekday, 
                           level = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                y = mean_duration, group = membership, color = membership)) +
  geom_line(linetype = "dashed", aes(x = factor(weekday, 
                                                level = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                     y = median_duration, group = membership, color = membership)) +
  xlab("Weekdays") +
  ylab("Trip Duration") +
  ggtitle("Trip Duration by Subscribers and Customers (daily Monday - Friday)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_y_time() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_du_line_wday

invisible(gc())

# examine the data to see whether they are normally distributed.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Mon"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Tue"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Wed"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Thu"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Fri"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sat"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun"])
# not normal
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer" & ride_data$start_day == "Sun"])
# not normal

# as the data is not normally distributed, we can use a non-parametric ANOVA to test whether the observed difference is statistically significant.
# for subscribers.
kruskal_sub_result <- kruskal.test(data = ride_data[ride_data$membership == "Subscriber", ], trip_duration ~ start_day)
kruskal_sub_result

# p-value < 2.2e-16, and the difference among years are significant!

# let's follow up with a pairwise comparison to find out between which weekdays, the difference is significant.
# we can achieve this with Wilcoxon signed ranking test
# for subscribers.
wsr_sub_data <- ride_data[ride_data$membership == "Subscriber", ] %>%
  select(start_day, trip_duration)
wsr_sub_data$start_day <- as.factor(wsr_sub_data$start_day)
wsr_sub_result <- dunn_test(data = wsr_sub_data, trip_duration ~ start_day, p.adjust.method = "bonferroni")
wsr_sub_result
# for subscribers, ride distance is significantly different between weekdays, except for between Tuesday and Wednesday, Tuesday and Thursday, and Wednesday and Thursday.

# for customers.
kruskal_cus_result <- kruskal.test(data = ride_data[ride_data$membership == "Customer", ], trip_duration ~ start_day)
kruskal_cus_result
# p-value < 2.2e-16, and the difference among years are significant!

# let's follow up with a pairwise comparison to find out between which weekdays, the difference is significant.
# we can achieve this with Wilcoxon signed ranking test
wsr_cus_data <- ride_data[ride_data$membership == "Customer", ] %>%
  select(start_day, trip_duration)
wsr_cus_data$start_day <- as.factor(wsr_cus_data$start_day)
wsr_cus_result <- dunn_test(data = wsr_cus_data, trip_duration ~ start_day, p.adjust.method = "bonferroni")
wsr_cus_result
# trip duration is statistically different between any two given weekdays for customers.
invisible(gc())

tri_rd_all <- data.frame(
  membership = c("Subscriber", "Customer"),
  max_distance = c(round(max(ride_data$rdistance[ride_data$membership == "Subscriber"]), digit = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer"]), digit = 2)),
  
  min_distance = c(round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$rdistance != 0]), digit = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$rdistance != 0]), digit = 2)),
  
  mean_distance = c(round(mean(ride_data$rdistance[ride_data$membership == "Subscriber"]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer"]), digits = 2)),
  
  median_distance = c(round(median(ride_data$rdistance[ride_data$membership == "Subscriber"]), digit = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer"]), digit = 2)),
  
  standard_deviantion = c(sd(ride_data$rdistance[ride_data$membership == "Subscriber"]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer"]))
)
kable(tri_rd_all, style = "rmarkdown")

# boxplot to visualise the descriptive statistics
p_tri_rd_all_box <- ggplot(ride_data, aes(x =  membership, y = rdistance, colour = membership, fill = membership)) + 
  geom_boxplot(stat = "boxplot", alpha = 0.3) +
  scale_y_continuous(limits = quantile(ride_data$rdistance, c(0.1, 0.9))) +
  xlab("Membership") +
  ylab("Trip Duration") +
  ggtitle("Ride Distance by Subscribers and Customers (2013 - 2023)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_rd_all_box

# as the extreme values are cut off on the boxplot, a scatter plot is created as a suppelemnt
p_tri_rd_all_point <- ggplot(ride_data, aes(x =  membership, y = rdistance, colour = membership)) + 
  geom_jitter() +
  scale_y_continuous(labels = unit_format(unit = "km", scale = 1e-3), expand = c(0, 0), limits = c(0, NA)) +
  xlab("Membership") +
  ylab("Trip Duration") +
  ggtitle("Ride Distance by Subscribers and Customers (2013 - 2023)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_rd_all_point

invisible(gc())

# examine whether the entire data set is normally distributed.
qqnorm(ride_data$trip_duration[ride_data$membership == "Subscriber"])
qqnorm(ride_data$trip_duration[ride_data$membership == "Customer"])

# as the data is non-normally distributed, we use a Two-sample Mann–Whitney U test from the coin package.
# normality may not be necessary as real-world data tends to be non-normal as the sample size becomes huge.
# convert the datetime format to dbl.
u_test_tri_du_all <- ride_data[,c(8,18)]
u_test_tri_du_all$membership <- as.factor(u_test_tri_du_all$membership)
u_test_tri_du_all$trip_duration <- as.numeric(u_test_tri_du_all$trip_duration)
u_test_tri_du_all_result <- coin :: wilcox_test(trip_duration ~ membership, data = u_test_tri_du_all, distribution = "approximate")
u_test_tri_du_all_result
invisible(gc())

# only the non-zero distance is used in computation as we can only infer ride distance from spatial coordinates
year_r_dis_sum <- data.frame(
  year = c(rep(2013, 2), rep(2014, 2), rep(2015, 2), rep(2016, 2), rep(2017, 2), rep(2018, 2),
           rep(2019, 2), rep(2020, 2), rep(2021, 2), rep(2022, 2), rep(2023, 2)),
  membership = c("Subscriber", "Customer"),
  max_distance = c(round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2013 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2013 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2014 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2014 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2015 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2015 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2016 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2016 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2017 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2017 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2018 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2018 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2019 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2019 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2020 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2020 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2021 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2021 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2022 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2022 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2023 & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2023 & ride_data$rdistance != 0]), digits = 2)),
  
  min_distance = c(round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2013 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2013 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2014 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2014 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2015 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2015 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2016 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2016 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2017 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2017 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2018 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2018 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2019 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2019 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2020 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2020 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2021 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2021 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2022 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2022 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2023 & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2023 & ride_data$rdistance != 0]), digits = 2)),
  
  mean_distance = c(round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2013 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2013 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2014 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2014 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2015 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2015 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2016 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2016 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2017 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2017 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2018 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2018 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2019 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2019 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2020 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2020 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2021 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2021 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2022 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2022 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2023 & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2023 & ride_data$rdistance != 0]), digits = 2)),
  
  median_distance = c(round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2013 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2013 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2014 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2014 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2015 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2015 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2016 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2016 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2017 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2017 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2018 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2018 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2019 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2019 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2020 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2020 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2021 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2021 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2022 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2022 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2023 & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2023 & ride_data$rdistance != 0]), digits = 2)),
  
  standard_deviantion = c(sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2013 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2013 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2014 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2014 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2015 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2015 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2016 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2016 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2017 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2017 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2018 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2018 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2019 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2019 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2020 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2020 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2021 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2021 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2022 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2022 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2023 & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2023 & ride_data$rdistance != 0]))
)
kable(year_r_dis_sum, style = "rmarkdown")
invisible(gc())

# boxplot to visualise the descriptive statistics
p_tri_rd_all_box <- ggplot(ride_data, aes(x =  start_year, y = rdistance, colour = membership, fill = membership)) + 
  geom_boxplot(stat = "boxplot", alpha = 0.3) +
  scale_y_continuous(limits = quantile(ride_data$rdistance, c(0.1, 0.9))) +
  xlab("Year") +
  ylab("Ride Distance") +
  ggtitle("Ride Distance by Subscribers and Customers (annually 2013 - 2023)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_rd_all_box

# scatter plots of trip distance 
# max ride distance is oddly large and could be a single outlier. We therefore remove it from the plot
ride_data$membership <- as.factor(ride_data$membership)
p_r_dis_point_year <- ggplot(data = ride_data[which(ride_data$rdistance > 0), ], aes(x =  start_year, y = rdistance, colour = membership)) + 
  geom_jitter() +
  scale_y_continuous(labels = unit_format(unit = "km", scale = 1e-3), expand = c(0, 0), limits = c(0, NA)) +
  xlab("Year") +
  ylab("Ride Distance") +
  ggtitle("Ride Distance by Subscribers and Customers (annually 2013 - 2023)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_r_dis_point_year
invisible(gc())

# plot to see changes in ride distance through out the years
r_dis_year <- ride_data[which(ride_data$rdistance > 0), ] %>% 
  group_by(month = floor_date(start_datetime, 'month'), membership) %>%  
  summarize(mean_r_dis = mean(rdistance))
r_dis_year$membership <- as.factor(r_dis_year$membership)
r_dis_year$mean_r_dis <- as.numeric(r_dis_year$mean_r_dis)

p_r_dis_line_year <- ggplot(data = r_dis_year, aes(x = month, y = mean_r_dis, group = membership, color = membership)) +  
  geom_line() +
  scale_y_continuous(labels = unit_format(unit = "km", scale = 1e-3), expand = c(0, 0), limits = c(0, NA)) +
  xlab("Year") +
  ylab("Ride Distance") +
  ggtitle("Ride Distance by Subscribers and Customers (annually 2013 - 2023)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_r_dis_line_year

invisible(gc())

# examine the normality of data
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2013])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2013])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2014])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2014])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2015])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2015])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2016])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2016])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2017])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2017])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2018])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2018])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2019])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2019])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2020])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2020])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2021])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2021])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2022])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2022])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_year == 2023])
# not normal.
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_year == 2023])
# not normal.

# as the data is not normally distributed, we can use a non-parametric ANOVA to test whether the observed difference is statistically significant for subscribers and customers, respectively.
# for subscribers
kruskal_sub_result <- kruskal.test(data = ride_data[ride_data$membership == "Subscriber", ], rdistance ~ start_year)
kruskal_sub_result
# p-value < 2.2e-16, and the difference among years are significant!

# let's follow up with a pairwise comparison to find out between which years, the difference is significant.
# we can achieve this with Wilcoxon signed ranking test
wsr_sub_data <- ride_data[ride_data$membership == "Subscriber", ] %>%
  select(start_year, rdistance)
wsr_sub_data$start_year <- as.factor(wsr_sub_data$start_year)
wsr_sub_result <- dunn_test(data = wsr_sub_data, rdistance ~ start_year, p.adjust.method = "bonferroni")
wsr_sub_result
invisible(gc())
# ride distances are statistically different between any two given year.

# for customers
kruskal_cus_result <- kruskal.test(data = ride_data[ride_data$membership == "Customer", ], rdistance ~ start_year)
kruskal_cus_result
# p-value < 2.2e-16, and the difference among years are significant!

# let's follow up with a Wilcoxon signed ranking test
wsr_cus_data <- ride_data[ride_data$membership == "Customer", ] %>%
  select(start_year, rdistance)
wsr_cus_data$start_year <- as.factor(wsr_cus_data$start_year)
wsr_cus_result <- dunn_test(data = wsr_cus_data, rdistance ~ start_year, p.adjust.method = "bonferroni")
wsr_cus_result
# ride distances of customers are significantly different between any given year, except between 2013 asnd 2020, between 2014 and 2015, between 2016 and 2017, between 2018 and 2019.
invisible(gc())

tri_rd_wday <- data.frame(
  weekday = c(rep("Mon", 2), rep("Tue", 2), rep("Wed", 2), rep("Thu", 2), rep("Fri", 2), rep("Sat", 2), rep("Sun", 2)),
  membership = c("Subscriber", "Customer"),
  max_distance = c(round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]), digits = 2),
                   round(max(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]), digits = 2)),
  
  min_distance = c(round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]), digits = 2),
                   round(min(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]), digits = 2)),
  
  mean_distance = c(round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]), digits = 2),
                    round(mean(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]), digits = 2)),
  
  median_distance = c(round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]), digits = 2),
                      round(median(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]), digits = 2)),
  
  standard_deviantion = c(sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Mon" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Tue" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Wed" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Thu" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Fri" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sat" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]),
                          sd(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sun" & ride_data$rdistance != 0]))
)
kable(tri_rd_wday, style = "rmarkdown")

# visualise 
# create a box plot.
p_tri_rd_wday_box <- ggplot(ride_data, aes(x =  factor(start_day,
                                                       level = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                           y = rdistance, colour = membership, fill = membership)) + 
  geom_boxplot(stat = "boxplot", alpha = 0.3) +
  scale_y_continuous(limits = quantile(ride_data$rdistance, c(0.1, 0.9))) +
  xlab("Weekdays") +
  ylab("Ride Distance") +
  ggtitle("Ride Distance by Subscribers and Customers (daily Monday - Friday)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_fill_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_rd_wday_box
invisible(gc())
# subscribers consistently have shorter mean ride distance than customers.

# scatter points to visualise the entire data set.
ride_data$membership <- as.factor(ride_data$membership)
p_tri_rd_point_wday <- ggplot(ride_data, aes(x =  start_day, y = rdistance, colour = membership)) + 
  geom_jitter() +
  scale_y_continuous(labels = unit_format(unit = "km", scale = 1e-3), expand = c(0, 0), limits = c(0, NA)) +
  xlab("Weekdays") +
  ylab("Ride Distance") +
  ggtitle("Ride Distance by Subscribers and Customers (daily Monday - Friday)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_rd_point_wday
invisible(gc())

# create a line plot to see fluctuation of means during a given week explicitly.
p_tri_rd_line_wday <- ggplot(data = tri_rd_wday) +  
  geom_line(aes(x = factor(weekday, 
                           level = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                y = mean_distance, group = membership, color = membership)) +
  geom_line(linetype = "dashed", aes(x = factor(weekday, 
                                                level = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                     y = median_distance, group = membership, color = membership)) +
  xlab("Weekdays") +
  ylab("Ride Distance") +
  ggtitle("Ride Distance by Subscribers and Customers (daily Monday - Friday)") +
  scale_colour_manual(name = "membership", values = c("#41B6E6", "#E4002B")) + 
  scale_y_continuous(labels = unit_format(unit = "KM", scale = 1e-3), expand = c(0, 0), limits = c(0, NA)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p_tri_rd_line_wday

invisible(gc())

# perform statistic tests
# check for normality
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Mon"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Mon"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Tue"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Tue"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Wed"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Wed"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Thu"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Thu"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Fri"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Fri"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sat"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sat"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Subscriber" & ride_data$start_day == "Sun"])
# not normal
qqnorm(ride_data$rdistance[ride_data$membership == "Customer" & ride_data$start_day == "Sun"])
# not normal

# as the data is not normally distributed, we can use a non-parametric ANOVA to test whether the observed difference is statistically significant.
kruskal_sub_result <- kruskal.test(data = ride_data[ride_data$membership == "Subscriber", ], rdistance ~ start_day)
kruskal_sub_result
# p-value < 2.2e-16, and the difference among years are significant!

# let's follow up with a pairwise comparison to find out between which weekdays, the difference is significant.
# we can achieve this with Wilcoxon signed ranking test
# for subscribers.
wsr_sub_data <- ride_data[ride_data$membership == "Subscriber", ] %>%
  select(start_day, rdistance)
wsr_sub_data$start_year <- as.factor(wsr_sub_data$start_day)
wsr_sub_result <- dunn_test(data = wsr_sub_data, rdistance ~ start_day, p.adjust.method = "bonferroni")
wsr_sub_result
# for subscribers, ride distance is significantly different between weekdays, except for between Tuesday and Wednesday, and between Tuesday and Thursday.

# for customers.
kruskal_cus_result <- kruskal.test(data = ride_data[ride_data$membership == "Customer", ], rdistance ~ start_day)
kruskal_cus_result
# p-value < 2.2e-16, and the difference among years are significant!

# let's follow up with a pairwise comparison to find out between which weekdays, the difference is significant.
# we can achieve this with Wilcoxon signed ranking test
wsr_cus_data <- ride_data[ride_data$membership == "Customer", ] %>%
  select(start_day, rdistance)
wsr_cus_data$start_year <- as.factor(wsr_cus_data$start_day)
wsr_cus_result <- dunn_test(data = wsr_cus_data, rdistance ~ start_day, p.adjust.method = "bonferroni")
wsr_cus_result
# for customers, ride distance is significantly different between weekdays, except for the days among Wednesday through Friday.
invisible(gc())

# What are the geographic distributions of subscribers and customers in Chicago?
# convert dataframe numbers to GPS format
start_map <- ride_data[, c(8:10)]
start_map <- st_as_sf(start_map, coords = c("start_station_lon", "start_station_lat"))
start_map$membership <- as.factor(start_map$membership)
end_map <- ride_data[, c(8, 14, 15)]
end_map <- st_as_sf(end_map, coords = c("end_station_lon", "end_station_lat"))
end_map$membership <- as.factor(end_map$membership)

# remove the unused object to save RAM
invisible(gc())

# I have limited RAM on my computer so I need to transform and simplify the dataset
# data from 2013 to 2023 are compiled into one, and only the distinct rows are preserved
# the number of duplicate rows are counted and used as a scaling factor
# for start station
start_map <- start_map %>%  
  group_by_all() %>%  
  count
start_map$n <- (start_map$n)
colnames(start_map) <- c("membership", "geometry", "Number of Ride")
invisible(gc())

start_map_plot <- tm_shape(shp = chicago) + 
  tm_polygons(col = "#FFFAD7") + 
  tm_shape(start_map) + 
  tm_bubbles(col = "membership", palette = c("#41B6E6", "#E4002B"), jitter = 0.01  , size = "Number of Ride") +
  tm_layout(legend.outside = TRUE) +
  tm_layout(legend.position = c("right", "bottom"), 
            title= "Locations of End Stations", 
            title.position = c("right", "top"))
start_map_plot
invisible(gc())

end_map <- end_map %>%  
  group_by_all() %>%  
  count
end_map$n <- (end_map$n)
colnames(end_map) <- c("membership", "geometry", "Number of Ride")
invisible(gc())

# for end station
end_map_plot <- tm_shape(shp = chicago) + 
  tm_polygons(col = "#FFFAD7") + 
  tm_shape(end_map) + 
  tm_bubbles(col = "membership", palette = c("#41B6E6", "#E4002B"), jitter = 0.01  , size = "Number of Ride") +
  tm_layout(legend.outside = TRUE) +
  tm_layout(legend.position = c("right", "bottom"), 
            title= "Locations of End Stations", 
            title.position = c("right", "top"))
end_map_plot
invisible(gc())


