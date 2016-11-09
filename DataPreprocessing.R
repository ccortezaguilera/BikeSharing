## Importing data

# WARNING : Before importing trip data, ensure all are sorted in ascending order by date
trip_data_201508 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/201508_trip_data.csv", na.strings = "NA")
trip_data_201508 = na.omit(trip_data_201508)
weather_data_201508 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/201508_weather_data.csv", na.strings = "NA")
weather_data_201508 = na.omit(weather_data_201508)
station_data_201508 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/201508_station_data.csv", na.strings = "NA")
station_data_201508 = na.omit(station_data_201508)

trip_data_201608 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_3/201608_trip_data.csv", na.strings = "NA")
trip_data_201608 = na.omit(trip_data_201608)
weather_data_201608 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_3/201608_weather_data.csv", na.strings = "NA")
weather_data_201608 = na.omit(weather_data_201608)
station_data_201608 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_3/201608_station_data.csv", na.strings = "NA")
station_data_201608 = na.omit(station_data_201608)

trip_data_201402 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_1/201402_babs_open_data/201402_trip_data.csv", na.strings = "NA")
trip_data_201402 = na.omit(trip_data_201402)
weather_data_201402 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_1/201402_babs_open_data/201402_weather_data.csv", na.strings = "NA")
weather_data_201402 = na.omit(weather_data201402)
station_data_201402 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_1/201402_babs_open_data/201402_station_data.csv", na.strings = "NA")
station_data_201402 = na.omit(station_data201402)

trip_data_201408 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_1/201408_babs_open_data/201408_trip_data.csv", na.strings = "NA")
trip_data_201408 = na.omit(trip_data_201408)
weather_data_201408 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_1/201408_babs_open_data/201408_weather_data.csv", na.strings = "NA")
weather_data_201408 = na.omit(weather_data201408)
station_data_201408 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/babs_open_data_year_1/201408_babs_open_data/201408_station_data.csv", na.strings = "NA")
station_data_201408 = na.omit(station_data201408)

# Ensuring column names match and merging trip_data files
names(trip_data_201402)[10] <- "Subscriber.Type"
trip_data_201408 <- rbind(trip_data_201402, trip_data_201408)
rm(trip_data_201402)

trip_data <- rbind(trip_data_201408, trip_data_201508, trip_data_201608)
rm(trip_data_201408, trip_data_201508, trip_data_201608)

# Ensuring column names match and merging weather files
names(weather_data_201402) <- names(weather_data_201408)
weather_data_201408 <- rbind(weather_data_201402, weather_data_201408)
rm(weather_data201402)

names(weather_data_201608)[24] <- "Zip"
weather_data <- rbind(weather_data_201408, weather_data_201508, weather_data_201608)
rm(weather_data_201408, weather_data_201608)

# Load workspace
## load("DataPreprocessing.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculating total trips each day in SF

## Grabbing only San Francisco stations
SF_stations <- station_data_201608[station_data_201608$landmark == "San Francisco",]

SF_trips <- trip_data[trip_data$Start.Terminal %in% SF_stations$station_id,] # %in% checks if values in list

curr_time = as.POSIXlt(SF_trips$Start.Date[1], format = "%m/%d/%Y %H:%M");

index <- 1:nrow(SF_trips)
j <- 1

day <- rep(NA, 900)
weekday <- rep(NA, 900)
month <- rep(NA, 900)
year <- rep(NA, 900)
day_total <- rep(NA, 900)
sub_total <- rep(NA,900)
cust_total <- rep(NA,900)
date <- rep(NA, 900)
duration_total <- rep(NA, 900)
sub_dur_total <- rep(NA, 900)
cust_dur_total <- rep(NA, 900)

total = 0
num_sub = 0
num_cust = 0
dur_total = 0
sub_dur = 0
cust_dur = 0
for(i in index){
  
  next_row_time <- as.POSIXlt(SF_trips$Start.Date[i], format = "%m/%d/%Y %H:%M")
  if(next_row_time$yday != curr_time$yday | next_row_time$year != curr_time$year | i == nrow(SF_trips)){
    
    # Storing data at index j to new vectors
    day_total[j] <- total
    day[j] <- curr_time$mday
    weekday[j] <- curr_time$wday
    month[j] <- curr_time$mon
    year[j] <- curr_time$year + 3900
    sub_total[j] <- num_sub
    cust_total[j] <- num_cust
    sub_dur_total[j] <- sub_dur
    cust_dur_total[j] <- cust_dur
    duration_total[j] <- dur_total
    date[j] <- as.Date.POSIXlt(curr_time, format = "%m/%d/%Y")
    
    j <- j + 1
    
    # Updating variables
    total <- 0
    curr_time <- next_row_time
    num_cust <- 0
    num_sub <- 0
    dur_total <-0
    sub_dur <- 0
    cust_dur <- 0
  }
  else{
    total <- total + 1
    dur_total <- dur_total + SF_trips$Duration[i]
    if(SF_trips$Subscriber.Type[i] == "Subscriber"){
      num_sub <- num_sub + 1
      sub_dur <- sub_dur + SF_trips$Duration[i]
    }
    else{
      num_cust <- num_cust +1
      cust_dur <- cust_dur + SF_trips$Duration[i]
    }
  }
}

#date is in numeric form with origin 1970-01-01
#converting it to readable format
PDT <- as.Date(date, origin = "3970-01-01")
is_weekend <- ifelse(SF_totals$weekday == 0 | SF_totals$weekday == 6, 1, 0)
SF_totals <- data.frame(PDT, day_total, sub_total, cust_total, duration_total, sub_dur_total, cust_dur_total,is_weekend, day, weekday, month, year)

#~~~~~~~~~~~~~~~
# Merging weather and SF_totals

SF_weather_data <- weather_data[weather_data$Zip == 94107,]
#Changing format to match in both datasets
SF_weather_data$PDT <- as.Date(SF_weather_data$PDT, format = "%m/%d/%Y")
?merge

# Merging on dates based on PDT
# If SF_weather_data is missing a specific date, it will not 
# be added to the merged data set
SF_daily_bikeshare <- merge(SF_totals, SF_weather_data, by = "PDT", all.y  = TRUE, all.x = FALSE)


rm(curr_time,cust_dur, cust_dur_total,cust_total, date, day, day_total,
   dur_total, duration_total, i, index, is_weekend, j ,month, 
   next_row_time, num_cust, num_sub, PDT, sub_dur, sub_dur_total, 
   sub_total, total, weekday, year)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculating distance of each trip
install.packages("geosphere")
library(geosphere)
index <- 1:dim(trip_data)[1]
coordinates <- matrix(nrow = dim(trip_data)[1], ncol = 4)
colnames(coordinates) <- c("Long1", "Lat1", "Long2", "Lat2")
distance <- rep(0, dim(trip_data)[1])
for(i in index){
  start <- trip_data[i, 5] 
  end <- trip_data[i, 8]
  
  coordinates[i,1] <- station_data[station_data$station_id == start, 4]
  coordinates[i,2] <- station_data[station_data$station_id == start, 3]
  coordinates[i,3] <- station_data[station_data$station_id == end, 4]
  coordinates[i,4] <- station_data[station_data$station_id == end, 3]
  
  start_pos <- c(station_data[station_data$station_id == start, 4], station_data[station_data$station_id == start, 3])
  end_pos <- c(station_data[station_data$station_id == end, 4], station_data[station_data$station_id == end, 3])
  distance[i] <- distCosine(start_pos, end_pos)
}
trip_data <- data.frame(trip_data, distance)

# Calculating avg speed of each trip based on displacement

install.packages("measurements")
library(measurements)
?conv_unit

dist_miles <- conv_unit(trip_data$distance, "m", "mi")
avg_speed <- dist_miles/(trip_data$Duration/60)
trip_data <- data.frame(trip_data, dist_miles, avg_speed)

