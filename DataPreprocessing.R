## Importing data
station_data = read.csv("201508_station_data.csv", na.strings = "NA")
station_data = na.omit(station_data)
trip_data = read.csv("201508_trip_data.csv", na.strings = "NA")
trip_data = na.omit(trip_data)
weather_data = read.csv("201508_weather_data.csv", na.strings = "NA")
weather_data = na.omit(weather_data)
status_data = read.csv("201508_status_data.csv", na.strings = "NA")
status_data = na.omit(status_data)

# Load workspace
## load("DataPreprocessing.RData")

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculating total number of bikes checked each day

# Creating smaller sample set
statuswID234 <- status_data[status_data$station_id <= 4,]

curr_time = as.POSIXlt(statuswID234$time[1], format = "%Y-%m-%d %H:%M:%S");
curr_available = statuswID234$bikes_available[1]
curr_id = statuswID234$station_id[1]
total = 0

index <- 1:dim(status_data)[1]
j <- 1

station_id <- rep(0, dim(statuswID234)[1]/1400)
day <- rep(0, dim(statuswID234)[1]/1400)
month <- rep(0, dim(statuswID234)[1]/1400)
weekday <- rep(0, dim(statuswID234)[1]/1400)
day_total <- rep(0, dim(statuswID234)[1]/1400)
for(i in index){
  
  next_row_time <- as.POSIXlt(statuswID234[i,4], format = "%Y-%m-%d %H:%M:%S")
  if(curr_id != statuswID234$station_id[i] | next_row_time$yday > curr_time$yday | next_row_time$year > curr_time$year){
    # Storing data at index j to new vectors
    station_id[j] <- curr_id
    weekday[j] <- curr_time$wday
    month[j] <- curr_time$mon
    day[j] <- curr_time$mday
    day_total[j] <- total
    
    j <- j + 1
    # Updating variables
    total <- 0
    curr_available <- statuswID234$bikes_available[i]
    curr_time <- next_row_time
    curr_id <- statuswID234$station_id[i]
  }
  
  # If someone checks out a bike
  if(curr_available > statuswID234$bikes_available[i]){
    
    total = total + (curr_available - statuswID234$bikes_available[i])
    curr_available = statuswID234$bikes_available[i]
  }
  
  # When someone checks in a bike, total is not effected
  else if(curr_available < statuswID234$bikes_available[i]){
    
    curr_available = statuswID234$bikes_available[i]
  }
}
total_each_day <- data.frame(station_id, day_total, day, weekday, month)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculating total number of bikes checked out each hour

#Creating an even smaller data set
statuswID2 <- status_data[status_data$station_id == 2,]

curr_time = as.POSIXlt(statuswID2$time[1], format = "%Y-%m-%d %H:%M:%S");
curr_available = statuswID2$bikes_available[1]
curr_id = statuswID2$station_id[1]
total = 0

index <- 1:dim(statuswID2)[1]
j <- 1

station_id <- rep(0, dim(statuswID2)[1]/24)
day <- rep(0, dim(statuswID2)[1]/24)
month <- rep(0, dim(statuswID2)[1]/24)
hour <- rep(0, dim(statuswID2)[1]/24)
hour_total <- rep(0, dim(statuswID2)[1]/24)
for(i in index){
  
  next_row_time <- as.POSIXlt(statuswID2[i,4], format = "%Y-%m-%d %H:%M:%S")
  if(curr_id != statuswID2$station_id[i] | next_row_time$min < curr_time$min){
    # Storing data at index j to new vectors
    station_id[j] <- curr_id
    day[j] <- curr_time$wday
    month[j] <- curr_time$mon
    hour[j] <- curr_time$hour
    hour_total[j] <- total
    
    j <- j + 1
    # Updating variables
    total <- 0
    curr_available <- statuswID2$bikes_available[i]
    curr_id <- statuswID2$station_id[i]
  }
  curr_time <- next_row_time
  # If someone checks out a bike
  if(curr_available > statuswID2$bikes_available[i]){
    
    total = total + (curr_available - statuswID2$bikes_available[i])
    curr_available = statuswID2$bikes_available[i]
  }
  
  # When someone checks in a bike, total is not effected
  else if(curr_available < statuswID2$bikes_available[i]){
    
    curr_available = statuswID2$bikes_available[i]
  }
}
total_each_hour <- data.frame(station_id, hour_total, hour, day, month)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculating total trips each day in SF

# Grabbing SF station IDs
sf_IDs <- station_data[station_data$landmark == "San Francisco",]
sf_IDs <- sf_IDs$station_id

SF_trips <- trip_data[trip_data$Start.Terminal %in% sf_IDs,] # %in% checks if values in list

curr_time = as.POSIXlt(SF_trips$Start.Date[1], format = "%m/%d/%Y %H:%M");

index <- 1:nrow(SF_trips)
j <- 1

day <- rep(0, 400)
weekday <- rep(0, 400)
month <- rep(0, 400)
year <- rep(0, 400)
day_total <- rep(0, 400)
sub_total <- rep(0,400)
cust_total <- rep(0,400)
date <- rep(0, 400)
duration_total <- rep(0, 400)
sub_dur_total <- rep(0, 400)
cust_dur_total <- rep(0, 400)

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
    year[j] <- curr_time$year - 100
    sub_total[j] <- num_sub
    cust_total[j] <- num_cust
    sub_dur_total[j] <- sub_dur
    cust_dur_total[j] <- cust_dur
    duration_total[j] <- dur_total
    date[j] <- as.Date(SF_trips$Start.Date[i], format = "%m/%d/%Y")
    
    j <- j + 1
    
    # Updating variables
    total <- 0
    curr_time <- next_row_time
    num_cust <- 0
    num_sub <- 0
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
PDT <- as.Date(date, origin = "1970-01-01")
SF_totals <- data.frame(PDT, day_total, sub_total, cust_total, duration_total, sub_dur_total, cust_dur_total, day, weekday, month, year)

is_weekend = ifelse(SF_totals$weekday == 0 | SF_totals$weekday == 6, 1, 0)
SF_totals <- data.frame(SF_totals, is_weekend)

#~~~~~~~~~~~~~~~
# Mergeing weather and SF_totals

SF_weather_data <- weather_data[weather_data$Zip == 94107,]
#Changing format to match in both datasets
SF_weather_data$PDT <- as.Date(SF_weather_data$PDT, format = "%m/%d/%Y")
?merge

#Mergeing on dates
SF_daily_bikeshare <- merge(SF_totals, SF_weather_data, by = "PDT")

rm(curr_time,cust_dur, cust_dur_total,cust_total, date, day, day_total,
   dur_total, duration_total, holidays, i, index, is_weekend, j ,month, 
   next_row_time, num_cust, num_sub, PDT, sf_IDs, sub_dur, sub_dur_total, 
   sub_total, total, weekday, year)
#~~~~~~~~~~~~~~~~~~~~DATA EXPLORATION~~~~~~~~~~~~~~~~~~

attach(SF_daily_bikeshare)
par(mfrow = c(2,2))
plot(weekday, sub_total, xlab = "Day of the week", ylab = "Number of rentals", main = "Subscribers", col = "skyblue")
plot(weekday, cust_total, xlab = "Day of the week", ylab = "Number of rentals", main = "Customers", col = "slateblue4")
plot(PDT, sub_total, col = "orange", xlab = "Month", ylab = "Number of rentals", main = "Subscribers")
plot(PDT, cust_total, col = "darkred", xlab = "Month", ylab = "Number of rentals", main = "Customers")

dev.off()

par(mfrow = c(2,1))
plot(PDT, day_total, type = 'line', col = "forestgreen", xlab = "Month", ylab = "Number of rentals", main = "Rentals each month")
plot(PDT, Mean.TemperatureF, type = 'line', col = "red", xlab = "Month", ylab = "Temperature in F", main = "Temperature each month ")
lines(PDT, Max.TemperatureF, col = "orange")
lines(PDT, Min.TemperatureF, col = "lightblue")
dev.off()
