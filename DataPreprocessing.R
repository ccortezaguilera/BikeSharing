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
time_diff <- as.numeric(difftime(as.POSIXlt(trip_data$End.Date, 
                                            format = "%m/%d/%Y %H:%M"), as.POSIXlt(trip_data$Start.Date,
                                                                                   format = "%m/%d/%Y %H:%M"),
                                 units = "hours"))
dist_miles <- conv_unit(trip_data$distance, "m", "mi")
avg_speed <- dist_miles/time_diff
trip_data <- data.frame(trip_data, avg_speed)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculating total number of bikes checked each day

# Creating smaller sample set
statuswID234 <- status_data[status_data$station_id <= 4,]

curr_time = as.Date(statuswID234$time[4], format = "%Y-%m-%d %H:%M:%S");
curr_available = statuswID234$bikes_available[1]
curr_id = statuswID234$station_id[1]
total = 0

index <- 1:dim(statuswID234)[1]
j <- 1:(365*3)

station_id <- rep(0, dim(statuswID234)[1]/1400)
each_day <- rep(0, dim(statuswID234)[1]/1400)
day_total <- rep(0, dim(statuswID234)[1]/1400)
for(i in index){
  
  next_row_time <- as.Date(statuswID234[i,4], format = "%Y-%m-%d %H:%M:%S")
  if(curr_id != statuswID234$station_id[i] | next_row_time > curr_time){
    # Storing data at index j to new vectors
    station_id[j] <- curr_id
    each_day[j] <- as.Date(curr_time, format ="%Y-%m-%d", origin = "1970-01-01")
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
total_each_day <- data.frame(station_id,day_total, each_day) #origin date : 1970-01-01
colnames(total_each_day)[3] <- "date"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculating total number of bikes checked out each hour

#Creating an even smaller data set
statuswID2 <- status_data[status_data$station_id == 2,]

curr_time = as.POSIXlt(statuswID2$time[1], format = "%Y-%m-%d %H:%M:%S");
curr_available = statuswID2$bikes_available[1]
curr_id = statuswID2$station_id[1]
total = 0

index <- 1:dim(statuswID2)[1]
j <- 1:(dim(statuswID2)[1]/24)

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculating hourly totals of San Francisco stations
sf_IDs <- station_data[station_data$landmark == "San Francisco",]
sf_IDs <- sf_IDs$station_id

# Creating subset of San Francisco stations
SF_status_data <- status_data[status_data$station_id == sf_IDs,]

curr_time = as.POSIXlt(SF_status_data$time[1], format = "%Y-%m-%d %H:%M:%S");
curr_available = SF_status_data$bikes_available[1]
curr_id = SF_status_data$station_id[1]
total = 0

index <- 1:dim(SF_status_data)[1]
j <- 1:(dim(SF_status_data)[1]/24)

station_id <- rep(0, dim(SF_status_data)[1]/24)
day <- rep(0, dim(SF_status_data)[1]/24)
month <- rep(0, dim(SF_status_data)[1]/24)
hour <- rep(0, dim(SF_status_data)[1]/24)
hour_total <- rep(0, dim(SF_status_data)[1]/24)
for(i in index){
  
  next_row_time <- as.POSIXlt(SF_status_data[i,4], format = "%Y-%m-%d %H:%M:%S")
  if(curr_id != SF_status_data$station_id[i] | next_row_time$min < curr_time$min){
    # Storing data at index j to new vectors
    station_id[j] <- curr_id
    day[j] <- curr_time$wday
    month[j] <- curr_time$mon
    hour[j] <- curr_time$hour
    hour_total[j] <- total
    
    j <- j + 1
    # Updating variables
    total <- 0
    curr_available <- SF_status_data$bikes_available[i]
    curr_id <- SF_status_data$station_id[i]
  }
  curr_time <- next_row_time
  # If someone checks out a bike
  if(curr_available > SF_status_data$bikes_available[i]){
    
    total = total + (curr_available - SF_status_data$bikes_available[i])
    curr_available = SF_status_data$bikes_available[i]
  }
  
  # When someone checks in a bike, total is not effected
  else if(curr_available < SF_status_data$bikes_available[i]){
    
    curr_available = SF_status_data$bikes_available[i]
  }
}
SF_total_each_hour <- data.frame(station_id, hour_total, hour, day, month)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

