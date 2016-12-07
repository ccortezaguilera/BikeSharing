## Importing data

# WARNING : Before importing trip data, ensure all are sorted in ascending order by date
trip_data_201508 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_2/201508_trip_data.csv", na.strings = "NA")
trip_data_201508 = na.omit(trip_data_201508)
weather_data_201508 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_2/201508_weather_data.csv", na.strings = "NA")
weather_data_201508 = na.omit(weather_data_201508)
station_data_201508 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_2/201508_station_data.csv", na.strings = "NA")
station_data_201508 = na.omit(station_data_201508)

trip_data_201608 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_3/201608_trip_data.csv", na.strings = "NA")
trip_data_201608 = na.omit(trip_data_201608)
weather_data_201608 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_3/201608_weather_data.csv", na.strings = "NA")
weather_data_201608 = na.omit(weather_data_201608)
station_data_201608 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_3/201608_station_data.csv", na.strings = "NA")
station_data_201608 = na.omit(station_data_201608)

trip_data_201402 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_1/201402_babs_open_data/201402_trip_data.csv", na.strings = "NA")
trip_data_201402 = na.omit(trip_data_201402)
weather_data_201402 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_1/201402_babs_open_data/201402_weather_data.csv", na.strings = "NA")
weather_data_201402 = na.omit(weather_data_201402)
station_data_201402 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_1/201402_babs_open_data/201402_station_data.csv", na.strings = "NA")
station_data_201402 = na.omit(station_data_201402)

trip_data_201408 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_1/201408_babs_open_data/201408_trip_data.csv", na.strings = "NA")
trip_data_201408 = na.omit(trip_data_201408)
weather_data_201408 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_1/201408_babs_open_data/201408_weather_data.csv", na.strings = "NA")
weather_data_201408 = na.omit(weather_data_201408)
station_data_201408 = read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/babs_open_data_year_1/201408_babs_open_data/201408_station_data.csv", na.strings = "NA")
station_data_201408 = na.omit(station_data_201408)

# Ensuring column names match and merging trip_data files
names(trip_data_201402)[10] <- "Subscriber.Type"
trip_data_201408 <- rbind(trip_data_201402, trip_data_201408)
rm(trip_data_201402)

trip_data <- rbind(trip_data_201408, trip_data_201508, trip_data_201608)
rm(trip_data_201408, trip_data_201508, trip_data_201608)

# Ensuring column names match and merging weather files
names(weather_data_201402) <- names(weather_data_201408)
weather_data_201408 <- rbind(weather_data_201402, weather_data_201408)
rm(weather_data_201402)

names(weather_data_201608)[24] <- "Zip"
weather_data <- rbind(weather_data_201408, weather_data_201508, weather_data_201608)
rm(weather_data_201408, weather_data_201608, weather_data_201508)

# PrecipitationIn column values with T are measured as >0.01
weather_data[weather_data$PrecipitationIn == "T","PrecipitationIn"] <- 0.01

# Categorizing events from 0-4
# 4 = Rain-Storm, 3 = Rain, 2 = Fog-Rain, 1 = Fog
weather_events <- ifelse(weather_data$Events == "Rain-Thunderstorm", 4, 
                         ifelse(weather_data$Events == "Rain" | weather_data$Events == "rain", 3,
                         ifelse(weather_data$Events == "Fog-Rain", 2,
                         ifelse(weather_data$Events == "Fog", 1, 0))))
weather_data$Events <- weather_events
rm(weather_events)

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

#date is in numeric form with origin 3970-01-01
#converting it to readable format
PDT <- as.Date(date, origin = "3970-01-01")
is_weekend <- ifelse(weekday == 0 | weekday == 6, 1, 0)
SF_totals <- data.frame(PDT, day_total, sub_total, cust_total, duration_total, sub_dur_total, cust_dur_total,is_weekend, day, weekday, month, year)

#~~~~~~~~~~~~~~~
# Merging weather and SF_totals

SF_weather_data <- weather_data[weather_data$Zip == 94107,]

# Removing Zip code column
SF_weather_data <-SF_weather_data[,-24]

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
   sub_total, total, weekday, year, station_data_201402, station_data_201408, station_data_201508, station_data_201608)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~Adding Features~~~~~~~~~~~~~~~~~~~~
# ~~~~~Adding qualitative feature value for days when a Giants home game occurs~~~~~~
Giants_Games <- read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/SF_Giants_Home_Games.csv")

# Changing the format of the dates column
# +730485 to alter year to 2013
Giants_Games$Date <- as.Date(Giants_Games$Date, format = "%m/%d/%Y") + 730485

# Creating an empty matrix with all dates over three years with 0s as their Game occurence value
Game <- rep(0,1083)
Game_with_Dates <- data.frame(Date = SF_daily_bikeshare$PDT, Game)

# Adding 1s to specific dates Game_with_Dates matrix where a Giants game occured
index <- 1
for(i in 1:nrow(Giants_Games)){
  for(j in index:nrow(Game_with_Dates)){
    if(Giants_Games$Date[i] == Game_with_Dates$Date[j]){
      
      Game_with_Dates$Game[j] = 1
      
      index = j + 1
      break
    }
  }
}

# Adding to the main dataset
SF_daily_bikeshare <- data.frame(SF_daily_bikeshare, Giants_Game = Game_with_Dates$Game)
rm(Game_with_Dates,Giants_Games,Game,i,j, index)

#~~~~~~~Adding qualitative feature value for days when a large SF event occurs that
#~~~~~~~may effect bicycle travel. 1 is SF Sunday events. 2 is SF Bike to work days
SF_events <- read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/SF_Events.csv")
SF_events$Date <- as.Date(SF_events$Date, format = "%m/%d/%Y") + 730485
Event <- rep(0, 1083)
Event_with_Dates <- data.frame(Date = SF_daily_bikeshare$PDT, Event)

index <- 1
for(i in 1:nrow(SF_events)){
  for(j in index:nrow(Event_with_Dates)){
    if(SF_events$Date[i] == Event_with_Dates$Date[j]){
      Event_with_Dates$Event[j] = SF_events$Event[i]
      index = j + 1
      break
    }
  }
}
SF_daily_bikeshare <- data.frame(SF_daily_bikeshare, SF_Event = Event_with_Dates$Event)
rm(SF_events, Event_with_Dates,Event,i,j, index)

#~~~~~~~Adding end of the week avg gas price in San Francisco
SF_gas_avg <- read.csv("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/Gas_Prices.csv")
SF_gas_avg$End.Date <- as.Date(SF_gas_avg$End.Date, format = "%m/%d/%Y") + 730485
Gas_Value <- rep(0,1083)
Gas_with_Dates <- data.frame(Date = SF_daily_bikeshare$PDT, Gas_Value)

# When adding end of week gas prices, the end of week gas value is reflected on
# all the days within that week.
index <- 1
for(i in 1:nrow(SF_gas_avg)){
  curr_gas_val <- SF_gas_avg$Value[i]
  for(j in index:nrow(Gas_with_Dates)){
    Gas_with_Dates$Gas_Value[j] <- curr_gas_val
    if(SF_gas_avg$End.Date[i] == Gas_with_Dates$Date[j]){
      
      if(i == nrow(SF_gas_avg)){
        
        for(k in 1:2){
          
          Gas_with_Dates$Gas_Value[j+ k] <- curr_gas_val
        }
      }
      
      index = j + 1
      break
    }
  }
}

SF_daily_bikeshare <- data.frame(SF_daily_bikeshare, Avg_Gas_Value = Gas_with_Dates$Gas_Value)
rm(Gas_with_Dates, SF_gas_avg, curr_gas_val, Gas_Value, i, index, j, k)

rm(SF_stations, SF_totals, SF_trips, SF_weather_data, trip_data, weather_data)


#~~~~~~~~~~~~~~~~~~~~~~ Calculating distance of each trip in San Francisco ~~~~~~~~~~~~~~~~~
# WARNING : THIS SCRIPT DOES NOT WORK AS SOME TRIPS ARE MADE FROM CITY TO CITY
# SINCE SOME STATIONS MAY HAVE BEEN REMOVED OR IDs WERE CHANGED, THIS SCRIPT DOES
# NOT ACCOUNT FOR THOSE CHANGES.
# install.packages("geosphere")
library(geosphere)
index <- 1:dim(SF_trips)[1]
coordinates <- matrix(nrow = dim(SF_trips)[1], ncol = 4)
colnames(coordinates) <- c("Long1", "Lat1", "Long2", "Lat2")
distance <- rep(0, dim(SF_trips)[1])
for(i in index){
  start <- SF_trips[i, 5] 
  end <- SF_trips[i, 8]
  
  coordinates[i,1] <- SF_stations[SF_stations$station_id == start, 4]
  coordinates[i,2] <- SF_stations[SF_stations$station_id == start, 3]
  coordinates[i,3] <- SF_stations[SF_stations$station_id == end, 4]
  coordinates[i,4] <- SF_stations[SF_stations$station_id == end, 3]
  
  start_pos <- c(SF_stations[SF_stations$station_id == start, 4], SF_stations[SF_stations$station_id == start, 3])
  end_pos <- c(SF_stations[SF_stations$station_id == end, 4], SF_stations[SF_stations$station_id == end, 3])
  distance[i] <- distCosine(start_pos, end_pos)
}
SF_trips <- data.frame(SF_trips, distance)

# Calculating avg speed of each trip based on displacement

#install.packages("measurements")
library(measurements)
?conv_unit

dist_miles <- conv_unit(trip_data$distance, "m", "mi")
avg_speed <- dist_miles/(trip_data$Duration/60)
trip_data <- data.frame(trip_data, dist_miles, avg_speed)