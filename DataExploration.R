#~~~~~~~~~~~~~~~~~~~~DATA EXPLORATION~~~~~~~~~~~~~~~~~~
load("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/DataPreprocessing.RData")

attach(SF_daily_bikeshare)
summary(SF_daily_bikeshare)
pairs(~PDT+duration_total+sub_dur_total+cust_dur_total+day_total+
        sub_total+cust_total+Mean.TemperatureF+Mean.Humidity+
        Mean.Wind.SpeedMPH+Mean.VisibilityMiles+ is_weekend + 
        day + weekday + WindDirDegrees + PrecipitationIn)

par(mfrow = c(2,2))
plot(weekday, sub_total, xlab = "Day of the week", ylab = "Number of rentals", main = "Subscribers", col = "skyblue")
plot(weekday, cust_total, xlab = "Day of the week", ylab = "Number of rentals", main = "Customers", col = "slateblue4")
plot(PDT, sub_total, col = ifelse(is_weekend == 1,"orange","steelblue"), xlab = "Month", ylab = "Number of rentals", main = "Subscribers")
plot(PDT, cust_total, col = "darkred", xlab = "Month", ylab = "Number of rentals", main = "Customers")
# Subscribers are more active on weekdays with customers on weekends
# Between late Dec through early Jan there is a decrease in Subscribers
dev.off()

par(mfrow = c(2,1))
plot(PDT, day_total, type = 'l', col = "forestgreen", xlab = "Month", ylab = "Number of rentals", main = "Rentals each month")
plot(PDT, Mean.TemperatureF, type = 'l', col = "red", xlab = "Month", ylab = "Temperature in F", main = "Temperature each month ")
lines(PDT, Max.TemperatureF, col = "orange")
lines(PDT, Min.TemperatureF, col = "lightblue")
# Decrease in temperature during winter time
dev.off()

par(mfrow = c(1,2))
plot(weekday, duration_total/60, xlab = "Day of the week", ylab= "Duration in minutes", main = "Duration totals per day", col = "slateblue")
plot(PDT, duration_total/60, xlab = "Month", ylab = "Duration in minutes", main = "Duration totals throughout the year", col = "firebrick")
# Notice the outlier
dev.off()

which.max(duration_total)#clear outlier where duration is about 173.5 days at index 96

par(mfrow = c(2,2))
plot(weekday, sub_dur_total/60, xlab = "Day of the week", ylab = "Duration in minutes", main = "Subscribers", col = "skyblue")
plot(weekday[-96], cust_dur_total[-96]/60, xlab = "Day of the week", ylab = "Duration in minutes", main = "Customers", col = "slateblue4")
plot(PDT, sub_dur_total/60, col = ifelse(is_weekend ==1,"orange", "steelblue"), xlab = "Month", ylab = "Duration in minutes", main = "Subscribers")
plot(PDT[-96], cust_dur_total[-96]/60, col = ifelse(is_weekend == 1, "darkred", "lightblue"), xlab = "Month", ylab = "Duration in minutes", main = "Customers")
# Similar results as total rentals
which.max(cust_dur_total)
dev.off()

par(mfrow = c(1, 2))
hist(SF_trips$Start.Terminal, xlab = "Station ID", ylab = "Number of occurences", main = "Start Station Stats", col = "darkseagreen3", breaks = 42)
abline(v = median(SF_trips$Start.Terminal), lwd = 2, col = "royalblue")
hist(SF_trips$End.Terminal, xlab = "Station ID", ylab = "Number of occurences", main = "End Station stats", col = "firebrick", breaks = 42)
abline(v = median(SF_trips$End.Terminal), lwd = 2, col = "royalblue")
# Stations 68 & 69 are most popular

dev.off()

plot(SF_trips$Subscriber.Type, SF_trips$distance)
#Outliers are evident
which(SF_trips$distance > 20000)
#Outliers at indexes 16804 156116 317828 320941

plot(SF_trips$Subscriber.Type[-c(16804,156116,317828,320941)] ,SF_trips$distance[-c(16804,156116,317828,320941)])
# Not much difference between Subscribers and Customers based on distance

type <- ifelse(SF_trips$Subscriber.Type == "Subscriber", 1, 0)
plot(SF_trips$Subscriber.Type[-c(16804,156116,317828,320941)] ,SF_trips$avg_speed[-c(16804,156116,317828,320941)], ylab = "MPH", main = "Customers Vs Subscribers \n based on AVG speed",
     col = ifelse(type == 0, "goldenrod", "darkgreen"))
# Larger difference when looking at avg_speed
