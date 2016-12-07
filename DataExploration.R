#~~~~~~~~~~~~~~~~~~~~DATA EXPLORATION~~~~~~~~~~~~~~~~~~
load("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/DataPreprocessing.RData")

attach(SF_daily_bikeshare)
summary(SF_daily_bikeshare)

par(mfrow = c(2,2))
plot(weekday, sub_total, xlab = "Day of the week", ylab = "Number of rentals", main = "Subscribers", col = "skyblue")
plot(weekday, cust_total, xlab = "Day of the week", ylab = "Number of rentals", main = "Customers", col = "slateblue4")
plot(PDT, sub_total, col = ifelse(is_weekend == 1,"orange","steelblue"), xlab = "Year", ylab = "Number of rentals", main = "Subscribers")
legend(locator(), c("Weekday", "Weekend"), col = c("steelblue", "orange"), pch = 1, cex = 0.6)
plot(PDT, cust_total, col = ifelse(is_weekend == 1,"orange", "steelblue"), xlab = "Year", ylab = "Number of rentals", main = "Customers")
legend(locator(), c("Weekday", "Weekend"), col = c("steelblue", "orange"), pch = 1, cex = 0.6)
# Subscribers are more active on weekdays with customers on weekends
# Between late Dec through early Jan there is a decrease in Subscribers
dev.off()

par(mfrow = c(2,1))
plot(PDT, day_total, type = 'l', col = "forestgreen", xlab = "Date", ylab = "Number of rentals", main = "Rentals each month")
plot(PDT, Mean.TemperatureF, type = 'l', col = "red", xlab = "Date", ylab = "Temperature in F", main = "Temperature each month ")
lines(PDT, Max.TemperatureF, col = "orange")
lines(PDT, Min.TemperatureF, col = "lightblue")
legend(locator(), c("MaxTemp", "MeanTemp", "MinTemp"), col = c("orange", "red", "lightblue"), lty = c(1,1), cex = 0.5, bty = "n")
# Decrease in temperature during winter time
dev.off()

par(mfrow = c(1,2))
plot(weekday, duration_total/60, xlab = "Day of the week", ylab= "Duration in minutes", main = "Duration totals per day", col = "slateblue")
plot(PDT, duration_total/60, xlab = "Month", ylab = "Duration in minutes", main = "Duration totals throughout the year", col = "firebrick")
# Notice the outlier
dev.off()

which.max(duration_total)#clear outlier where duration at index 457
SF_daily_bikeshare[which.max(duration_total), "duration_total"]
SF_daily_bikeshare <- SF_daily_bikeshare[-457,] # Removing outlier

attach(SF_daily_bikeshare)

par(mfrow = c(1,2))
plot(weekday, duration_total/60, xlab = "Day of the week", ylab= "Duration in minutes", main = "Duration totals per day", col = "slateblue")
plot(PDT, duration_total/60, xlab = "Month", ylab = "Duration in minutes", main = "Duration totals throughout the year", col = "firebrick")
# Notice another outlier

which.max(duration_total)#clear outlier where duration at index 655
SF_daily_bikeshare[which.max(duration_total), "duration_total"]
SF_daily_bikeshare <- SF_daily_bikeshare[-655,] # Removing outlier

attach(SF_daily_bikeshare)

par(mfrow = c(1,2))
plot(weekday, duration_total/60, xlab = "Day of the week", ylab= "Duration in minutes", main = "Duration totals per day", col = "slateblue")
plot(PDT, duration_total/60, xlab = "Month", ylab = "Duration in minutes", main = "Duration totals throughout the year", col = "firebrick")
# Outliers removed


par(mfrow = c(2,2))
plot(weekday, sub_dur_total/60, xlab = "Day of the week", ylab = "Duration in minutes", main = "Subscribers", col = "skyblue")
plot(weekday[-96], cust_dur_total[-96]/60, xlab = "Day of the week", ylab = "Duration in minutes", main = "Customers", col = "slateblue4")
plot(PDT, sub_dur_total/60, col = ifelse(is_weekend ==1,"orange", "steelblue"), xlab = "Month", ylab = "Duration in minutes", main = "Subscribers")
legend(locator(), c("Weekday", "Weekend"), col = c("steelblue", "orange"), pch = 1, cex = 0.6)
plot(PDT[-96], cust_dur_total[-96]/60, col = ifelse(is_weekend == 1, "orange", "steelblue"), xlab = "Month", ylab = "Duration in minutes", main = "Customers")
legend(locator(), c("Weekday", "Weekend"), col = c("steelblue", "orange"), pch = 1, cex = 0.6)


# ~~ Analyzing docking popularity
par(mfrow = c(1, 2))
hist(SF_trips$Start.Terminal, xlab = "Station ID", ylab = "Number of occurences", main = "Start Station Stats", col = "darkseagreen3", breaks = 42)
hist(SF_trips$End.Terminal, xlab = "Station ID", ylab = "Number of occurences", main = "End Station stats", col = "firebrick", breaks = 42)
# Stations 68 & 69 are most popular

dev.off()

#~~~~ Analyzing distance and speed~~~~
# NOT APPLICABLE FOR FINAL DATASET AS THESE VARIABLES WERE NOT CONSIDERED
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rentals and gas prices
par(mfrow = c(2,1))
plot(PDT, day_total, type = 'l', col = "lightblue", xlab = "Date", ylab = "Number of rentals", main = "Rentals each day")
plot(PDT, Avg_Gas_Value, type = 'l', col = "orange", xlab = "Date", ylab = "Gas Prices", main = "Average Gas Prices")
dev.off()

# Rentals and giants games
plot(PDT, day_total, panel.first = lines(PDT, Giants_Game*1400, type = 'h', col = "beige"), 
      type = 'l', col = "lightblue", xlab = "Date", ylab = "Number of rentals", ylim = c(100,1400), 
      main = "Rentals each day")
legend(locator(), c("Num Rentals", "Giants Game"), col = c("lightblue", "beige"), pch = '-', lwd = 2, cex = 0.85)

