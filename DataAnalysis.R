# Data analysis

hist(SF_total_each_hour$station_id, col = "gray", labels = TRUE)
plot(SF_total_each_hour$station_id, SF_total_each_hour$hour_total)
plot(SF_total_each_hour$month, SF_total_each_hour$hour_total, ylab = "totals", xlab = "Month" )

