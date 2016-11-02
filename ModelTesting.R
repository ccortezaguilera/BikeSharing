#~~~~~~~~~~~~~~~~~~~Model Testing~~~~~~~~~~~~~~~~~~

load("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/DataPreprocessing.RData")
# Using best subset selection
regit.full = regsubsets(day_total~duration_total+ is_weekend + month + Mean.TemperatureF + Mean.VisibilityMiles + Mean.Wind.SpeedMPH + Mean.Humidity ,data = SF_daily_bikeshare)
regit.full
regfit.summary = summary(regit.full)
plot(regit.full, scale = "adjr2")
plot(regfit.summary$adjr2, col = "darkblue", xlab = "Number of predictors", ylab = "Adjusted R^2", main = "Best Subset Selection", type = 'l')
which.max(regfit.summary$adjr2)
points (6, regfit.summary$adjr2[6], col ="red",cex =2, pch =20)

# Highest adjr^2 at model with 6 predictors
lm.fit <- lm(day_total ~ duration_total+is_weekend+month+Mean.TemperatureF+Mean.VisibilityMiles+Mean.Wind.SpeedMPH, data = SF_daily_bikeshare)
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()

# Adding ratio to model to see if it has any effect
ratio <- sub_total/cust_total
lm.fit <- lm(day_total ~ duration_total+is_weekend+month+Mean.TemperatureF+Mean.VisibilityMiles+Mean.Wind.SpeedMPH + ratio, data = SF_daily_bikeshare)
summary(lm.fit)

