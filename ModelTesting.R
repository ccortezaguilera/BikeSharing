#~~~~~~~~~~~~~~~~~~~Model Testing~~~~~~~~~~~~~~~~~~

load("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/DataPreprocessing.RData")
# Using best subset selection
library(leaps)
attach(SF_daily_bikeshare)
set.seed(10)
train <- sample(1:1083, 700)

regfit.full = regsubsets(day_total~duration_total+ is_weekend + month + Mean.TemperatureF + Mean.VisibilityMiles+ WindDirDegrees,data = SF_daily_bikeshare, subset = train)
regfit.full
regfit.summary = summary(regfit.full)
plot(regfit.full, scale = "adjr2")
plot(regfit.summary$adjr2, col = "darkblue", xlab = "Number of predictors", ylab = "Adjusted R^2", main = "Best Subset Selection", type = 'l')
which.max(regfit.summary$adjr2)
points (6, regfit.summary$adjr2[6], col ="red",cex =2, pch =20)

# Highest adjr^2 at model with all 6 predictors
# Creating linear model to predict duration_total
lm.fit <- lm(day_total~duration_total+ is_weekend + month + Mean.TemperatureF + Mean.VisibilityMiles+ WindDirDegrees, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred = predict(lm.fit, newdata = SF_daily_bikeshare[-train,])
test.vals <- SF_daily_bikeshare$day_total[-train]
mean((test.vals - lm.pred)^2)
# MSE of 34378.4 =(

# Creating linear model to predict sub_total only
lm.fit <- lm(sub_total~ is_weekend + month + Mean.TemperatureF + Mean.VisibilityMiles+ WindDirDegrees, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred_sub = predict(lm.fit, newdata = SF_daily_bikeshare[-train,])
test.vals <- SF_daily_bikeshare$sub_total[-train]
mean((test.vals - lm.pred_sub)^2)
# MSE of over 30k =(

# Creating linear model to predict cust_total only
lm.fit <- lm(cust_total ~is_weekend + month + Mean.TemperatureF + Mean.VisibilityMiles+ WindDirDegrees, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred_cust = predict(lm.fit, newdata = SF_daily_bikeshare[-train,])
test.vals_cust <- SF_daily_bikeshare$cust_total[-train]
mean((test.vals_cust - lm.pred_cust)^2)
# MSE of 1366.994

# Adding each model and comparing to day_total.
lm.pred_total <- lm.pred_sub + lm.pred_cust
mean((lm.pred_total - SF_daily_bikeshare$day_total[-train])^2)
# MSE of 34.8K
#Using forward subset selection
plot(regsubsets(day_total~.-cust_total-sub_total-Zip-PDT-Events, 
                data = SF_daily_bikeshare, method = "forward"), scale = "adjr2")
# Highest adjr^2 with predictors : is_weekend,sub_dur_total,Mean.Temp, 
# Mean.Visibility, Mean.SeaLevel, CloudCover

summary(lm(day_total~is_weekend+sub_dur_total + Mean.TemperatureF + Mean.Humidity+
        Mean.VisibilityMiles + Mean.Sea.Level.PressureIn+CloudCover, data = SF_daily_bikeshare))
set.seed(1)
train <- sample(1:359, 250)
lm.fit <- lm(day_total~is_weekend+sub_dur_total + Mean.TemperatureF + Mean.Humidity+
               Mean.VisibilityMiles + Mean.Sea.Level.PressureIn+CloudCover, data = SF_daily_bikeshare[train,])
lm.pred = predict(lm.fit, SF_daily_bikeshare[-train,])
sqrt(mean((lm.pred - day_total[-train])^2))
day_total[-train]
lm.pred

set.seed(1)
library(tree)
train <- sample(1:359, 250)
tree.bikes = tree(sub_total~is_weekend+month+Mean.Humidity+Mean.TemperatureF+Mean.VisibilityMiles+Mean.Wind.SpeedMPH, data = SF_daily_bikeshare, subset = train)
plot(tree.bikes)
text(tree.bikes)
cv.tree = cv.tree(tree.bikes)
plot(cv.tree$size ,cv.tree$dev ,type="b")
prune.bikes = prune.tree(tree.bikes, best = 4)
plot(prune.bikes)
text(prune.bikes)

yhat = predict(tree.bikes, newdata = SF_daily_bikeshare[-train,])
bikes.test = SF_daily_bikeshare[-train, "duration_total"]
plot(yhat, bikes.test)
abline(0,1)
mean((yhat-bikes.test)^2)


library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)