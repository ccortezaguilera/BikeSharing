#~~~~~~~~~~~~~~~~~~~Model Testing~~~~~~~~~~~~~~~~~~

load("/Users/lancefernando/Desktop/DataMining/DataSets/BayAreaBikeShareData/DataPreprocessing.RData")
##~~~~~~~~~~~~~~~~Round 1 of model testing~~~~~~~~~~~~~~~~~~~
# RESULTS IN ROUND 1 BASED ON DATA FROM 9/1/14-8/31/15
# Using best subset selection
library(leaps)
attach(SF_daily_bikeshare)
set.seed(10)
train <- sample(1:363, 200)

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


## ~~~~~~~~~~~~Round 2 of model fitting~~~~~~~~~~~~~~~~~~~~
# RESULTS BASED ON DATA FROM 8/29/13 to 9/1/16
set.seed(101)
train <- sample(1:1083, 700)

# ~~~~~~~~~~~~Running basic linear regression models~~~~~~~~~~~~~~~~~
lm.fit <- lm(day_total~.-Zip-PDT-sub_total-cust_total-PrecipitationIn, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred = predict(lm.fit, newdata = SF_daily_bikeshare[-train,-31])
test.vals <- SF_daily_bikeshare$day_total[-train]
mean((test.vals - lm.pred)^2)
# MSE of 15038.15

# Creating linear model to predict sub_total only
lm.fit <- lm(sub_total~. -Zip-PDT-cust_total-day_total, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred_sub = predict(lm.fit, newdata = SF_daily_bikeshare[-train,])
test.vals <- SF_daily_bikeshare$sub_total[-train]
mean((test.vals - lm.pred_sub)^2)
# MSE of over 19242.95

# Creating linear model to predict cust_total only
lm.fit <- lm(cust_total ~.-Zip-PDT-sub_total-day_total, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred_cust = predict(lm.fit, newdata = SF_daily_bikeshare[-train,])
test.vals_cust <- SF_daily_bikeshare$cust_total[-train]
mean((test.vals_cust - lm.pred_cust)^2)
# MSE of 15617.69

# ~~~~~~~~Running K-Fold Cross-Validation~~~~~~~~~~~~
library(boot)
glm.fit <- glm(day_total ~.-Zip-PDT-sub_total-cust_total, data=SF_daily_bikeshare)
cv_error <- cv.glm(SF_daily_bikeshare, glm.fit, K= 10)
cv_error$delta[1]
# MSE of 13280.61

glm.fit <- glm(day_total ~.-cust_total-Zip-PDT-sub_total-day_total, data=SF_daily_bikeshare)
cv_error <- cv.glm(SF_daily_bikeshare, glm.fit, K= 10)
cv_error$delta[1]
# MSE of 14194.25

glm.fit <- glm(cust_total ~.-Zip-PDT-sub_total-day_total, data=SF_daily_bikeshare)
cv_error <- cv.glm(SF_daily_bikeshare, glm.fit, K= 10)
cv_error$delta[1]
# MSE of 6565.271


#~~~~~~~~~~~~~~~~~~~~~~~USING REGRESSION TREES~~~~~~~~~~~~~~~~~~~~~~~
library(tree)
tree.bikes <- tree(day_total~.-Zip-PDT-sub_total-cust_total-PrecipitationIn, data = SF_daily_bikeshare, subset = train)
summary(tree.bikes)
plot(tree.bikes)
text(tree.bikes)
yhat <- predict(tree.bikes, newdata = SF_daily_bikeshare[-train,])
bikes.test <- SF_daily_bikeshare$day_total[-train]
mean((yhat - bikes.test)^2)
# MSE 11585.95

# Using cross validation to find optimal tree size
cv.bikes <- cv.tree(tree.bikes)
cv.bikes
plot(cv.bikes)
plot(cv.bikes$size ,cv.bikes$dev ,type='b')

# Optimal size is 4 
prune.bikes <- prune.tree(tree.bikes, best = 4)
plot(prune.bikes)
text(prune.bikes)
yhat.prune <- predict(prune.bikes, newdata = SF_daily_bikeshare[-train,])
mean((yhat.prune - bikes.test)^2)
# Same MSE as above since pruning prefers 4 leaves which is the same as original tree

## Using Bagging Trees
library(randomForest)
bag.bikes <- randomForest(day_total~.-Zip-PDT-sub_total-cust_total-PrecipitationIn, data = SF_daily_bikeshare ,subset =train, mtry= 29, importance =TRUE)
bag.bikes
yhat.bag <- predict(bag.bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.bag, bikes.test)
abline(0,1, col = "red")
mean((yhat.bag - bikes.test)^2)
importance(bag.bikes)
varImpPlot(bag.bikes) # High values in matrix are good.
# MSE of 4884.98

## Using Random forest
rf.bikes <- randomForest(day_total~.-Zip-PDT-sub_total-cust_total-PrecipitationIn, 
                         data = SF_daily_bikeshare, subset = train, 
                         mtry = 5, ntree = 100, importance = TRUE)
rf.bikes
yhat.rf <- predict(rf.bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.rf, bikes.test)
abline(0,1, col = "red")
mean((yhat.rf - bikes.test)^2)
# MSE of 4656.236

## Running random forest on sub_total and cur_total

# sub_total
rf.sub_bikes <- randomForest(sub_total~.-Zip-PDT-day_total-cust_total-PrecipitationIn, 
                         data = SF_daily_bikeshare, subset = train, 
                         mtry = 5, ntree = 100, importance = TRUE)
rf.sub_bikes
yhat.rf.sub <- predict(rf.sub_bikes, newdata = SF_daily_bikeshare[-train,])
sub_bikes.test <- SF_daily_bikeshare$sub_total[-train]
plot(yhat.rf, sub_bikes.test)
abline(0,1, col = "red")
mean((yhat.rf.sub - sub_bikes.test)^2)
# MSE 3825.983

rf.cust_bikes <- randomForest(cust_total~.-Zip-PDT-sub_total-day_total-PrecipitationIn, 
                         data = SF_daily_bikeshare, subset = train, 
                         mtry = 5, ntree = 100, importance = TRUE)
rf.cust_bikes
cust_bikes.test <- SF_daily_bikeshare$cust_total[-train]
yhat.rf.cust <- predict(rf.cust_bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.rf, cust_bikes.test)
abline(0,1, col = "red")
mean((yhat.rf.cust - cust_bikes.test)^2)
# MSE 832.3284

# Summing prediction of customers and subscribers to predict actual totals
mean(((yhat.rf.sub+yhat.rf.cust) - bikes.test)^2)
# MSE 4531.033

## ~~~~~~~~~~~~~~~RUNNING FEATURE SELECTION METHODS~~~~~~~~~~~~~~~~~~~~~~~~
library(leaps)

regfit.full = regsubsets(day_total~.-Zip-PDT-cust_total-sub_total,data = SF_daily_bikeshare, method = "forward", nvmax = 30)
regfit.full
regfit.summary = summary(regfit.full)
regfit.summary$which
plot(regfit.full, scale = "adjr2")
plot(regfit.summary$adjr2, col = "darkblue", xlab = "Number of predictors", ylab = "Adjusted R^2", main = "Best Subset Selection", type = 'l')
which.max(regfit.summary$adjr2)
points (18, regfit.summary$adjr2[18], col ="red",cex =2, pch =20)
# Predictors with model of highest adjr^2
# sub_dur_total + cust_dur_total + is_weekend + day + weekday + month + year
# + Max.TemperatureF + Min.TemperatureF + Mean.TemperatureF+ Max.Dew.PointF
# + Min.DewpointF + Min.Humidity + Mean.Sea.Level.PressureIn + Min.Sea.Level.PressureIn
# + Max.VisibilityMiles + Max.Wind.SpeedMPH + WindDirDegrees



# library(rattle)
# library(rpart.plot)
# library(RColorBrewer)
# 
# # Time to plot your fancy tree
# fancyRpartPlot(my_tree_two)
