#~~~~~~~~~~~~~~~~~~~Model Testing~~~~~~~~~~~~~~~~~~

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
# MSE of 13669.94

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

glm.fit <- glm(sub_total ~.-cust_total-Zip-PDT-sub_total-day_total, data=SF_daily_bikeshare)
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
# MSE 12492.15

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

# Predicting Subscriber total
tree.bikes.sub <- tree(sub_total~.-Zip-PDT-day_total-sub_total-cust_total-
                  PrecipitationIn, data = SF_daily_bikeshare, 
                   subset = train)
summary(tree.bikes.sub)
plot(tree.bikes.sub)
text(tree.bikes.sub)
yhat.sub <- predict(tree.bikes.sub, newdata = SF_daily_bikeshare[-train,])
bikes.test.sub <- SF_daily_bikeshare$sub_total[-train]
mean((yhat.sub - bikes.test.sub)^2)
# MSE 9667.074

# Predicting customer total
tree.bikes.cust <- tree(cust_total~.-Zip-PDT-day_total-sub_total-cust_total-
                      PrecipitationIn, data = SF_daily_bikeshare, 
                       subset = train)
summary(tree.bikes.cust)
plot(tree.bikes.cust)
text(tree.bikes.cust)
yhat.cust <- predict(tree.bikes.cust, newdata = SF_daily_bikeshare[-train,])
bikes.test.cust <- SF_daily_bikeshare$cust_total[-train]
mean((yhat.cust - bikes.test.cust)^2)
# MSE 1555.12

# Summing prediction of customers and subscribers to predict actual totals
mean(((yhat.sub + yhat.cust) - bikes.test)^2)
#MSE 7247.974

## Using Bagging Trees
library(randomForest)
bag.bikes <- randomForest(day_total~.-Zip-PDT-day_total-sub_total-cust_total-PrecipitationIn, 
                          data = SF_daily_bikeshare ,subset =train, mtry= 29, importance =TRUE)
bag.bikes
yhat.bag <- predict(bag.bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.bag, bikes.test)
abline(0,1, col = "red")
mean((yhat.bag - bikes.test)^2)
importance(bag.bikes)
varImpPlot(bag.bikes) # High values in matrix are good.
# MSE of 5127.362

# Prediction Subscriber total
bag.bikes.sub <- randomForest(sub_total~.-Zip-PDT-day_total-sub_total-cust_total,
                data = SF_daily_bikeshare ,subset =train, mtry= 29, importance =TRUE)
bag.bikes.sub
yhat.bag.sub <- predict(bag.bikes.sub, newdata = SF_daily_bikeshare[-train,])
plot(yhat.bag.sub, bikes.test.sub)
abline(0,1, col = "red")
mean((yhat.bag.sub - bikes.test.sub)^2)
# MSE of 4250.126

# Predicting Customer total
bag.bikes.cust <- randomForest(cust_total~.-Zip-PDT-day_total-sub_total-cust_total-PrecipitationIn,
                              data = SF_daily_bikeshare ,subset =train, mtry= 29, importance =TRUE)
bag.bikes.cust
yhat.bag.cust <- predict(bag.bikes.cust, newdata = SF_daily_bikeshare[-train,])
plot(yhat.bag.cust, bikes.test.cust)
abline(0,1, col = "red")
mean((yhat.bag.cust - bikes.test.cust)^2)
# MSE of 1215.458

# Summing prediction of customers and subscribers to predict actual totals
mean(((yhat.bag.sub+yhat.bag.cust) - bikes.test)^2)
# MSE 5335.834

## ~~~~ Using Random forest ~~~~
rf.bikes <- randomForest(day_total~.-Zip-PDT-sub_total-cust_total-PrecipitationIn, 
                         data = SF_daily_bikeshare, subset = train, 
                         mtry = 5, ntree = 100, importance = TRUE)
rf.bikes
yhat.rf <- predict(rf.bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.rf, bikes.test)
abline(0,1, col = "red")
mean((yhat.rf - bikes.test)^2)
# MSE of 5604.87

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
# MSE 4874.54

rf.cust_bikes <- randomForest(cust_total~.-Zip-PDT-sub_total-day_total-PrecipitationIn, 
                         data = SF_daily_bikeshare, subset = train, 
                         mtry = 5, ntree = 100, importance = TRUE)
rf.cust_bikes
cust_bikes.test <- SF_daily_bikeshare$cust_total[-train]
yhat.rf.cust <- predict(rf.cust_bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.rf, cust_bikes.test)
abline(0,1, col = "red")
mean((yhat.rf.cust - cust_bikes.test)^2)
# MSE 1014.82

mean(((yhat.rf.sub + yhat.rf.cust) - bikes.test)^2)
# MSE 4468.12

# Summing prediction of customers and subscribers to predict actual totals
mean(((yhat.rf.sub+yhat.rf.cust) - bikes.test)^2)
# MSE 5579.339

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





# ~~~~~ ~~~~~~~~~~~~~~~~~~~~~~ Round 3 of Model Testing ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Incorporating new features

# Removing PrecipitationIn column because it is being outputted like a matrix
# and ruining the results
# SF_daily_bikeshare <- SF_daily_bikeshare[,-31]
set.seed(101)
train <- sample(1:1081, 700)

#Checking for null hypothesis
lm.fit = lm(day_total~Avg_Gas_Value + Giants_Game + SF_Event, data = SF_daily_bikeshare )
summary(lm.fit)
# The p-value is low for all three variables so we can reject the null hypothesis

# ~~~~~~~~ Analyzing feature selection ~~~~~~~~~~~

library(leaps)

regfit.full = regsubsets(day_total~.-day_total-PDT-cust_total-sub_total,data = SF_daily_bikeshare[train,], method = "forward", nvmax = 8)
regfit.full
regfit.summary = summary(regfit.full)
regfit.summary$which

par(mfrow = c(1,2))
plot(regfit.full, scale = "adjr2")
plot(regfit.summary$adjr2, col = "darkblue", xlab = "Number of predictors", ylab = "Adjusted R^2", main = "Best Subset Selection", type = 'l')
which.max(regfit.summary$adjr2)
points (11, regfit.summary$adjr2[11], col ="red",cex =2, pch =20)

total_vars <- names(coef(regfit.forward, id = 9))
total_vars <- total_vars[-1]

total_vars

# feature selection for predicting subscriber total
regfit.sub = regsubsets(sub_total~.-day_total-PDT-cust_total-sub_total,data = SF_daily_bikeshare[train,], method = "forward", nvmax = 30)
regfit.full
regfit.summary = summary(regfit.full)
regfit.summary$which

par(mfrow = c(1,2))
plot(regfit.full, scale = "adjr2")
plot(regfit.summary$adjr2, col = "darkblue", xlab = "Number of predictors", ylab = "Adjusted R^2", main = "Best Subset Selection", type = 'l')
which.max(regfit.summary$adjr2)
points (13, regfit.summary$adjr2[13], col ="red",cex =2, pch =20)

sub_vars <- names(coef(regfit.forward, id = 13))
sub_vars <- sub_vars[-1]

sub_vars

# Feature selection for predicting customer total
regfit.cust = regsubsets(cust_total~.-day_total-PDT-cust_total-sub_total,data = SF_daily_bikeshare[train,], method = "forward", nvmax = 15)
regfit.cust
regfit.summary = summary(regfit.cust)
regfit.summary$which

par(mfrow = c(1,2))
plot(regfit.cust, scale = "adjr2")
plot(regfit.summary$adjr2, col = "darkblue", xlab = "Number of predictors", ylab = "Adjusted R^2", main = "Best Subset Selection", type = 'l')
which.max(regfit.summary$adjr2)
points (16, regfit.summary$adjr2[13], col ="red",cex =2, pch =20)

cust_vars <- names(coef(regfit.cust, id = 16))
cust_vars <- cust_vars[-1]

cust_vars

# Forward subset selection models do not reduce MSE but rather increase it


# ~~~~~~~~~~~~~~~~~~~~ Running linear regression ~~~~~~~~~~~~~~~~~~~~~

lm.fit <- lm(day_total~.-day_total-PDT-sub_total-cust_total, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred = predict(lm.fit, newdata = SF_daily_bikeshare[-train,])
test.vals <- SF_daily_bikeshare$day_total[-train]
mean((test.vals - lm.pred)^2)
# MSE of 14740.47

# Creating linear model to predict sub_total only
lm.fit <- lm(sub_total~. -sub_total-day_total-PDT-cust_total-day_total, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred_sub = predict(lm.fit, newdata = SF_daily_bikeshare[-train,])
test.vals <- SF_daily_bikeshare$sub_total[-train]
mean((test.vals - lm.pred_sub)^2)
# MSE of over 14332.68

# Creating linear model to predict cust_total only
lm.fit <- lm(cust_total ~.-cust_total-PDT-sub_total-day_total, data = SF_daily_bikeshare[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()
lm.pred_cust = predict(lm.fit, newdata = SF_daily_bikeshare[-train,])
test.vals_cust <- SF_daily_bikeshare$cust_total[-train]
mean((test.vals_cust - lm.pred_cust)^2)
# MSE of 15035.47

# ~~~~~~~~Running K-Fold Cross-Validation~~~~~~~~~~~~
library(boot)

glm.fit <- glm(day_total ~.-day_total-PDT-sub_total-cust_total, data=SF_daily_bikeshare)
cv_error <- cv.glm(SF_daily_bikeshare, glm.fit, K= 10)
cv_error$delta[1]
# MSE of 12623.71


glm.fit <- glm(sub_total ~.-cust_total-PDT-sub_total-day_total, data=SF_daily_bikeshare)
cv_error <- cv.glm(SF_daily_bikeshare, glm.fit, K= 10)
cv_error$delta[1]
# MSE of 12533.24

glm.fit <- glm(cust_total ~.-PDT-sub_total-day_total, data=SF_daily_bikeshare)
cv_error <- cv.glm(SF_daily_bikeshare, glm.fit, K= 10)
cv_error$delta[1]
# MSE of 1092.988

#~~~~~~~~~ Regression Trees ~~~~~~~~~~
library(tree)
tree.bikes <- tree(day_total~.-PDT-sub_total-cust_total-day_total, data = SF_daily_bikeshare, subset = train)
summary(tree.bikes)
plot(tree.bikes)
text(tree.bikes)
yhat <- predict(tree.bikes, newdata = SF_daily_bikeshare[-train,])
bikes.test <- SF_daily_bikeshare$day_total[-train]
mean((yhat - bikes.test)^2)
# MSE 12492.1

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

tree.bikes.sub <- tree(sub_total~.-PDT-day_total-sub_total-cust_total, data = SF_daily_bikeshare, 
                       subset = train)
summary(tree.bikes.sub)
plot(tree.bikes.sub)
text(tree.bikes.sub)
yhat.sub <- predict(tree.bikes.sub, newdata = SF_daily_bikeshare[-train,])
bikes.test.sub <- SF_daily_bikeshare$sub_total[-train]
mean((yhat.sub - bikes.test.sub)^2)
# MSE 9667.074

# Predicting customer total
tree.bikes.cust <- tree(cust_total~.-PDT-day_total-sub_total-cust_total, 
                        data = SF_daily_bikeshare, subset = train)
summary(tree.bikes.cust)
plot(tree.bikes.cust)
text(tree.bikes.cust)
yhat.cust <- predict(tree.bikes.cust, newdata = SF_daily_bikeshare[-train,])
bikes.test.cust <- SF_daily_bikeshare$cust_total[-train]
mean((yhat.cust - bikes.test.cust)^2)
# MSE 1555.12

# Summing prediction of customers and subscribers to predict actual totals
mean((yhat.cust + yhat.sub - bikes.test)^2)
# MSE 11490.53

# ~~~~~~~~ Tree Ensembles ~~~~~~~~~

set.seed(101)
train <- sample(1:1081, 700)
bikes.test <- SF_daily_bikeshare$day_total[-train]


library(randomForest)

# ~~~ USING BAGGING ~~~
bag.bikes <- randomForest(day_total~.-day_total-PDT-sub_total-cust_total, data = SF_daily_bikeshare ,subset =train, mtry= 29, importance =TRUE)
bag.bikes
yhat.bag <- predict(bag.bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.bag, bikes.test)
abline(0,1, col = "red")
mean((yhat.bag - bikes.test)^2)
# MSE of 4896.944

# Prediction Subscriber total
bag.bikes.sub <- randomForest(sub_total~.-PDT-day_total-sub_total-cust_total,
                              data = SF_daily_bikeshare ,subset =train, mtry= 29, importance =TRUE)
bag.bikes.sub
yhat.bag.sub <- predict(bag.bikes.sub, newdata = SF_daily_bikeshare[-train,])
plot(yhat.bag.sub, bikes.test.sub)
abline(0,1, col = "red")
mean((yhat.bag.sub - bikes.test.sub)^2)
# MSE of 3993.661

# Predicting Customer total
bag.bikes.cust <- randomForest(cust_total~.-PDT-day_total-sub_total-cust_total,
                               data = SF_daily_bikeshare ,subset =train, mtry= 29, importance =TRUE)
#bag.bikes.cust
yhat.bag.cust <- predict(bag.bikes.cust, newdata = SF_daily_bikeshare[-train,])
plot(yhat.bag.cust, bikes.test.cust)
abline(0,1, col = "red")
mean((yhat.bag.cust - bikes.test.cust)^2)
# MSE of 1059.445

# Summing prediction of customers and subscribers to predict actual totals
mean(((yhat.bag.cust + yhat.bag.sub) - bikes.test)^2)
# MSE 5083.283

## ~~~~~ Using Random forest ~~~~~
rf.bikes <- randomForest(day_total~.-day_total-PDT-sub_total-cust_total, 
                         data = SF_daily_bikeshare, subset = train, 
                         mtry = 5, ntree = 100, importance = TRUE)
rf.bikes
yhat.rf <- predict(rf.bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.rf, bikes.test)
abline(0,1, col = "red")
mean((yhat.rf - bikes.test)^2)
# MSE of 5628.969

## Running random forest on sub_total and cur_total

# sub_total
rf.sub_bikes <- randomForest(sub_total~.-sub_total-PDT-day_total-cust_total, 
                             data = SF_daily_bikeshare, subset = train, 
                             mtry = 5, ntree = 100, importance = TRUE)
rf.sub_bikes
yhat.rf.sub <- predict(rf.sub_bikes, newdata = SF_daily_bikeshare[-train,])
sub_bikes.test <- SF_daily_bikeshare$sub_total[-train]
plot(yhat.rf.sub, sub_bikes.test)
abline(0,1, col = "red")
mean((yhat.rf.sub - sub_bikes.test)^2)
# MSE 4930.166

rf.cust_bikes <- randomForest(cust_total~.-cust_total-PDT-sub_total-day_total, 
                              data = SF_daily_bikeshare, subset = train, 
                              mtry = 5, ntree = 100, importance = TRUE)
rf.cust_bikes
cust_bikes.test <- SF_daily_bikeshare$cust_total[-train]
yhat.rf.cust <- predict(rf.cust_bikes, newdata = SF_daily_bikeshare[-train,])
plot(yhat.rf.cust, cust_bikes.test)
abline(0,1, col = "red")
mean((yhat.rf.cust - cust_bikes.test)^2)
# MSE 895.4241

# Summing prediction of customers and subscribers to predict actual totals
mean(((yhat.rf.sub+yhat.rf.cust) - bikes.test)^2)
# MSE 5434.24



# ~~~~~~~~~~~~~~~~~ Plotting results ~~~~~~~~~~~~~~~~~~

# Plotting Linear Regression MSE
y1 <- c(34378.4, 15038, 14740.47)
y2 <- c(30000,19242.95, 14332.68)
y3 <- c(13669.94, 15617.69, 1123.544)
par(mfrow = c(1,3), oma = c(0,0,2,0))
plot(1:3, y1, xaxt = "n",  xlab = "Model Fitting Round", ylab = "MSE",
     main = "Prediction Accuracy of Total Rentals", type = "l")
axis(1, at = 1:3, labels = c("Round 1", "Round 2", "Round 3"))

plot(1:3, y2, xaxt = "n",  xlab = "Model Fitting Round", ylab = "MSE", 
     main = "Prediction Accuracy of Subscriber Rentals", type = "l")
axis(1, at = 1:3, labels = c("Round 1", "Round 2", "Round 3"))

plot(1:3, y3, xaxt = "n", xlab = "Model Fitting Round", ylab = "MSE", 
     main = "Prediction Accuracy of Customer Rentals", type = "l")
axis(1, at = 1:3, labels = c("Round 1", "Round 2", "Round 3"))

mtext("Linear Regression Prediction Accuracy Over Three Rounds of Fitting", outer = TRUE, cex = 1)

# Plotting Cross Validation MSE
y1 <- c(13280.61, 12623.71)
y2 <- c(14194.25, 12533.24)
y3 <- c(6565.271, 1092.988)


par(mfrow = c(1,3), oma = c(0,0,2,0))
plot(2:3, y1, xaxt = "n", xlab = "Model Fitting Round", ylab = "MSE",
     main = "Prediction Accuracy of Total Rentals Per Day", type = "l")
axis(1, at = 2:3, labels = c("W/O new features", "W/New Features"))

plot(2:3, y2,xaxt = "n",  xlab = "Model Fitting Round", ylab = "MSE", 
     main = "Prediction Accuracy of Subscriber Rentals Per Day", type = "l")
axis(1, at = 2:3, labels = c("W/O new features", "W/New Features"))

plot(2:3, y3,xaxt = "n",  xlab = "Model Fitting Round", ylab = "MSE", 
     main = "Prediction Accuracy of Customer Rentals Per Day", type = "l")
axis(1, at = 2:3, labels = c("W/O new features", "W/New Features"))

mtext("K-Fold CV Prediction Accuracy Over Three Rounds of Fitting", outer = TRUE, cex = 1)


# MSE Values for each tree model
# Indexes are Single Tree, Bagging and Random Forest values respectively
# Everything up to y3a are from the second round of tree modeling
y1 <- c(12492.150, 5127.362, 5604.872) # MSE of day_total for each Tree model
y2 <- c(9667.074, 4250.126, 4874.540)# MSE of sub_total
y3 <- c(1555.1200, 1215.458, 1014.820)# MSE of cust_total
y3a <- c(11490.530, 5335.834, 5579.339)# MSE of sub_total + cust_total

# Third round of tree modeling
y4 <- c(12492.150, 4896.944, 5628.969)# MSE of day_total for each Tree model
y5 <- c(9667.074, 3993.661, 4930.166)# MSE of sub_total
y6 <- c(1555.1200, 1059.445, 895.4241)# MSE of cust_total
y6a <- c(11490.530,  4399.548, 5434.240)# MSE of sub_total + cust_total

par(mfrow = c(2,2), oma = c(0,0,2,0))
# Plotting sub_total MSE
plot(1:3, y2, xaxt = "n", xlab='Types of Modeling', ylab = "MSE", ylim = c(3000, 9000), type = "l",
     main = "MSE for Predicting sub_total", col = "red")
axis(1, at=1:3, labels=models)
lines(1:3, y5, col = "blue")
legend(2.1, 8000, c("w/o New Features", "w/ New Features"), col = c("red", "blue"), pch = "-",
       lwd = 1, cex = .95)

# Plotting cust_total MSE
plot(1:3, y3, xaxt = "n", xlab='Types of Modeling', ylab = "MSE", ylim = c(500, 2000), type = "l",
     main = "MSE for Predicting cust_total", col = "red")
axis(1, at=1:3, labels=models)
lines(1:3, y6, col = "blue")

# Plotting day_total MSE
models <- c("Single Tree", "Bagging", "Random Forest")

plot(1:3, y1, xaxt = "n", xlab='Types of Modeling', ylab = "MSE", ylim = c(4000, 12000), type = "l",
     main = "MSE for Predicting day_total", col = "red")
axis(1, at=1:3, labels=models)
lines(1:3, y4, col = "blue")

# Plotting sub_total + cust_total MSE
plot(1:3, y3a, xaxt = "n", xlab='Types of Modeling', ylab = "MSE", ylim = c(4000, 12000), type = "l",
     main = "MSE for day_total from of sub_total and cust_total", col = "red")
axis(1, at=1:3, labels=models)
lines(1:3, y6a, col = "blue")

mtext("Change in Tree Ensemble MSE Based on the Addition of New Features", outer = TRUE, cex = 1.25)
