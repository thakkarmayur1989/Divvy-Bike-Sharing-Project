

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}




# Import Train and Test Data Set:
train <- read.csv("train .csv")
test <- read.csv("test.csv")
str(test)
#bike=read.csv("train .csv")

#Combine test and train data set

test$registered=0
test$casual=0
test$count=0
Alldata <- rbind(train, test)

#Structure of the dataset
str(Alldata)

#Missing Values in the Dataset

table(is.na(Alldata))
colSums(is.na(Alldata))

#plot a histogram for each numerical variables and analyze the distribution.


library(ggplot2)
qplot(Alldata$season, geom="histogram") 
qplot(Alldata$season,geom="histogram",binwidth=.25, main = "Histogram of Season", xlab = "Season", fill=I("Steelblue"),alpha=I(.6))

histseason=ggplot(data=Alldata,aes(Alldata$season))+geom_histogram(aes(fill=..count..))+scale_fill_gradient("Count", low="Steelblue",high ="Red")+labs(title="Histogram for Seasons") +labs(x="Season", y="Count")

histweather=ggplot(data=Alldata,aes(Alldata$weather))+geom_histogram(aes(fill=..count..))+scale_fill_gradient("Count", low="Steelblue",high = "Red")+labs(title="Histogram for weather") +labs(x="weather", y="Count")

histhumidity=ggplot(data=Alldata,aes(Alldata$humidity))+geom_histogram(aes(fill=..count..),breaks=seq(10,100))+scale_fill_gradient("Count", low="Steelblue",high = "Red")+labs(title="Histogram for humidity") +labs(x="humidity", y="Count")

histHD=ggplot(data=Alldata,aes(Alldata$holiday))+geom_histogram(aes(fill=..count..))+scale_fill_gradient("Count", low="Steelblue",high = "Red")+labs(title="Histogram for holiday") +labs(x="holiday", y="Count")

histWD=ggplot(data=Alldata,aes(Alldata$workingday))+geom_histogram(aes(fill=..count..))+scale_fill_gradient("Count", low="Steelblue",high = "Red")+labs(title="Histogram for workingday") +labs(x="workingday", y="Count")

histTemp=ggplot(data=Alldata,aes(Alldata$temp))+geom_histogram(aes(fill=..count..))+scale_fill_gradient("Count", low="Steelblue",high = "Red")+labs(title="Histogram for temp") +labs(x="temp", y="Count")

histAtemp=ggplot(data=Alldata,aes(Alldata$atemp))+geom_histogram(aes(fill=..count..))+scale_fill_gradient("Count", low="Steelblue",high = "Red")+labs(title="Histogram for feels like temp") +labs(x="Feel Like temp", y="Count")

histWS=ggplot(data=Alldata,aes(Alldata$windspeed))+geom_histogram(aes(fill=..count..),breaks=seq(5,50))+scale_fill_gradient("Count", low="Steelblue",high = "Red")+labs(title="Histogram for windspeed") +labs(x="windspeed", y="Count")


multiplot(histseason,histweather)
multiplot(histHD,histWD)
multiplot(histTemp,histAtemp)

multiplot(histhumidity,histWS)
#Convert discrete variables into factor (season, weather, holiday, workingday)
Alldata$season=as.factor(Alldata$season)
Alldata$weather=as.factor(Alldata$weather)
Alldata$holiday=as.factor(Alldata$holiday)
Alldata$workingday=as.factor(Alldata$workingday)

#Hourly Trend
#There is no variable 'hour', we only have 'datetime'. 
#So we can extract it using the datetime column

Alldata$hour=substr(Alldata$datetime,12,13)
Alldata$hour=as.factor(Alldata$hour)

train=Alldata[as.integer(substr(Alldata$datetime,9,10))<20,]
test=Alldata[as.integer(substr(Alldata$datetime,9,10))>19,]

layout(matrix(c(1,2,3),1,3,byrow=FALSE))
boxplot(train$count~train$hour,xlab="hour",ylab="OVERALL COUNT(Users)",main="OVERALL COUNT(Users)",col =c("red","sienna","royalblue2", "palevioletred1"))
boxplot(train$casual~train$hour, xlab="hour",ylab="CASUAL(Users)",main="CASUAL(Users)",col=c("palegreen2","plum3", "sienna2","snow3"))
boxplot(train$registered~train$hour,xlab="hour", ylab="REGISTERED(Users)",main="REGISTERED(Users)",col=c("red","sienna","royalblue2", "palevioletred1"))

#Daywise Trend:
#Prediction is that the Registered users might demand more bike on weekdays as compared to weekend or holiday
date=substr(Alldata$datetime,1,10)
days<-weekdays(as.Date(date))
Alldata$day=days

layout(matrix(c(1,2,3),1,3,byrow=FALSE))
boxplot(Alldata$count~Alldata$day,xlab="day",ylab="OVERALL COUNT(Users)",main="OVERALL COUNT(Users)",col =c("red","sienna","royalblue2", "palevioletred1"))
boxplot(Alldata$casual~Alldata$day, xlab="day",ylab="CASUAL(Users)",main="CASUAL(Users)",col=c("palegreen2","plum3", "sienna2","snow3"))
boxplot(Alldata$registered~Alldata$day,xlab="day", ylab="REGISTERED(Users)",main="REGISTERED(Users)",col=c("red","sienna","royalblue2", "palevioletred1"))

#Tempwise
layout(matrix(c(1,2,3),1,3,byrow=FALSE))
boxplot(Alldata$count~Alldata$temp,xlab="temp",ylab="OVERALL COUNT(Users)",main="OVERALL COUNT(Users)",col =c("red","sienna","royalblue2", "palevioletred1"))
boxplot(Alldata$registered~Alldata$temp,xlab="temp", ylab="CASUAL(Users)",main="CASUAL(Users)",col=c("palegreen2","plum3", "sienna2","snow3"))
boxplot(Alldata$casual~Alldata$temp,xlab="temp", ylab="REGISTERED(Users)",main="REGISTERED(Users)",col=c("red","sienna","royalblue2", "palevioletred1"))

#Weather
layout(matrix(c(1,2,3),1,3,byrow=FALSE))
boxplot(Alldata$count~Alldata$weather,xlab="weather",ylab="OVERALL COUNT(Users)",main="OVERALL COUNT(Users)",col =c("red","sienna","royalblue2", "palevioletred1"))
boxplot(Alldata$registered~Alldata$weather,xlab="weather", ylab="CASUAL(Users)",main="CASUAL(Users)",col=c("palegreen2","plum3", "sienna2","snow3"))
boxplot(Alldata$casual~Alldata$weather,xlab="weather", ylab="REGISTERED(Users)",main="REGISTERED(Users)",col=c("red","sienna","royalblue2", "palevioletred1"))

# splitting the train and test data from the Combineddata which we used for exploration
train.data <- Alldata[1:10886,]
test.data <- Alldata[10887:17379,]

library(ggplot2)
#relationship between the count of bikes rented and the temperature 
ggplot(train.data, aes(temp, count)) + geom_point(aes(color=temp), alpha = 0.5) + scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Temperature vs Count")


#we need to convert the timestamp to POSIX 
train.data$datetime <- as.POSIXct(train.data$datetime)
#ggplot between date and bike count

ggplot(train.data, aes(datetime, count)) + geom_point(aes(color = temp))+ scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Date vs Count")


#bike count on working and non working day
bike.wd <- subset(train.data, workingday == 1)
ggplot(bike.wd, aes(hour, count)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Working Days")


bike.nwd <- subset(train.data, workingday == 0)
ggplot(bike.nwd, aes(hour, count)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Non Working Days")

#bike count on holiday and non holiday
bike.hd=subset(train.data,holiday==1)
ggplot(bike.hd, aes(hour, count)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Holiday")

bike.nhd=subset(train.data,holiday==0)
ggplot(bike.nhd, aes(hour, count)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Non Holiday")


# Converting integer to factor on training set
train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
train$datetime <-as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")


# Extract day from datetime value
train$day <-  strftime(train$datetime, '%u')
train$day <- as.factor(train$day)

# Extract hour from datetime value
train$hour <- substring(train$datetime, 12,13)
train$hour <- as.factor(train$hour)

# Removing datetime field 
train <- train[,-1]
install.packages("sqldf")
library(sqldf)

# Get the average count of bikes rent by season, hour
season_summary_by_hour <- sqldf('select season, hour, avg(count) as count from train group by season, hour')

# From this plot it shows, 
# There are more rental in morning(from 7-9th hour) and evening(16-19th hour)
# People rent bikes more in Fall, and much less in Spring
ggplot(train, aes(x=hour, y=count, color=season))+geom_point(data = season_summary_by_hour, aes(group = season))+geom_line(data = season_summary_by_hour, aes(group = season))+ggtitle("Bikes Rent By Season")+ scale_colour_hue('Season',breaks = levels(train$season), labels=c('spring', 'summer', 'fall', 'winter'))

# Get the average count of bikes rent by weather, hour
weather_summary_by_hour <- sqldf('select weather, hour, avg(count) as count from train group by weather, hour')

# From this plot it shows, 
# People rent bikes more when weather is good
# We see bike rent only at 18th hour when weather is very bad
ggplot(train, aes(x=hour, y=count, color=weather))+geom_point(data = weather_summary_by_hour, aes(group = weather))+geom_line(data = weather_summary_by_hour, aes(group = weather))+ggtitle("Bikes Rent By Weather")+ scale_colour_hue('Weather',breaks = levels(train$weather), labels=c('Good', 'Normal', 'Bad', 'Very Bad'))


# Get the average count of bikes rent by day, hour
day_summary_by_hour <- sqldf('select day, hour, avg(count) as count from train group by day, hour')

# From this plot it shows, 
# There are more bikes rent on weekdays during morining and evening
# There are more bikes rent on weekends during daytime
ggplot(train, aes(x=hour, y=count, color=day))+geom_point(data = day_summary_by_hour, aes(group = day))+geom_line(data = day_summary_by_hour, aes(group = day))+ggtitle("Bikes Rent By Weekday")+ scale_colour_hue('Weekday',breaks = levels(train$day), labels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))



#--------------Explorarity Analysis done-------

#Linear Regression. 

# Cleaning the old data and reloading again

rm(list=ls())
library(readxl)
train <- read_excel("train .xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(train)

train=train[,-c(10,11)]


# Converting integer to factor on training set
train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
#train$datetime <-as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")


# Extract day from datetime value
train$day <-  strftime(train$datetime, '%u')
train$day <- as.factor(train$day)

# Extract hour from datetime value
train$hour <- substring(train$datetime, 12,13)
train$hour <- as.factor(train$hour)

# Removing datetime field 
train <- train[,-1]

# Splitting the Train dataset
library(caTools)
set.seed(123)
split <- sample.split(train$count, SplitRatio = 0.80)
training_set <- subset(train, split == TRUE)
validation_set <- subset(train, split == FALSE)

# Applying Linear Regression model
#Model1
str(train)
BikeModel <- lm(count~., data = training_set)
summary(BikeModel)


par(mfrow = c(2, 2))
plot(BikeModel)

# Apply prediction on validation set
BikeModelPrediction <- predict(BikeModel, newdata = validation_set)


#Lets compute the root-mean-square error value between actual and predicted
install.packages("Metrics")
library(Metrics)
BikeModel_RMSE<-rmse(validation_set$count,BikeModelPrediction)
print("RMSE between actual and predicted")
BikeModel_RMSE

# Now perform stepwise model selection by AIC with both directions(Forward, Backward)
library(MASS)
#Model2
BikeRentAIC<-stepAIC(BikeModel, direction="both")
summary(BikeRentAIC)
par(mfrow = c(2, 2))
plot(BikeModel)

# Apply prediction on validation set
BikeRentAIC_validation <- predict(BikeRentAIC, newdata = validation_set)


#Lets compute the root-mean-square error value between actual and predicted
#install.packages("Metrics")
library(Metrics)
BikeRentAIC_rmse<-rmse(validation_set$count,BikeRentAIC_validation)
print("RMSE Value of Model2")
print(BikeRentAIC_rmse)



# Since we got high RMSE values, let's do log transformation and run regression model again
#Model3
BikeRentLogModel <- lm(log(count)~., data = training_set)

# Now perform stepwise model selection on log model
BikeRentLogModelAIC <- stepAIC(BikeRentLogModel, direction="both")

lm_predict_validation_log <- predict(BikeRentLogModelAIC,newdata=validation_set)

# As the predicted values are in log format, use exponential(exp) to convert from log to non-log values
lm_predict_validation_nonlog <- exp(lm_predict_validation_log)


# Let's check the summary of predicted count values, it shows there are no negative values
print("summary of predicted count values after log transformation")
summary(lm_predict_validation_nonlog)

# Check rmse value again, it got reduced from 0.9549201 to 0.6278527
validaion_nonlog_rmsle<-rmsle(validation_set$count,lm_predict_validation_nonlog)
print("RMSE value after log transformation")
print(validaion_nonlog_rmsle)

# Let's check the Residual vs Fitted plot
# It shows some points forms a straight lines
# If you select bottom straight line points using "identify", you will find that the bike rent count is 1
# and next straight line points will have bike rent count of 2. 
plot(BikeRentLogModel$fitted.values, BikeRentLogModel$residuals)

plot(BikeRentLogModelAIC)

#testing on validation
# Run model on test data
lm_predict_test_log <- predict(BikeRentLogModelAIC,newdata=validation_set)

# As the predicted values are in log format, use exponential(exp) to convert from log to non-log values
lm_predict_test_nonlog <- exp(lm_predict_test_log)

Predictedvalue <- cbind(as.data.frame(lm_predict_test_nonlog), validation_set$count)
colnames(Predictedvalue) <- c("Pred count", "Actual count")
Predictedvalue
# model3 has least RMSE value, hence we say that this is best for predication among the models

#####*************************Time Series***************************#########

#timeSeries


rm(list=ls())

library(tseries)
library(fBasics)
library(zoo)
library(tidyverse)
library(forecast)
library(lubridate)

# Read data

bikeData <- read.csv("train .csv")  %>% mutate(datetime = date(datetime))



# Separate data

train <- bikeData %>% select(datetime, count) %>%
        
        filter(datetime < as.Date("2013-01-01")) %>%
        
        group_by(datetime) %>%
        
        summarise(y = sum(count))

names(train) <- c('Date', 'Count')

head(train)

valid <- bikeData %>% select(datetime, count) %>%
        
        filter(datetime >= as.Date("2012-01-01") & datetime < as.Date("2012-07-01")) %>%
        
        group_by(datetime) %>%
        
        summarise(y = sum(count))

names(valid) <- c('ds', 'y')





#library(forecast)

bikeTS=ts(train$Count,start =c(2011,01,01),frequency=365)

plot(bikeTS)

#testing for ts
plot((diff(bikeTS)))
plot(diff(diff(bikeTS)))
plot(log(bikeTS))
plot(log(diff(bikeTS)))
plot(sqrt(bikeTS))
plot(sqrt(diff(bikeTS)))

ts1=diff(bikeTS)
plot(ts1,xlab="Time",ylab="Count")


#basicstat

basicStats(ts1)

#ACF plot - p value in (p,d,q)
acf(coredata(ts1), lag=15)

# hypothesis testing to find serial correlation
Box.test(ts1,lag=12,type = 'Ljung')
Box.test(ts1,lag=6,type = 'Ljung')



hist(ts1, xlab="Bike Count trend", prob=TRUE, main="Histogram")
xfit<-seq(min(ts1),max(ts1),length=40)
yfit<-dnorm(xfit,mean=mean(ts1),sd=sd(ts1))
lines(xfit, yfit, col="blue", lwd=4)

#checking for normal distibution
#QQ Plot:
qqnorm(ts1)
qqline(ts1, col=2)

# Command to plot the time series
plot(ts1)

# Perform Jarque-Beratest (for larger observations)
jarque.bera.test(ts1)

normalTest(ts1,method = c("jb"))


#ACF plot - p value in (p,d,q)
acfvalue=acf(coredata(ts1),plot = F, lag=15)
acfvalue

#prints acfto console


acf(ts1, plot=F,lag=15)

#plots acf(correlogram)
acf(coredata(ts1), PLOT=T,lag=15)

#plots pacf
pacf(coredata(ts1), lag=15)


# to Lag 6
Box.test(ts1,lag=6,type='Ljung')
# to Lag 12
Box.test(ts1,lag=12,type='Ljung')




#Building model
#AR model
m1=arima(diff(bikeTS),order=c(9,0,0))
m1

tsdiag(m1)
names(m1)
plot(m1$residuals,type='l')

qqnorm(m1$residuals)
qqline(m1$residuals,col=2)

#Check whether residual is white noise or not
# by ACF/PACF plot
acf(m1$residual, plot=T, lag=20)
pacf(m1$residual, plot=T, lag=20)

#Model fitting by automatically identify the value of the p
AutoARModel=ar(ts1)
AutoARModel


#Check whether residual is white noise or not
# by LjungBox test
# Test if residuals are white noise using the Ljung-Box test for m=6, 12, and 18
Box.test(m1$resid, lag=6, type='Ljung')
Box.test(m1$resid, lag=12, type='Ljung')
Box.test(m1$resid, lag=18, type='Ljung')



#MA model
m2=arima(ts1,order=c(0,0,1))
m2
tsdiag(m2)

autoMA=arma(ts1,order = c(0,1))
summary(autoMA)
plot(autoMA)

#ARIMA
m3=auto.arima(diff(bikeTS), max.p = 20,max.q = 20 ,stationary = TRUE,ic = c("aic"), stepwise = TRUE)
m3

m4=auto.arima(bikeTS,d=1,approximation =FALSE, trace=FALSE)
# Command to predict the model
p1=predict(m1, n.ahead=5, se.fit = T)
p1

p2=predict(m2, n.ahead=50, se.fit = T)
p2
fr=forecast(p2$pred)
plot(fr)

f1=forecast(m3)
plot(f1)

#COMPUTE PREDICTIONS
x.fore= predict(m4, n.ahead=50)
x.fore
#We need more data to evalutate the timeseries model. 
