#################################### BIKE RENTAL PROJECT ########################################

#Clean the environment
rm(list = ls())

                                    # LOAD LIBRARIES AND IMPORT DATASET
#Set working directory
setwd("C:/Users/shrid/Downloads/edWisor")


#Load libraries 
library("readr")
library("dplyr")
library("plyr")
library("corrplot")
library("ggplot2")
library("randomForest")
library("ggExtra")
library("ggpubr")
library("corrgram")
library("rpart")
library("DMwR")
library("Metrics")
library("rpart")
library("randomForest")

#Import dataset
day = read.csv(file = "day.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))

#Structure of the data set 
str(day)

#Let's check for the Missing values
missing_values = sapply(day,function(x){  sum(is.na(x))  })
missing_values        # We don't see any missing values


                                      # FEATURE ENGINEERING


# season: Season (1:spring, 2:summer, 3:fall, 4:winter)
day$season <- factor( x = day$season, levels = c(1,2,3,4), labels = c("Spring", "Summer", "Fall", "Winter"))
                      
# yr: Year (0: 2011, 1:2012)
day$yr <- factor(x = day$yr, levels = c(0,1), labels = c("2011", "2012"))

#mnth: Month (1 to 12)

day$mnth <- factor(x = day$mnth, levels = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


#holiday: weather day is holiday or not (extracted fromHoliday Schedule)
day$holiday <- factor(x = day$holiday,levels = c(0,1),labels = c("Working Day", "Holiday") )

#weekday: Day of the week
day$weekday <- factor(x = day$weekday, levels = c(0:6), labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ))
 
#workingday: If day is neither weekend nor holiday is 1, otherwise is 0.
day$workingday <- factor(x = day$workingday, levels = c(0,1), labels = c("Weekend","Working Day"))


#weathersit: (extracted fromFreemeteo)
#1: Clear, Few clouds, Partly cloudy, Partly cloudy
#2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
day$weathersit <- factor( x = day$weathersit, levels = c(1:4), labels = c("Clear", "Mist+Clouds", "Light Snow", "Heavy Rain"))


#temp: Normalized temperature in Celsius. The values are derived via
# temp = (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale) 
#t = temp*(39+8) -8 which is approximately t = temp *39
day$temp = day$temp*39


#atemp: Normalized feeling temperature in Celsius. The values are derived via
#(t-t_min)/(t_maxt_min), t_min=-16, t_max=+50 (only in hourly scale)
day$atemp = day$atemp*50


#hum: Normalized humidity. The values are divided to 100 (max)
day$hum = day$hum*100


#windspeed: Normalized wind speed. The values are divided to 67 (max)
day$windspeed = day$windspeed*67

#Let's change the variables to proper data types

day$dteday <- as.character(day$dteday)
day$mnth <- as.factor(day$mnth)
day$weekday = as.factor(as.character(day$weekday))
day$workingday = as.factor(as.character(day$workingday))



                              # EXPLORATORY DATA ANALYSIS

#Let's see how different scenarios affect the Bike rentals



a = ggplot(day, aes(season)) + geom_bar(fill = "cyan3") + labs(x = "Season", y = "Number of bike rental")
#a

b = ggplot(day, aes(mnth)) + geom_bar(fill = "darkcyan") + labs(x = "Month", y = ("Number of bike rentals"))
#b

c = ggplot(day, aes(workingday)) + geom_bar(fill = "aquamarine") + labs(x = "Working day", y="Number of Bike rentals")
#c

d = ggplot(day, aes(weekday)) + geom_bar(fill = "cornflowerblue") + labs(x = "Weekday", y = "Number of Bike rentals")
#d

e = ggplot(day, aes(yr)) + geom_bar(fill = "powderblue")  + labs(x = "Year", y = "Number of Bike rentals") 
#e

f = ggplot(day, aes(holiday)) + geom_bar(fill = "midnightblue") + labs(x="Holiday or Weekend", y = "Number of Bike rentals")
#f

ggarrange(a,b,c,d,e,f, widths = c(1,1))


#Let's see how different weather conditions affect the count


chart1 = ggplot(day, aes(x = temp, y = cnt, col = factor(mnth))) + 
  geom_point() + labs( x = "Temperature", y = "No. of Bike Rentals", col = "Month") +
    stat_smooth(method = lm, col = "mediumblue")
#chart1

chart2 = ggplot(day, aes(x = hum, y = cnt, col = factor(mnth))) + geom_point() +
  labs( x = "Humidity", y = "No. of Bike Rentals", col = "Month") + 
  stat_smooth(method = "lm", col = "olivedrab4")
#chart2

chart3 = ggplot(day, aes(x = weathersit, y=cnt, col = factor(mnth) )) + geom_point() + geom_smooth()+
  labs(x = "WeatherSit", y = "No. of Bike Rentals", col = "Month")
#chart3

chart4 = ggplot(day, aes(x = windspeed, y = cnt, col = factor(mnth))) + geom_point() +
  labs(x = "Windspeed", y= "No. of Bike Rentals", col = "Month") + stat_smooth(method = lm, col = "red")
#chart4

ggarrange(chart1, chart2, chart3, chart4, widths = c(1,1))
# Let's use the Correlation Plot to check the Co-efficient of Correlation and select hghly correlated variables


p1 <- corrgram(day,order =  FALSE, main = "Correlation Plot")
p2 <-corrgram(day,order =  FALSE, main = "Correlation Plot", panel = panel.cor)

#Let's remove variables which do not help us in the prediction

data1 <- subset(day, select = c(season,yr,mnth,weekday,workingday,weathersit,temp,hum,windspeed,cnt))
data1

set.seed(300)
index = sample(1:nrow(data1), as.integer(0.7*nrow(data1)))
train = data1[index,]
test = data1[-index,] 

#Linear Regression

model1 = lm(formula = cnt~., data = train)

summary(model1)

prediction1 = predict(model1, test[,-10])

df = data.frame("actual" = test[,10], "pred" = prediction1)
head(df)


regr.eval(trues = test[,10], preds = prediction1, stats = c("mae", "mse", "rmse", "mape"))

require(stats)
mape(test[,10], prediction1)


# Random Forest
model2 = randomForest(cnt~., data = train, tree = 100)
summary(model2)
prediction2 = predict(model2, test[,-10])

df = cbind(df, prediction2)
head(df)

regr.eval(trues = test[,10], preds = prediction2, stats = c("mae","mse","rmse","mape"))
mape(test[,10], prediction2)

predict(model2, test[1,])


