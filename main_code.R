# Uber Data Analysis

getwd() #check the directory
setwd("C:/Users/User/OneDrive/Desktop/Uber data anaysis") # set directory
#file.exists("C:/Users/User/OneDrive/Desktop/Uber data anaysis") # Check if the file exists
#files <- list.files("C:/Users/User/OneDrive/Desktop/Uber data anaysis") # Check the folder has files in it
# print(files)

april <- read.csv("april.csv")
may <- read.csv("may.csv")
june <- read.csv("june.csv")
july <- read.csv("july.csv")
august <- read.csv("august.csv")
september <- read.csv("september.csv")

combined_data <- rbind(april, may, june, july, august, september)
View(combined_data)
str(combined_data)

#import libraries
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("DT")
#install.packages("scales")

#load libraries
library("ggplot2")
library("ggthemes")
library("lubridate")
library("dplyr")
library("tidyr")
library("tidyverse")
library("DT")
library("scales")

#vector of colours

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
colors

head(combined_data)

#formatting date and time variable

combined_data$Date.Time <- as.POSIXct(combined_data$Date.Time, format="%m/%d/%Y %H:%M:%S")
combined_data$Time <- format(as.POSIXct(combined_data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
combined_data$Date.Time <- ymd_hms(combined_data$Date.Time)

head(combined_data)

#creating seperate columns for month, day and year

combined_data$day <- factor(day(combined_data$Date.Time))
combined_data$month <- factor(month(combined_data$Date.Time, label=TRUE))
combined_data$year <- factor(year(combined_data$Date.Time))
combined_data$dayofweek <- factor(wday(combined_data$Date.Time, label=TRUE))

head(combined_data)

#adding ime variables

combined_data$second = factor(second(hms(combined_data$Time)))
combined_data$minute = factor(minute(hms(combined_data$Time)))
combined_data$hour = factor(hour(hms(combined_data$Time)))

head(combined_data)

#DATA VISUALIZATION

#1 Ploting the trips on hourly basis in a particular day

hourly_data <- combined_data %>% group_by(hour) %>% dplyr::summarise(Total = n())
datatable(hourly_data)
# %>% is called the pipe operator which allows for more readable and concise code by chaining together a sequence of operations
# dplyr data manipulation tool 
# datatable is used to display data in a searchable JavaScript table using R

#Plotting data by hour

ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="darkblue") + 
  ggtitle("Trips on Hourly basis", subtitle = "(for any particular day)") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

# aes aesthetic mappings and these mappings describe how data variables are mapped to visual properties
#geom_bar is a versatile function and allows for a variety of bar chart configurations
#legend.position is used to define position in/out the graph

#2 Plotting by month and hour
month_hour_data <- combined_data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())
datatable(month_hour_data)

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)

#3 Grouping the data by the day of the month
day_data <- combined_data %>% group_by(day) %>% dplyr::summarize(Trips = n())
day_data

#Plotting the data by the day
ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)

#4 Grouping the data by the day week and month
day_month_data <- combined_data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
day_month_data

#Plotting the data by the day week and month
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

#Number of trips during the given months(april to september) of the year
month_data <- combined_data %>% group_by(month) %>% dplyr::summarize(Total = n())
month_data

ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# Create a visualization of Trips done in NYC in given months
min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

ggplot(combined_data, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING APR-SEP")


