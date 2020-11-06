rm(list=ls())
setwd('C:/Users/Arathen/Desktop/Github Projects/Store-Item-Forecast/')

# Load libraries
library(randomForest)
library(DataExplorer)
library(tidyverse)
library(lubridate)

# Read in data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
fulldata <- bind_rows(train, test)

# Do some transformations
fulldata$date <- as.Date(fulldata$date)
fulldata <- fulldata %>% mutate_at(vars(store, item), factor)

#########################
## Feature Engineering ##
#########################

# Extract values from date column
fulldata$year     <- lubridate::year(fulldata$date)
fulldata$yday     <- yday(fulldata$date)
fulldata$quarter  <- quarter(fulldata$date)
fulldata$month    <- lubridate::month(fulldata$date)
fulldata$day      <- lubridate::day(fulldata$date)
fulldata$weekdays <- weekdays(fulldata$date)
fulldata$week     <- format(fulldata$date, "%V")
fulldata$week     <- as.integer(fulldata$week)
fulldata$weekdays <- factor(fulldata$weekdays,levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday",'Sunday'))
fulldata          <- fulldata %>% mutate(weekend = (weekdays %in% c("Saturday", "Sunday")) * 1)
fulldata          <- fulldata %>% mutate_at(vars(year, quarter, month, week, weekend), factor)
fulldata          <- fulldata %>% select(-`weekend <- (weekdays %in% c("Saturday", "Sunday")) * 1`)
glimpse(fulldata)

# Break into train and test again
train <- fulldata %>% filter(is.na(sales) == FALSE)
test <- fulldata %>% filter(is.na(sales) == TRUE)

# Random Forest model
# defrag memory 
save.image(file="temp.RData")
rm(list=ls())
load(file="temp.RData")
rf <- randomForest(sales ~ ., data=(train %>% select(-c(id, date))))
