rm(list=ls())
setwd("C:/Users/Arathen/Desktop/Github Projects/Store-Item-Forecast/Eirik's Models")

# Load libraries
library(randomForest)
library(DataExplorer)
library(forecast)
library(tidyverse)
library(tseries)
library(ggplot2)

# Read in data
train <- read.csv("../train.csv")
test <- read.csv("../test.csv")
fulldata <- bind_rows(train, test)

# Do some transformations
fulldata$date <- as.Date(fulldata$date)
fulldata <- fulldata %>% mutate_at(vars(store, item), factor)

# Break into train and test again
train <- fulldata %>% filter(is.na(sales) == FALSE)
test <- fulldata %>% filter(is.na(sales) == TRUE)

# Initialize dataframe to save all predictions
preds <- data.frame()

# Create nested for loop to subset, train, and predict
for (s in 1:10) {
  for (i in 1:50) {
    # Subset
    temp_train <- train %>% filter((store == s) & (item == i)) %>% select(date, sales)
    temp_test <- test %>% filter((store == s) & (item == i))
    
    ## Prepare to fit ARIMA model
    ts_train <- ts(temp_train[,2], start=c(2013,1), frequency = 365)
    # Decomposing data
    #decomp <- stl(ts_train, s.window="periodic")
    #deseasonal <- seasadj(decomp) # Remove seasonality
    # Fit model
    fit <- auto.arima(ts_train, seasonal=TRUE)#, max.p = 15)
    # Predict
    seas_fcast <- forecast(fit, h=90)
    
    # Join predictions to subset of test data
    temp_test$sales <- as.vector(seas_fcast$mean)
    
    # Save predictions
    preds <- rbind(preds, temp_test)
    
    # Tracking
    print(paste("Store number:", s, "Item number:", i))
  }
}

# Sort preds the way the submissions are supposed to be sorted
preds <- preds %>% arrange(item, store, date)

# Write final submission file
preds_final <- data.frame(id = test$id, sales = round(preds$sales))
write_csv(preds_final, "submission.csv")
