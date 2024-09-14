# Installing packages : 
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")

# Loading packages :

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(skimr)
library(janitor)

# Importing Datasets:
  #dailyActivities_mergerd
  #dailyCalories_mergerd
  #dailyIntensities_mergerd
  #dailySteps_merged
  #heartrate_seconds_merged
  #sleepDay_merged
  #weightLogInfo_merged


head(dailyActivity_merged)
colnames(dailyActivity_merged)
str(dailyActivity_merged)

head(weightLogInfo_merged)
colnames(weightLogInfo_merged)
str(weightLogInfo_merged)

# Formatting dataset :
#---------------------

# Activity
dailyActivity_merged$ActivityDate=as.POSIXct(dailyActivity_merged$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
dailyActivity_merged$date <- format(dailyActivity_merged$ActivityDate, format = "%m/%d/%y")
#dailyActivity_merged$ActivityDate=as.Date(dailyActivity_merged$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
#dailyActivity_merged$date=as.Date(dailyActivity_merged$date, format="%m/%d/%Y")


# Intensities
dailyIntensities_merged$ActivityDay=as.Date(dailyIntensities_merged$ActivityDay, format="%m/%d/%Y", tz=Sys.timezone())
hourlyIntensities_merged$ActivityHour=as.POSIXct(hourlyIntensities_merged$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourlyIntensities_merged$time <- format(hourlyIntensities_merged$ActivityHour, format = "%H:%M:%S")
hourlyIntensities_merged$date <- format(hourlyIntensities_merged$ActivityHour, format = "%m/%d/%y")

# Sleep
sleepDay_merged$SleepDay=as.POSIXct(sleepDay_merged$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleepDay_merged$date <- format(sleepDay_merged$SleepDay, format = "%m/%d/%y")
sleepDay_merged$date=as.Date(sleepDay_merged$date, "% m/% d/% y")

# calories
dailyCalories_merged$ActivityDay=as.POSIXct(dailyCalories_merged$ActivityDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
dailyCalories_merged$time <- format(dailyCalories_merged$ActivityDay, format = "%H:%M:%S")
dailyCalories_merged$date <- format(dailyCalories_merged$ActivityDay, format = "%m/%d/%y")

hourlyCalories_merged$ActivityHour=as.POSIXct(hourlyCalories_merged$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourlyCalories_merged$time <- format(hourlyCalories_merged$ActivityHour, format = "%H:%M:%S")
hourlyCalories_merged$date <- format(hourlyCalories_merged$ActivityHour, format = "%m/%d/%y")

head(dailyCalories_merged)
head(hourlyCalories_merged)

# Summarizing the dataset (Analyze Phase)

n_distinct(dailyActivity_merged$Id)
n_distinct(dailyCalories_merged$Id)
n_distinct(hourlyCalories_merged$Id)
n_distinct(dailyIntensities_merged$Id)
n_distinct(sleepDay_merged$Id)
n_distinct(weightLogInfo_merged$Id)


# activity
dailyActivity_merged %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()


# explore num of active minutes per category
dailyActivity_merged %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# calories
hourlyCalories_merged %>%
  select(Calories) %>%
  summary()

# sleep
sleepDay_merged %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

# weight
weightLogInfo_merged %>%
  select(WeightKg, BMI) %>%
  summary()


#Merging data

merged_data <- merge(sleepDay_merged, dailyActivity_merged, by=c('Id','date'))
head(merged_data)
head(sleepDay_merged)
head(dailyActivity_merged)

#Visualization

ggplot(data=dailyActivity_merged, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")
#correlation here between Total Steps and Calories

ggplot(data=sleepDay_merged, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ geom_smooth() + labs(title="Total Minutes Asleep vs. Total Time in Bed")
#Total Minutes Asleep and Total Time in Bed looks linear.

#intensities data over time (hourly).

int_new <- hourlyIntensities_merged %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

head(dailyIntensities_merged)

ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='purple') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")


#Total Minutes Asleep and Sedentry Minutes.
ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='purple') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")