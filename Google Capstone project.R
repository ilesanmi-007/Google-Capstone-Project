#install packages
#install.packages('lubridate')
#install.packages('ggplot2')
#install.packages('janitor')
#install.packages('skimr')
#install.packages('tidyverse')

#Load packages

library(tidyverse)  # for wrangling data
library(lubridate)  # for wrangling date attributes
library(ggplot2)  # for visualizing data
library(janitor)  # for examining and cleaning dirty data
library(skimr)  # for providing summary statistic


#=========================================================
# STEP 1: GATHER DATA TO PREPARE FOR ANALYSIS
#=========================================================

#Import your data.
may22 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202205-divvy-tripdata.csv')
april22 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202204-divvy-tripdata.csv')
march22 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202203-divvy-tripdata.csv')
feb22 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202202-divvy-tripdata.csv')
jan22 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202201-divvy-tripdata.csv')
dec21 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202112-divvy-tripdata.csv')
nov21 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202111-divvy-tripdata.csv')
oct21 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202110-divvy-tripdata.csv')
#oct21 <- select(oct21, -X) #had to remove a column i mistakenly added

sep21 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202109-divvy-tripdata.csv')
aug21 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202108-divvy-tripdata.csv')
jul21 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202107-divvy-tripdata.csv')
june21 <- read.csv('C:/Users/ilesanmi/Desktop/ipny/google/Capstone Project/extracted/202106-divvy-tripdata.csv')


#Make columns consistent and merge them into a single dataframe.

fall <- rbind(sep21, oct21)
summer <- rbind(june21,jul21, aug21)
spring <- rbind(may22,march22, april22)
winter <- rbind(nov21, dec21, jan22, feb22)



#single dataframe
all_trips <- rbind(fall,summer, spring, winter)

#=========================================================
# STEP 2: aSSESS DATA TO PREPARE FOR ANALYSIS
#=========================================================

# Inspect the new table that has been created

head(all_trips)#shows first 6 rows
summary(all_trips)  #gives the details of each column and statistics of number column
glimpse(all_trips) #gives details of rows and columns with sample rows
#skim(all_trips) #more details of rows and columns and more detailed statistics



#=========================================================
# STEP 3: CLEAN AND CALCULATE DATA TO PREPARE FOR ANALYSIS
#=========================================================


# Check if there is any duplicated value in ride_id, which supposed to be all unique values.
sum(duplicated(all_trips$ride_id))  # 2082 duplicate values found in ride_id

# Drop duplicate
#https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/

new_df <- all_trips[!duplicated(all_trips$ride_id), ]

new_new_df <- na.omit(new_df)

#CHECK IF DUPLICATED ROWS ARE REMOVED
#summary(new_df) 5858694
#summary(new_new_df) 5853660

#=========================================================
# STEP 3:EXPLORATORY ANALYSIS
#=========================================================

#===== 1. ======
# Get unique values in rideable_type
unique(new_df$rideable_type)     # unique values: classic bike, docked bike, electric bike
new_df %>% ggplot() +
  geom_bar(aes(x=rideable_type))+
  ggtitle('Ride Type')


#===== 2. ======
# Get unique values in member_casual
unique(new_df$member_casual)     # unique values: member, casual
new_df %>% ggplot() +
  geom_bar(aes(x=member_casual))+
  ggtitle('User types')


#===== 3. ======
# Get unique values in rideable_type
unique(new_df$rideable_type)     # unique values: classic bike, docked bike, electric bike
new_df %>% ggplot() +
  geom_bar(aes(x=rideable_type, fill = member_casual), position = position_dodge())+
  ggtitle('Ride Types for each user type')

#===== 4. ======
#what hour of most ride
new_new_df$started_at <-  strptime(new_new_df$started_at, "%H:%M:%S")
#new_df$ride_hr <- hour(new_df$ride_length)
glimpse(new_new_df)
new_new_df$started_hr <- format(as.POSIXct(new_new_df$started_at), format = "%H")
glimpse(new_new_df)
new_new_df$started_hr_int <- as.numeric(new_new_df$started_hr)

new_new_df %>% ggplot() + 
  geom_bar(aes(x=started_hr_int)) + ggtitle('Start Ride')

#===== 5. ======
#Start Ride for  different user type
new_new_df %>% ggplot() + 
  geom_bar(aes(x=started_hr_int, fill = member_casual), position = position_dodge()) + ggtitle('Start Ride for  different user type')



#===== 6. ======
#what hour of most ride
new_new_df$ended_at <-  strptime(new_new_df$ended_at, "%H:%M:%S")
#new_df$ride_hr <- hour(new_df$ride_length)
glimpse(new_new_df)
new_new_df$ended_hr <- format(as.POSIXct(new_new_df$ended_at), format = "%H")
glimpse(new_new_df)
new_new_df$ended_hr_int <- as.numeric(new_new_df$ended_hr)

new_new_df %>% ggplot() + 
  geom_bar(aes(x=ended_hr_int))+ggtitle('End ride')

#===== 6. ======
#plot of start hr and end hr

#new_new_df %>%
 # ggplot(aes(x=ended_hr_int)) + 
  #geom_bar(aes(fill = started_hr_int, color = started_hr_int), stat = 'identity', position = position_dodge())
  

#===== 7. ======
#show the statistics ride_length of rides
new_new_df$ride_length <-  strptime(new_new_df$ride_length, "%H:%M:%S")
#new_df$ride_hr <- hour(new_df$ride_length)
glimpse(new_new_df)

new_new_df$ride_min <- format(as.POSIXct(new_new_df$ride_length), format = "%M")
glimpse(new_new_df)
new_new_df$ride_min_int <- as.numeric(new_new_df$ride_min)
glimpse(new_new_df)

new_new_df <- na.omit(new_new_df) #removing missing values in all dataset
#unique(new_new_df$ride_min_int)

new_new_df %>% ggplot() + 
  geom_bar(aes(x=ride_min_int))+ggtitle('Rides in minutes') #most of the rides are lesser than 20 min
#@@@@@@@ need to remove mins == 0


#===== 8. ======
#FURTHER VIEW BASED ON USER TYPE
new_new_df %>% ggplot() + 
  geom_bar(aes(x=ride_min_int, fill = member_casual), position = position_dodge())+
  ggtitle('Plots of Ride Length per Usertype')
#casual members rides a little longer than member riders


#===== 9. ======
#FURTHER VIEW BASED ON USER TYPE
new_new_df %>% ggplot() + 
  geom_bar(aes(x=ride_min_int, fill = rideable_type), position = position_dodge())+
  ggtitle('Plots of Ride Length per rideable_type')
#casual members rides a little longer than member riders





#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
#save(new_new_df, file = 'total.csv') #saving for Tableau Visuals

