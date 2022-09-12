## Set up data environment

install.packages(tidyverse)
library(tidyverse)
install.packages("lubridate")
library(lubridate)
library(scales)

## Import Cyclistic trip data from Aug2021 to Jul2022

df1 <- read_csv("Case_Study_Cyclistic/202108-divvy-tripdata.csv")
df2 <- read_csv("Case_Study_Cyclistic/202109-divvy-tripdata.csv")
df3 <- read_csv("Case_Study_Cyclistic/202110-divvy-tripdata.csv")
df4 <- read_csv("Case_Study_Cyclistic/202111-divvy-tripdata.csv")
df5 <- read_csv("Case_Study_Cyclistic/202112-divvy-tripdata.csv")
df6 <- read_csv("Case_Study_Cyclistic/202201-divvy-tripdata.csv")
df7 <- read_csv("Case_Study_Cyclistic/202202-divvy-tripdata.csv")
df8 <- read_csv("Case_Study_Cyclistic/202203-divvy-tripdata.csv")
df9 <- read_csv("Case_Study_Cyclistic/202204-divvy-tripdata.csv")
df10 <- read_csv("Case_Study_Cyclistic/202205-divvy-tripdata.csv")
df11 <- read_csv("Case_Study_Cyclistic/202206-divvy-tripdata.csv")
df12 <- read_csv("Case_Study_Cyclistic/202207-divvy-tripdata.csv")

## Combine 12 data frames into 1 data frame

cyclistic_trips <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

## Stat of data frame

summary(cyclistic_trips)

## Clean data

### Delete outliners and nonsense data

trip1 <- cyclistic_trips %>%
  filter(Duration_in_minutes >= 0.1 & Duration_in_minutes <= 14400)

### Convert "start_at" from Character format to DateTime format

trip1$started_at <- lubridate::dmy_hm(trip1$started_at)
trip1$ended_at <- lubridate::dmy_hm(trip1$ended_at)

### Create hour column

trip1$start_hour <- lubridate::hour(trip1$started_at)

### Create date column

trip1$start_date <- as.Date(trip1$started_at)

### Create month column

trip1$start_month <- strftime(trip1$started_at, "%m")

### Create weekday column

trip1$weekday <- wday(trip1$started_at, week_start=1)

summary(trip1)


## Analyze data

### Rides per hour

data <- trip1 %>%
  group_by(member_casual, start_hour) %>%
  count(start_hour) %>%
  print(n=48)

### Rides and average ride time per month
trip1 %>%
  group_by(member_casual) %>%
  count(start_month) %>%
  print(n=24)

trip1 %>%
  group_by(member_casual, start_month) %>%
  summarise(mean(Duration_in_minutes)) %>%
  print(n = 48)

### Rides and ride time over day of week

trip1 %>%
  group_by(member_casual, weekday) %>%
  count()

trip1 %>%
  group_by(member_casual, weekday) %>%
  summarise(mean(Duration_in_minutes))

### Ride type

trip1 %>%
  group_by(member_casual) %>%
  count(rideable_type)

## Visualization

trip1 %>%
  ggplot() +
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual)) +
  labs(title = "Rideable Type", x = "", legend = "") +
  scale_y_continuous(labels = comma)

trip1 %>%
  count(start_hour) %>%
  ggplot() + 
  geom_line(mapping = aes(x = start_hour, y = n)) +
  labs(title = "Rides over hours", x = "Start hour", y = "Number of rides") +
  scale_y_continuous(labels = comma)
