### Author: JMZ
### Modified: 12/8/18
### Purpose: Use provided datasets and compute the daily max and min data, cleaning it up appropriately

### We needed to look at each data set individually to determine the correct columns


### Railtracks data
library(tidyverse)
library(readxl)  # Read xcel files
library(lubridate)
library(devtools)

# Railtracks - time (1) temp (2)
data_file <- 'data-raw/Railtracks_weather.xlsx'
railtracks <-read_excel(data_file) %>%
  select(1:2) %>%   # Select the first two columns
  rename(time = 1,temperature = 2) %>%  # Rename columns
  mutate(time = mdy_hms(time),day = yday(time),year=year(time)) %>% # Then convert first column to a day format
  group_by(year,day) %>%
  summarize(min_temp = min(temperature),max_temp=max(temperature))

use_data(railtracks,overwrite = TRUE)


### Elephant Pond - time (1), temp (4)

data_file <- 'data-raw/Elephant_weather.xlsx'
elephant <-read_excel(data_file) %>%
  select(1,4) %>%   # Select the first two columns
  rename(time = 1,temperature = 2) %>%  # Rename columns
  mutate(time = mdy_hms(time),day = yday(time),year=year(time)) %>% # Then convert first column to a day format
  group_by(year,day) %>%
  summarize(min_temp = min(temperature),max_temp=max(temperature))

use_data(elephant,overwrite = TRUE)

### Stickleback Pond - time (1), temp (3)
data_file <- 'data-raw/Stickleback_weather.xlsx'
stickleback <-read_excel(data_file) %>%
  select(1,3) %>%   # Select the first two columns
  rename(time = 1,temperature = 2) %>%  # Rename columns
  mutate(time = mdy_hms(time),day = yday(time),year=year(time)) %>% # Then convert first column to a day format
  group_by(year,day) %>%
  summarize(min_temp = min(temperature),max_temp=max(temperature))

use_data(stickleback,overwrite = TRUE)


### Warner fishless - time (1), temp (2)
data_file <- 'data-raw/Warner_Fishless_weather.xlsx'
warner_fishless <-read_excel(data_file) %>%
  select(1,2) %>%   # Select the first two columns
  rename(time = 1,temperature = 2) %>%  # Rename columns
  mutate(time = mdy_hms(time),day = yday(time),year=year(time)) %>% # Then convert first column to a day format
  group_by(year,day) %>%
  summarize(min_temp = min(temperature),max_temp=max(temperature))

use_data(warner_fishless,overwrite = TRUE)


### Forest Lake weather station data
data_file <- 'data-raw/Forest Lake weather station data.csv'
forest_lake <-read_csv(data_file,skip=15) %>%
  rename(time = 1,min_temp = 2,max_temp=3) %>%  # Rename columns
  mutate(time=mdy(time)) %>%
  mutate(year=year(time),day=yday(time)) %>%
  select(year,day,min_temp,max_temp)

use_data(forest_lake,overwrite = TRUE)
