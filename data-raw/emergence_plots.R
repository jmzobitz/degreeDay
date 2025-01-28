### Author: JMZ, MCE
### Last modified: 1/19/24
### Purpose: create an emergence graph by lakes -- updated to save the combined dataset for 2024 emergence data


library(tidyverse)  # Brings in ggplot + other data analysis tools
library(lubridate)  # Works with dates
library(scales)
library(gridExtra)
emergence_data_in <- read_csv('data-raw/All Species phenology 2017-2019_r-v2.csv',na = c("ND","NA"))   # It looks like NA is no data

### Clean up the data set

emergence_data <- emergence_data_in %>%
  select(1:6) %>%
  rename(Lake=Pond,number = 4, duration=5, Rate = 6) %>%
  mutate(Rate = as.numeric(Rate)) %>% # Change to a numeric value
  filter(!is.na(Rate)) %>%   # remove rates that are NA
  mutate(Date=mdy(Date)) %>%  # Create the date string
  mutate(year=year(Date)) %>%
  mutate(Date_new = as.Date(paste0("2017-",format(Date, "%j")), "%Y-%j"))  %>% # Just get a common date
  mutate(Lake=ifelse(Lake=='Fishless','Warner',Lake))   # Rename the column as lake just to make it work with stuff below.

# We need to set the class in emergence data
class(emergence_data$Date_new) <- class(emergence_data$Date)

# Define a function that will compute the minutes for an interval
interval_minutes <- function(time) {
  out_time <- int_diff(time)
  out_duration <- as.duration(out_time)
  out_minutes <- c(as.numeric(out_duration,"minutes"),0)
  return(out_minutes)
}
  
emergence_data <- emergence_data %>%
  filter(Lake!="Total") %>% # Remove the total rates
  group_by(Lake,year,Species) %>%
  arrange(Date) %>%
  mutate(minutes = interval_minutes(Date)) %>%
  mutate(emergence_cum=cumsum(Rate*minutes)/sum(Rate*minutes))  # Get the cumulative pecentage - we do a left hand Riemann sum - cool!

how_long <- emergence_data %>%
  filter(Lake!="Total") %>% # Remove the total rates
  group_by(Lake,year,Species) %>%
  arrange(Date) %>%
  summarize(n_days=sum(minutes)) 


# Determine when we are near 50%, 90% and 100% emergence:
arranged_data_50 <-emergence_data %>% 
  group_by(year,Lake,Species) %>%
  filter(near(emergence_cum,.5,tol=0.05)) %>%
  select(Date,Lake,emergence_cum)

arranged_data_90 <-emergence_data %>% 
  group_by(year,Lake,Species) %>%
  filter(near(emergence_cum,.9,tol=0.05)) %>%
  select(Date,Lake,emergence_cum)
  

arranged_data_100 <-emergence_data %>% 
  group_by(year,Lake,Species) %>%
  filter(near(emergence_cum,1)) %>%
  filter(Date==min(Date)) %>%
  select(Date,Lake,emergence_cum)
# Get the plot

combined_data <- emergence_data |> filter(Species == "A. canadensis") %>%
  rename(location=Lake) |>
  group_by(year,location) |>
  arrange(Date) |>
  nest()



save(combined_data,file='data-raw/2024-emergence/previous-data.Rda')

emergence_data %>%
  ggplot(aes(x=Date_new,y=Rate,color=as.factor(year),shape=Species)) + 
  geom_point(size=2) +
  geom_line(size=1) +
  facet_grid(Lake~.,scales="free_y") +
  labs(x="Day of Year", y="Emergence Rate (exuviae / min)",color="Year") +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=28),
        title=element_text(size=26),
        legend.text=element_text(size=12),
        legend.title=element_text(size=16)) +
  theme(legend.position="bottom") +
  theme(strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=16),
        strip.background = element_rect(colour="white", fill="white")) +
  xlim(c(140,200))


# Ok I am just going to work with the emergence data
# First we need to add in a column so we can plot the multiple years on the same axis.
p1<- emergence_data %>%
  filter(Species == "A. canadensis") %>%
  ggplot(aes(x=(Date_new),y=Rate,color=as.factor(year))) +
  geom_point(size=3) +
  geom_line(size=1) +
  facet_grid(Lake~.) +
  labs(x="Day of Year", y="Exuvia / minute",color="Year",shape="Year") +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.text.x = element_text(angle=45,vjust=1.0,hjust=1.0),
        axis.title=element_text(size=24),
        title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=16)) +
  theme(legend.position="bottom") +
  theme(strip.text.x = element_text(size=12),
        strip.text.y = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
annotate("rect", xmin =as.Date('2017-06-25'), xmax = as.Date('2017-08-01'), ymin = -Inf, ymax = Inf,alpha=0.2) +
  ylim(c(0,1.2)) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week",
               limits = as.Date(c('2017-05-15','2017-08-01'))) +
  ggtitle("a) Collection Rate")


p2<- emergence_data %>%
  filter(Species == "A. canadensis") %>%
  ggplot(aes(x=(Date_new),y=emergence_cum,color=as.factor(year))) +
  geom_point(size=3) +
  geom_line(size=1) +
  facet_grid(Lake~.) +
  labs(x="Day of Year", y="Value (%)",color="Year",shape="Year") +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.text.x = element_text(angle=45,vjust=1.0,hjust=1.0),
        axis.title=element_text(size=24),
        title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=16)) +
  theme(legend.position="bottom") +
  theme(strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=16),
        strip.background = element_rect(colour="white", fill="white")) +
  geom_hline(yintercept = c(.5,.9,1),linetype='dashed') +
  scale_y_continuous(labels = percent) +
  annotate("rect", xmin =as.Date('2017-06-25'), xmax = as.Date('2017-08-01'), ymin = -Inf, ymax = Inf,alpha=0.2) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week",
               limits = as.Date(c('2017-05-15','2017-08-01'))) +
  ggtitle('b) Cumulative Emergence')


p3=grid.arrange(p1, p2, nrow=1)

ggsave('data-raw/cumulative_emergence-out.png',plot=p3,width=10,height=6.3)

ggsave('data-raw/cumulative_emergence.tiff',plot=p3,width=10,height=6.3)





