### Author: JMZ, MCE, JJ
### Last modified: 12/1/21
### Purpose: create an emergence graph by lakes


library(tidyverse)  # Brings in ggplot + other data analysis tools
library(lubridate)  # Works with dates
library(scales)
library(gridExtra)
emergence_data_in <- read_csv('data-raw/2021 Itasca Exuviae Sample Identity - Sheet1.csv')   # It looks like NA is no data

### Clean up the data set

emergence_data <- emergence_data_in %>%
  select(1,4,5,6) %>%
  rename(Date=1,
         Obs_Min=2,
         Species=3,
         Confirmed=4) %>%
  filter(str_detect(Species, "A. canadensis")) %>% # Filter out only Canada Darners
  mutate(Date=mdy(Date)) %>%  # Create the date string
  mutate(Rate = Confirmed/Obs_Min) %>% # Change to a numeric value
  arrange(Date)


# Define a function that will compute the minutes for an interval
interval_minutes <- function(time) {
  out_time <- int_diff(time)
  out_duration <- as.duration(out_time)
  out_minutes <- c(as.numeric(out_duration,"minutes"),0)
  return(out_minutes)
}

emergence_data_new <- emergence_data %>%
  mutate(minutes = interval_minutes(Date)) %>%
  mutate(emergence_cum=cumsum(Rate*minutes)/sum(Rate*minutes))  # Get the cumulative pecentage - we do a left hand Riemann sum - cool!

# How many observer minutes total
how_long <- emergence_data_new %>%
  arrange(Date) %>%
  summarize(n_days=sum(minutes)) 


# Determine when we are near 50%, 90% and 100% emergence:
arranged_data_50 <-emergence_data_new %>% 
  filter(near(emergence_cum,.5,tol=0.05)) %>%
  select(Date,emergence_cum)

arranged_data_90 <-emergence_data_new %>% 
  filter(near(emergence_cum,.9,tol=0.05)) %>%
  select(Date,emergence_cum)


arranged_data_100 <-emergence_data_new %>% 
  filter(near(emergence_cum,1)) %>%
  filter(Date==min(Date)) %>%
  select(Date,emergence_cum)
# Get the plot

combined_data <- rbind(arranged_data_50,arranged_data_90,arranged_data_100)

write_csv(combined_data,file='data-raw/out-emergence-2021-jj.csv')




# Ok I am just going to work with the emergence data

# Plot of the emergence rate
p1<- emergence_data_new %>%
  ggplot(aes(x=(Date),y=Rate)) +
  geom_point(size=3) +
  geom_line(size=1) +
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
#  annotate("rect", xmin =as.Date('2017-06-25'), xmax = as.Date('2017-08-01'), ymin = -Inf, ymax = Inf,alpha=0.2) +  # Emergence period
  ylim(c(0,1.2)) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week",
               limits = as.Date(c('2021-06-01','2021-08-01'))) +
  ggtitle("a) Collection Rate")

# Plot of the cumulative emergence
p2<- emergence_data_new %>%
  ggplot(aes(x=(Date),y=emergence_cum)) +
  geom_point(size=3) +
  geom_line(size=1) +
  labs(x="Day of Year", y="Value (%)") +
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
 # annotate("rect", xmin =as.Date('2017-06-25'), xmax = as.Date('2017-08-01'), ymin = -Inf, ymax = Inf,alpha=0.2) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week",
               limits = as.Date(c('2021-06-01','2021-08-01'))) +
  ggtitle('b) Cumulative Emergence')


# Let's combine them all together.
p3=grid.arrange(p1, p2, nrow=1)

ggsave('data-raw/cumulative_emergence-out-2021.png',plot=p3,width=10,height=4)

ggsave('data-raw/cumulative_emergence-2021.tiff',plot=p3,width=10,height=6.3)





