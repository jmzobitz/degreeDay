### Author: JMZ, MCE
### Last modified: 5/29/19
### Purpose: create an emergence graph by lakes


library(tidyverse)  # Brings in ggplot + other data analysis tools
library(lubridate)  # Works with dates
library(scales)
library(gridExtra)
emergence_data_in_21 <- readxl::read_excel("data-raw/2024-emergence/2021-2022 Exuviae counts.xlsx", 
                                                             sheet = "2021") |>
  select(1:4) |>
  rename(Date=1,location=2,count=3,minutes=4)

emergence_data_in_22 <- readxl::read_excel("data-raw/2024-emergence/2021-2022 Exuviae counts.xlsx", 
                                           sheet = "2022") |>
  select(1:4) |>
  rename(Date=1,location=2,count=3,minutes=4)

emergence_data_in <- rbind(emergence_data_in_21,emergence_data_in_22)

### Clean up the data set

emergence_data <- emergence_data_in |>
  group_by(Date) |>
  summarize(across(count:minutes,.fns=sum)) |>
  mutate(rate = count/minutes,
         year_day=yday(Date),
         year = year(Date)) |>
  relocate(year,Date,year_day)

# Define a function that will compute the minutes for an interval
interval_minutes <- function(time) {
  out_time <- int_diff(time)
  out_duration <- as.duration(out_time)
  out_minutes <- c(as.numeric(out_duration,"minutes"),0)
  return(out_minutes)
}

first_cumulative_emergence <- function(input_data,percent=c(.5,.9,1),tol_in=0.05) {
  
  map_dfr(.x=percent,.f=~(  input_data |>
                              filter(near(emergence_cum,.x,tol=tol_in)) |>
                              filter(Date==min(Date)) |>
                              mutate(percent = .x) |>
                              select(Date,percent) ))
  
}



emergence_data_out <- emergence_data |>
  group_by(year) |>
  arrange(Date) |>
  mutate(minutes_interval = interval_minutes(Date)) |>
  mutate(emergence_cum=cumsum(rate*minutes_interval)/sum(rate*minutes_interval)) |> # Get the cumulative pecentage - we do a left hand Riemann sum - cool!
  nest() |>
  mutate(emergence_date = map(.x=data,.f=~first_cumulative_emergence(.x)))



 

combined_data <- rbind(arranged_data_50,arranged_data_90,arranged_data_100) %>%
  filter(Species == "A. canadensis") %>%
  arrange(year,Lake,emergence_cum)

write_csv(combined_data,path='data-raw/out-emergence.csv')


p1 <- emergence_data_out %>%
  unnest(cols=c(data)) |>
  ggplot(aes(x=year_day,y=rate,color=as.factor(year))) + 
  geom_point(size=2) +
  geom_line(linewidth=1) +
  labs(x="Day of Year", y="Emergence Rate (exuviae / min)",color="Year") +
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
  ggtitle("a) Collection Rate") + ylim(c(0,1.25))


p2 <- emergence_data_out %>%
  unnest(cols=c(data)) |>
  ggplot(aes(x=year_day,y=emergence_cum,color=as.factor(year))) + 
  geom_point(size=2) +
  geom_line(linewidth=1) +
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
  ggtitle("b) Cumulative Emergence")




p3=gridExtra::grid.arrange(p1, p2, nrow=1)

ggsave('data-raw/2024-cumulative_emergence-out.png',plot=p3,width=10,height=6.3)

ggsave('data-raw/cumulative_emergence.tiff',plot=p3,width=10,height=6.3)





