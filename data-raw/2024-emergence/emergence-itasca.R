### Author: JMZ, MCE
### Last modified: 5/29/19
### Purpose: create an emergence graph by lakes


library(tidyverse)  # Brings in ggplot + other data analysis tools
library(lubridate)  # Works with dates
library(scales)
library(gridExtra)

#  Clean up an error with one of the dates
bad_date <- as.Date("1934-07-24",format="%Y-%m-%d")

emergence_data_itasca <- readxl::read_excel("data-raw/2024-emergence/Emergence data 2021_22_23 Itasca.xlsx") |>
  rename(Date=1,location=2,count=3,minutes=4,rate=5) |>
  mutate(Date = if_else(Date == bad_date,as.POSIXct("2023-07-24"),Date),
         year_day = yday(Date),
         year = year(Date)) |>
  relocate(Date,year,year_day)

# Define a function that will compute the minutes for an interval
interval_minutes <- function(time) {
  out_time <- int_diff(time)
  out_duration <- as.duration(out_time)
  out_minutes <- c(as.numeric(out_duration,"minutes"),0)
  return(out_minutes)
}

first_cumulative_emergence <- function(input_data,percent=c(.5,.9,1),tol_in=0.10) {
  
  map_dfr(.x=percent,.f=~(  input_data |>
                              filter(near(emergence_cum,.x,tol=tol_in)) |>
                              mutate(diff = abs(emergence_cum-.x)) |>
                              slice_min(tibble(diff,Date)) |>  # Break ties by date
                              select(-diff) |>
                              mutate(percent = .x) |>
                              select(Date,percent) ))
  
}

# Load up previous data
load('data-raw/2024-emergence/previous-data.Rda')

combined_data_wrangled <- combined_data |>
  mutate(data = map(.x=data,.f=~(.x |> mutate(Date = as.POSIXct(Date),
                                              year_day = yday(Date)) |> 
                                   rename(rate = Rate) |>
                                   select(-Species,-number,-duration,-Date_new))))


emergence_data_out <- emergence_data_itasca |>
  group_by(location,year) |>
  arrange(Date) |>
  nest() |>
  rbind(combined_data_wrangled) |>
  mutate(data = map(.x=data,.f=~(.x |> 
                                   mutate(minutes_interval=interval_minutes(Date),
                                          emergence_cum=cumsum(rate*minutes_interval)/sum(rate*minutes_interval))
                                 ) ) ) |>
  mutate(emergence_date = map(.x=data,.f=~first_cumulative_emergence(.x))) |>
  filter(location != "Amphipond") |>
  mutate(location = if_else(location =="WildernessBeaver","Wilderness\nBeaver",location))



p1 <- emergence_data_out %>%
  unnest(cols=c(data)) |>
  mutate(Date_new = as.Date(paste0("2021-",format(Date, "%j")), "%Y-%j"))  %>% # Just get a common date
  ggplot(aes(x=Date_new,y=rate,color=as.factor(year))) + 
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
        strip.text.y = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  ggtitle("a) Collection Rate") + ylim(c(0,1.25)) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week",
               limits = as.Date(c('2021-05-15','2021-09-01'))) +
  facet_grid(location~.) +
  annotate("rect", xmin =as.Date('2021-06-25'), xmax = as.Date('2021-09-01'), ymin = -Inf, ymax = Inf,alpha=0.2)


p2 <- emergence_data_out %>%
  unnest(cols=c(data)) |>
  mutate(Date_new = as.Date(paste0("2021-",format(Date, "%j")), "%Y-%j"))  %>% # Just get a common date |>
  ggplot(aes(x=Date_new,y=emergence_cum,color=as.factor(year))) + 
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
  ggtitle("b) Cumulative Emergence") +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week",
               limits = as.Date(c('2021-05-15','2021-09-01'))) +
  facet_grid(location~.) +
  annotate("rect", xmin =as.Date('2021-06-25'), xmax = as.Date('2021-09-01'), ymin = -Inf, ymax = Inf,alpha=0.2)




p3=gridExtra::grid.arrange(p1, p2, nrow=1)

ggsave('data-raw/2024-combined-emergence-out.png',plot=p3,width=10,height=7)

ggsave('data-raw/cumulative_emergence.tiff',plot=p3,width=10,height=6.3)




out_data <- emergence_data_out |> 
  select(-data) |>
  unnest(cols=c(emergence_date)) |>
  filter(percent == 0.9)

write_csv(out_data,file='data-raw/2024-emergence/emergence_90.csv')
