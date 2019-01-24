library(devtools)
install_github("jmzobitz/degreeDay")

library(degreeDay)
library(tidyverse)

t_L <- 10
t_U <- 15

### This would be in our code



daily_data_fl <- forest_lake %>%
  mutate(lag_min =lead(forest_lake$min_temp,1) ) %>%  # Add in a minimum temperature for next day
  mutate(triangle = mapply(single_triangle,min_temp,max_temp,t_L,t_U),
         sine = mapply(single_sine,min_temp,max_temp,t_L,t_U),
         dtriangle= mapply(double_triangle,min_temp,max_temp,lag_min,t_L,t_U),
         dsine=mapply(double_sine,min_temp,max_temp,lag_min,t_L,t_U)) %>%
  select(year,day,triangle,sine,dtriangle,dsine)


daily_data_r <- railtracks %>%
  mutate(lag_min =lead(railtracks$min_temp,1) ) %>%  # Add in a minimum temperature for next day
  mutate(triangle = mapply(single_triangle,min_temp,max_temp,t_L,t_U),
         sine = mapply(single_sine,min_temp,max_temp,t_L,t_U),
         dtriangle= mapply(double_triangle,min_temp,max_temp,lag_min,t_L,t_U),
         dsine=mapply(double_sine,min_temp,max_temp,lag_min,t_L,t_U)) %>%
  select(year,day,triangle,sine,dtriangle,dsine)

daily_data_s <- stickleback %>%
  mutate(lag_min =lead(stickleback$min_temp,1) ) %>%  # Add in a minimum temperature for next day
  mutate(triangle = mapply(single_triangle,min_temp,max_temp,t_L,t_U),
         sine = mapply(single_sine,min_temp,max_temp,t_L,t_U),
         dtriangle= mapply(double_triangle,min_temp,max_temp,lag_min,t_L,t_U),
         dsine=mapply(double_sine,min_temp,max_temp,lag_min,t_L,t_U)) %>%
  select(year,day,triangle,sine,dtriangle,dsine)

daily_data_wf <- warner_fishless %>%
  mutate(lag_min =lead(warner_fishless$min_temp,1) ) %>%  # Add in a minimum temperature for next day
  mutate(triangle = mapply(single_triangle,min_temp,max_temp,t_L,t_U),
         sine = mapply(single_sine,min_temp,max_temp,t_L,t_U),
         dtriangle= mapply(double_triangle,min_temp,max_temp,lag_min,t_L,t_U),
         dsine=mapply(double_sine,min_temp,max_temp,lag_min,t_L,t_U)) %>%
  select(year,day,triangle,sine,dtriangle,dsine)

