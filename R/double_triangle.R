#' Compute the degree day from temperature data.
#'
#' \code{double_triangle} Computes the degree days from temperature data, using interpolation from min and max temperature.
#'
#' @param min_temp Minimum daily temperature
#' @param max_temp Maximum daily temperature
#' @param min_temp_lag The next day's minimum daily temperature
#' @param t_L Lower bound on the temperature
#' @param t_U Upper bound on the temperature
#' @seealso For the equations to compute the degree day: \url{http://ipm.ucanr.edu/WEATHER/dddt_tbl.html}.  Because this is done over a 12 day period, we have an extra minimum temperature to compute
#' @source \url{http://biomet.ucdavis.edu/DegreeDays/DegDay.htm}
#' @return Degree day from the list of numbers.
#' @examples
#'
#' double_triangle(10,15,12,11,16)

#' @import dplyr
#' @export


double_triangle <-function(min_temp,max_temp,min_temp_lag,t_L,t_U) {
  # min_test <- between(min_temp,t_L,t_U)  # Is the minimum temperature in the range of t_L and t_U?
  # max_test <- between(max_temp,t_L,t_U)  # Is the maximum temperature in the range of t_L and t_U?
  # 
  # if (is.na(min_temp) | is.na(max_temp)) {
  #   degree_day<-0
  #   return(degree_day)
  # } 
  # 
  # if(min_test & !max_test) {  # Intercepted by the upper threshold.
  #   degree_day <- 6*(max_temp+min_temp-2*t_L)/24 -(6*(max_temp-t_U)^2/(max_temp-min_temp))/24
  #   return(degree_day)
  # } 
  # 
  # if( !min_test & !max_test & min_temp < t_L & t_U < max_temp) {  # Intercepted by both thresholds.
  #   degree_day = (6*(max_temp-t_L)^2/(max_temp-min_temp) - 6*(max_temp-t_U)^2/(max_temp-min_temp) )/24
  #   return(degree_day)
  # } 
  # 
  # if(min_test & max_test) {  # Entirely between both thresholds.
  #   degree_day <- 6*(max_temp + min_temp -2*t_L)/24
  #   return(degree_day)
  # }
  # 
  # if(!min_test & max_test) { #Intercepted by the lower threshold
  #   degree_day <- 6*(max_temp- t_L)^2/(max_temp-min_temp)/24
  #   return(degree_day)
  # }
  # 
  # if( !min_test & !max_test & t_U < min_temp) { #Completely above both thresholds
  #   degree_day <- (t_U-t_L)/2
  #   return(degree_day)
  # } 
  # 
  # if( !min_test & !max_test & max_temp < t_L) { #Completely below both thresholds
  #   degree_day <- 0
  #   return(degree_day)
  # }
  
  return(single_triangle(min_temp,max_temp,t_L,t_U)/2+single_triangle(min_temp_lag,max_temp,t_L,t_U)/2)
}
