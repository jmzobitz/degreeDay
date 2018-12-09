#' Compute the degree day from temperature data.
#'
#' \code{single_sine} Computes the degree days from temperature data, using interpolation from min and max temperature.
#'
#' @param min_temp Minimum daily temperature
#' @param max_temp Maximum daily temperature
#' @param t_L Lower bound on the temperature
#' @param t_U Upper bound on the temperature
#' @seealso For the equations to compute the degree day: \url{http://ipm.ucanr.edu/WEATHER/ddss_tbl.html}
#' @source \url{http://biomet.ucdavis.edu/DegreeDays/DegDay.htm}
#' @return Degree day from the list of numbers.
#' @examples
#'
#' single_sine(10,15,11,16)

#' @import dplyr
#' @export


single_sine <-function(min_temp,max_temp,t_L,t_U) {
  min_test <- between(min_temp,t_L,t_U)  # Is the minimum temperature in the range of t_L and t_U?
  max_test <- between(max_temp,t_L,t_U)  # Is the maximum temperature in the range of t_L and t_U?
  
 
  alpha <- (max_temp - min_temp)/2  # amplitude between min and max temp
#  theta_min <- asin( (t_L - 0.5*(max_temp+min_temp))/alpha) # min theshold intercept
#  theta_max <- asin( (t_U - 0.5*(max_temp+min_temp))/alpha) # max theshold intercept
  
  if (is.na(min_temp) | is.na(max_temp)) {
    degree_day<-0
    return(degree_day)
  } 
  
  if(min_test & !max_test) {  # Intercepted by the upper threshold.
    theta_max <- asin( (t_U - 0.5*(max_temp+min_temp))/alpha) # max theshold intercept
    
    degree_day <- 1/pi*( (0.5*(max_temp+min_temp)-t_L)*(0.5*pi+theta_max)+(t_U-t_L)*(0.5*pi-theta_max)-alpha*cos(theta_max) )
    
    return(degree_day)
  } 
  
  if( !min_test & !max_test & min_temp < t_L & t_U < max_temp) {  # Intercepted by both thresholds.
    theta_min <- asin( (t_L - 0.5*(max_temp+min_temp))/alpha) # min theshold intercept
    theta_max <- asin( (t_U - 0.5*(max_temp+min_temp))/alpha) # max theshold intercept
    
    degree_day <- 1/pi*( (0.5*(max_temp+min_temp)-t_L)*(theta_max-theta_min)+alpha*(cos(theta_min)-cos(theta_max))+(t_U-t_L)*(0.5*pi-theta_max) )
    
    
    return(degree_day)
  } 
  
  if(min_test & max_test) {  # Entirely between both thresholds. 
    degree_day <- (max_temp + min_temp)/2 - t_L
    return(degree_day)
  }
  
  if(!min_test & max_test) { #Intercepted by the lower threshold 
    
    theta_min <- asin( (t_L - 0.5*(max_temp+min_temp))/alpha) # min theshold intercept
    
    degree_day <- 1/pi*( (0.5*(max_temp+min_temp)-t_L)*(0.5*pi-theta_min)+alpha*cos(theta_min) )
    
    return(degree_day)
  }
  
  if( !min_test & !max_test & t_U < min_temp) { #Completely above both thresholds 
    degree_day <- t_U-t_L
    return(degree_day)
  } 
  
  if( !min_test & !max_test & max_temp < t_L) { #Completely below both thresholds 
    degree_day <- 0
    return(degree_day)
  }
  
  
}
