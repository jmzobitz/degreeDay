#' Compute the degree day from temperature data.
#'
#' \code{single_triangle} Computes the degree days from temperature data, using interpolation from min and max temperature.
#'
#' @param min_temp Minimum daily temperature
#' @param max_temp Maximum daily temperature
#' @param t_L Lower bound on the temperature
#' @param t_U Upper bound on the temperature
#' @seealso For the equations to compute the degree day: \url{http://ipm.ucanr.edu/WEATHER/ddst_tbl.html}
#' @source \url{http://biomet.ucdavis.edu/DegreeDays/DegDay.htm}
#' @return Degree day from the list of numbers.
#' @examples
#'
#' single_triangle(10,15,11,16)

#' @import dplyr
#' @export


single_triangle <-function(min_temp,max_temp,t_L,t_U) {
  min_test <- between(min_temp,t_L,t_U)  # Is the minimum temperature in the range of t_L and t_U?
  max_test <- between(max_temp,t_L,t_U)  # Is the maximum temperature in the range of t_L and t_U?
  
  if (is.na(min_temp) | is.na(max_temp)) {
    degree_day<-0
    return(degree_day)
  } 
  
  if(min_test & !max_test) {  # Intercepted by the upper threshold.
      degree_day <- 6*(max_temp+min_temp-2*t_L)/12 -(6*(max_temp-t_U)^2/(max_temp-min_temp))/12
      return(degree_day)
  } 
  
  if( !min_test & !max_test & min_temp < t_L & t_U < max_temp) {  # Intercepted by both thresholds.
      degree_day = (6*(max_temp-t_L)^2/(max_temp-min_temp) - 6*(max_temp-t_U)^2/(max_temp-min_temp) )/12
      return(degree_day)
  } 
  
  if(min_test & max_test) {  # Entirely between both thresholds.
      degree_day <- 6*(max_temp + min_temp -2*t_L)/12
      return(degree_day)
  }
  
  if(!min_test & max_test) { #Intercepted by the lower threshold
      degree_day <- 6*(max_temp- t_L)^2/(max_temp-min_temp)/12
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
