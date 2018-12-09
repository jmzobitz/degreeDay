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

#' @export

single_triangle <-function(min_temp,max_temp,t_L,t_U) {
  if (min_temp!=max_temp) {
    if(t_L < min_temp & t_U < max_temp) {  # Intercepted by the upper threshold.
      degree_day <- 6*(max_temp+min_temp-2*t_L)/12 -(6*(max_temp-t_U)^2/(max_temp-min_temp))/12
    } else if( min_temp <= t_L & t_U < max_temp) {  # Intercepted by both thresholds.
      degree_day = (6*(max_temp-t_L)^2/(max_temp-min_temp) - 6*(max_temp-t_U)^2/(max_temp-min_temp) )/12
    } else if(t_L < min_temp & max_temp <= t_U ) {  # Entirely between both thresholds.
      degree_day <- 6*(max_temp + min_temp -2*t_L)/12
    } else if(min_temp <= t_L & max_temp <= t_U) { #Intercepted by the lower threshold
      degree_day <- 6*(max_temp- t_L)^2/(max_temp-min_temp)/12
    } else if(t_U < min_temp) {degree_day <- t_U-t_L} #Completely above both thresholds
    else if(max_temp < t_L) {degree_day <- 0} #Completely below both thresholds
    
  } else {degree_day <- 0}
  
  return(degree_day)
  
  
}