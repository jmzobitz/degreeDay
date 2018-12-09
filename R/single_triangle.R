#' Compute the degree day from temperature data.
#'
#' \code{single_triangle} Computes the degree days from temperature data, using interpolation from 
#'
#' @param min_t Minimum daily temperature
#' @param max_t Maximum daily temperature
#' @param t_L Lower bound on the temperature
#' @param t_U Upper bound on the temperature
#' @seealso For the equations to compute the degree day: \url{http://ipm.ucanr.edu/WEATHER/ddst_tbl.html}
#' @return Degree day from the list of numbers.
#' @examples
#'
#' single_triangle(10,15,11,16)

#' @export

single_triangle <-function(min_t,max_t,t_L,t_U) {
  if (min_t!=max_t) {
    if(t_L < min_t & t_U < max_t) {  # Intercepted by the upper threshold.
      degree_day <- 6*(max_t+min_t-2*t_L)/12 -(6*(max_t-t_U)^2/(max_t-min_t))/12
    } else if( min_t <= t_L & t_U < max_t) {  # Intercepted by both thresholds.
      degree_day = (6*(max_t-t_L)^2/(max_t-min_t) - 6*(max_t-t_U)^2/(max_t-min_t) )/12
    } else if(t_L < min_t & max_t <= t_U ) {  # Entirely between both thresholds.
      degree_day <- 6*(max_t + min_t -2*t_L)/12
    } else if(min_t <= t_L & max_t <= t_U) { #Intercepted by the lower threshold
      degree_day <- 6*(max_t- t_L)^2/(max_t-min_t)/12
    } else if(t_U < min_t) {degree_day <- t_U-t_L} #Completely above both thresholds
    else if(max_t < t_L) {degree_day <- 0} #Completely below both thresholds
    
  } else {degree_day <- 0}
  
  return(degree_day)
  
  
}