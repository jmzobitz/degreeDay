#' Compute the degree day from temperature data.
#'
#' \code{double_sine} Computes the degree days from temperature data, using interpolation from min and max temperature.
#'
#' @param min_temp Minimum daily temperature
#' @param max_temp Maximum daily temperature
#' @param min_temp_lag The next day's minimum daily temperature
#' @param t_L Lower bound on the temperature
#' @param t_U Upper bound on the temperature
#' @seealso For the equations to compute the degree day: \url{http://ipm.ucanr.edu/WEATHER/ddds_tbl.html}.  Because this is done over a 12 day period, we have an extra minimum temperature to compute, hence the extra minimum temperature
#' @source \url{http://biomet.ucdavis.edu/DegreeDays/DegDay.htm}
#' @return Degree day from the list of numbers.
#' @examples
#'
#' double_sine(10,15,12,11,16)

#' @import dplyr
#' @export


double_triangle <-function(min_temp,max_temp,min_temp_lag,t_L,t_U) {
 
  
  return(single_sine(min_temp,max_temp,t_L,t_U)/2+single_sine(min_temp_lag,max_temp,t_L,t_U)/2)
}
