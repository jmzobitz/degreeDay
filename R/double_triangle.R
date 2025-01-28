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
  
  
  return(single_triangle(min_temp,max_temp,t_L,t_U)/2+single_triangle(min_temp_lag,max_temp,t_L,t_U)/2)
}
