#' Almond Yield Function
#'
#' Function based on model specified in Lobell et. al, 2006 computing almond yields given climate variables.
#' @param clim Climate data (years 1989, 2010)
#' @param t1 February minimum temperature (ºC)
#' @param t2 February minimum temperature (ºC), squared
#' @param p1 January precipitation (mm)
#' @param p2 January precipitation (mm), squared
#' @param c constant term
#'
#' @returns `results` dataframe containing minimum, maximum, and mean yield for years 1989 to 2010, in units of tons per acre.
#'
#' @author Naomi Moraes, Leilanie Rubinstein

summary <- function(clim, t1 = -0.015, t2 = -0.0046, p1 = -0.07, p2 = 0.0043, c = 0.28) {

  # Find minimum February temperature for each year
  min_temp_feb <- clim %>%
    filter(month == 2) %>%
    group_by(year) %>%
    summarise(feb_min_temp = mean(tmin_c)) %>%
    ungroup()

  # Find January precipitation total for each year
  precip_jan <- clim %>%
    filter(month == 1) %>%
    group_by(year) %>%
    summarise(jan_precip = sum(precip)) %>%
    ungroup()

  # Join climate variables
  joined <- left_join(min_temp_feb, precip_jan)

  # Calculate almond yield using climate variables
  results <- joined %>%
    mutate(yield =
             t1*feb_min_temp +
             t2*feb_min_temp^2 +
             p1*jan_precip +
             p2*jan_precip^2 +
             c)

  # Calculate yield anomalies
  results <- results %>%
    summarise(min_yield = min(yield), max_yield = max(yield), mean_yield = mean(yield))

  return(results)
}
