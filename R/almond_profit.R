#' Almond Profit Function
#'
#' Function to calculate profit from almond yields
#' @param clim Climate data (years 1989, 2010)
#' @param market_price Market per ton of almonds (in USD)
#' @param production_cost Cost of production per acre (in USD)
#' @param acres Number of acres in production
#' @param yield_parameters Yeild parameters from previous functions
#'
#' @returns `profit_result` dataframe containing yearly profits, yields, and price factors
#'
#' @author Naomi Moraes, Leilanie Rubinstein, with improvements
#'
almond_profit <- function(clim,
                          market_price = 5000,
                          production_cost = 4000,
                          acres = 100,
                          yield_parameters = list(t1 = -0.015, t2 = -0.0046,
                                                  p1 = -0.07, p2 = 0.0043, c = 0.28)){

  # Calculate yields for each year using the almond_yield function
  yield_data <- almond_yield(clim,
                             t1 = yield_parameters$t1,
                             t2 = yield_parameters$t2,
                             p1 = yield_parameters$p1,
                             p2 = yield_parameters$p2,
                             c = yield_parameters$c)


  mean_yield <- mean(yield_data$yield)

  annual_yield <- mean_yield + yield_data$yield

  # Add profit calculations to the yield data
  profit_result <- yield_data %>%
    mutate(
      # Total production in tons
      quantity_produced = annual_yield * acres,

      # Assume all quantity grown is quantity sold
      revenue = quantity_produced * market_price,

      # Cost is per acre (not quantity produced)
      costs = acres * production_cost,

      # Profit calculation
      profit = revenue - costs
    )

  return(profit_result)
}
