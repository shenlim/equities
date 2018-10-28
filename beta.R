library(tidyquant)

beta <- function(symbol,index,from,to,period) {
  capm_beta <- rename((tq_performance(data = left_join((symbol %>%
                                                          tq_get(get  = "stock.prices",
                                                                 from = from,
                                                                 to   = to) %>%
                                                          tq_transmute(select     = adjusted, 
                                                                       mutate_fun = periodReturn, 
                                                                       period     = period, 
                                                                       type       = "log",
                                                                       col_rename = "Ra")),
                                                       (index %>%
                                                          tq_get(get  = "stock.prices",
                                                                 from = from,
                                                                 to   = to) %>%
                                                          tq_transmute(select     = adjusted, 
                                                                       mutate_fun = periodReturn, 
                                                                       period     = period, 
                                                                       type       = "log",
                                                                       col_rename = "Rb")),
                                                       by = c("date" = "date")),
                                      Ra = Ra,
                                      Rb = Rb,
                                      performance_fun = CAPM.beta)), Beta = 'CAPM.beta.1')
  return(capm_beta)
}