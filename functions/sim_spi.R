

# *------------------------------------------------------------------
# | PROGRAM NAME: sim_spi
# | FILE NAME: .R
# | DATE:
# | CREATED BY:  Jim Stagge
# *----------------------------------------------------------------
# | PURPOSE:  Generates a random sequence of SPI values
# |
# |
# *------------------------------------------------------------------

### Date helper
create_day_seq <- function(n_years, start_date, leap_days = TRUE){
  require(lubridate)

  date_seq <- seq(start_date, start_date + years(n_years) - 1, by = "days")

  ## Remove leap Days to make a 365 day year
  if(leap_days == FALSE){
    date_df <- data.frame(date = date_seq) %>%
      mutate(year = year(date), month = month(date), day = day(date)) %>%
      mutate(leap_day = case_when(month == 2 & day == 29~1,
        TRUE ~0)
      ) %>%
      filter(leap_day == 0)

    date_seq <- date_df$date
   }

  return(date_seq)
}


create_month_seq <- function(n_years, start_date, leap_days = TRUE){
  require(lubridate)

  date_seq <- seq(start_date, start_date + years(n_years) - 1, by = "months")

  return(date_seq)
}




### Function to simulate SPI
sim_spi <- function(date_seq, accum_period){
  ###
  require(tidyverse)
  require(lubridate)

  ### Create the dataframe object to hold
  spi_df <- data.frame(date = date_seq) %>%
    mutate(jdate = yday(date)) %>%
  	mutate(month = month(date), day = day(date), year = year(date))

  ### Simulate the SPI
  n_period <- dim(spi_df)[1]

  ### Use a 92 day moving average MA(91) with innovations of sqrt(92)/n_roll, which produces N(0,1)
  ### Technically, the moving average would be on precip, not SPI, but this is a very close approximation
  innov_c <- rnorm(n_period, 0, sqrt(accum_period))

  ### Generate SPI series using an MA(91) model with coef of 1. Need to remove the first 91 values
  spi <- arima.sim(list(order = c(0,0,(accum_period - 1)), ma = rep(1,(accum_period -1))), n = n_period, innov=innov_c/accum_period)
  spi[seq(1,(accum_period-1))] <- NA

  ### Add back to the dataframe
  spi_df <- spi_df %>%
      mutate(innov = innov_c/accum_period) %>%
	    mutate(spi = c(spi)) %>%
      select(date, year, month, day, jdate, innov, spi)

    ### Return result
    return(spi_df)

}
