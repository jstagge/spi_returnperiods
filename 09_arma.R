# *------------------------------------------------------------------
# | PROGRAM NAME:
# | FILE NAME: .R
# | DATE:
# | CREATED BY:  Jim Stagge
# *----------------------------------------------------------------
# | PURPOSE:
# |
# |
# *------------------------------------------------------------------

###########################################################################
## Set the Paths
###########################################################################
require(here)

### Path for Data and Output
data_path <- file.path(here(), "data")
function_path <- file.path(here(), "functions")
#output_path <- "/fs/ess/PAS1921"
output_path <- file.path(here(), "output")

### Set up output folders
write_output_path <- file.path(output_path, "arma")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)

### Set up figure folder
write_figures_path <- file.path(output_path, "figures")
dir.create(write_figures_path, recursive=TRUE, showWarnings = FALSE)

###########################################################################
###  Load functions
###########################################################################
require(tidyverse)
require(tictoc)
require(viridis)

### For Dates
require(lubridate)

### To save in SVG
require(svglite)
require(viridis)


#require(tukeyGH)

select <- dplyr::select

theme_set(theme_classic(8))

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

###########################################################################
## Set initial values
###########################################################################
### Set seed so everyone gets the same random numbers (reproducible example)
set.seed(7890)


###########################################################################
## Generate theoretical ACF and PACF
###########################################################################
accum_period <- 6
n_lags <- 8

### From Brockwell, P. J. and Davis, R. A. (1991) Time Series: Theory and Methods, Second Edition. Springer.
## ACF for the MA(5) processes
acf_theor <- ARMAacf(ma= rep(1,(accum_period -1)), lag.max=n_lags, pacf = FALSE)

## PACF for the MA(5) processes
pacf_theor <- ARMAacf(ma= rep(1,(accum_period -1)), lag.max=n_lags, pacf = TRUE)

plot_acf <- data.frame(lag = as.numeric(names(acf_theor)), acf = acf_theor, pacf = c(NA, pacf_theor))

p <- ggplot(plot_acf, aes(x=lag, y=acf)) %>%
  + geom_line() %>%
  + geom_point() %>%
  + scale_x_continuous(name = "Lag", breaks = seq(0,10, by = 2)) %>%
  + scale_y_continuous(name = "Autocorrelation") %>%
  + theme_classic(12)

p

### Save plot
ggsave(file.path(write_figures_path, "acf_spi6.png"), p,  width = 3.5, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "acf_spi6.pdf"), p,    width = 3.5, height = 3)
ggsave(file.path(write_figures_path, "acf_spi6.svg"), p,    width = 3.5, height = 3)

p <- ggplot(plot_acf, aes(x=lag, y=acf)) %>%
+ geom_col(fill = "grey70") %>%
+ geom_hline(yintercept = 0, colour = "black") %>%
  + scale_x_continuous(name = "Lag", breaks = seq(0,10, by = 2)) %>%
  + scale_y_continuous(name = "Autocorrelation") %>%
  + theme_classic(12)

p

### Save plot
ggsave(file.path(write_figures_path, "acf_spi6_column.png"), p,   width = 3.5, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "acf_spi6_column.pdf"), p,   width = 3.5, height = 3)
ggsave(file.path(write_figures_path, "acf_spi6_column.svg"), p,    width = 3.5, height = 3)

p <- ggplot(plot_acf, aes(x=lag, y=pacf)) %>%
  + geom_line() %>%
  + geom_point() %>%
  + geom_hline(yintercept = 0, colour = "grey70", linetype = "longdash") %>%
  + scale_x_continuous(name = "Lag", breaks = seq(0,10, by = 2)) %>%
  + scale_y_continuous(name = "Partial Autocorrelation") %>%
  + theme_classic(12)

p

### Save plot
ggsave(file.path(write_figures_path, "pacf_spi6.png"), p,  width = 3.5, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "pacf_spi6.pdf"), p,   width = 3.5, height = 3)
ggsave(file.path(write_figures_path, "pacf_spi6.svg"), p,   width = 3.5, height = 3)


p <- ggplot(plot_acf, aes(x=lag, y=pacf)) %>%
+ geom_col(fill = "grey70") %>%
+ geom_hline(yintercept = 0, colour = "black") %>%
#  + geom_hline(yintercept = 0, colour = "grey70", linetype = "longdash") %>%
  + scale_x_continuous(name = "Lag", breaks = seq(0,10, by = 2)) %>%
  + scale_y_continuous(name = "Partial Autocorrelation") %>%
  + theme_classic(12)

p
### Save plot
ggsave(file.path(write_figures_path, "pacf_spi6_col.png"), p,  width = 3.5, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "pacf_spi6_col.pdf"), p,   width = 3.5, height = 3)
ggsave(file.path(write_figures_path, "pacf_spi6_col.svg"), p,   width = 3.5, height = 3)




n_lags <- 45

### From Brockwell, P. J. and Davis, R. A. (1991) Time Series: Theory and Methods, Second Edition. Springer.
## ACF for the MA(5) processes
acf_theor <- ARMAacf(ma= rep(1,(accum_period -1)), lag.max=n_lags, pacf = FALSE)

## PACF for the MA(5) processes
pacf_theor <- ARMAacf(ma= rep(1,(accum_period -1)), lag.max=n_lags, pacf = TRUE)

plot_acf <- data.frame(lag = as.numeric(names(acf_theor)), acf = acf_theor, pacf = c(NA, pacf_theor))


p <- ggplot(plot_acf, aes(x=lag, y=pacf)) %>%
  + geom_line() %>%
  + geom_point() %>%
  + geom_hline(yintercept = 0, colour = "grey70", linetype = "longdash") %>%
  + scale_x_continuous(name = "Lag", breaks = seq(0,50, by = 5)) %>%
  + scale_y_continuous(name = "Partial Autocorrelation", breaks = seq(-1,1, by = 0.2)) %>%
  + coord_cartesian(ylim = c(-0.2, 0.9)) %>%
  + theme_classic(12)

p

p <- ggplot(plot_acf, aes(x=lag, y=pacf)) %>%
  + geom_col(fill = "grey70") %>%
  + geom_hline(yintercept = 0, colour = "black") %>%
  + scale_x_continuous(name = "Lag", breaks = seq(0,50, by = 5)) %>%
  + scale_y_continuous(name = "Partial Autocorrelation", breaks = seq(-1,1, by = 0.2)) %>%
  + coord_cartesian(ylim = c(-0.2, 0.9)) %>%
  + theme_classic(12)

p

### Save plot
ggsave(file.path(write_figures_path, "pacf_spi6_long.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "pacf_spi6_long.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "pacf_spi6_long.svg"), p,   width = 4.5, height = 3.5)




n_lags <- 1000000

### From Brockwell, P. J. and Davis, R. A. (1991) Time Series: Theory and Methods, Second Edition. Springer.
## ACF for the MA(5) processes
acf_theor <- ARMAacf(ma= rep(1,(accum_period -1)), lag.max=n_lags, pacf = FALSE)

## PACF for the MA(5) processes
pacf_theor <- ARMAacf(ma= rep(1,(accum_period -1)), lag.max=n_lags, pacf = TRUE)

plot_acf <- data.frame(lag = as.numeric(names(acf_theor)), acf = acf_theor, pacf = c(NA, pacf_theor))

p <- ggplot(plot_acf, aes(x=lag, y=pacf)) %>%
  + geom_col(fill = "grey70") %>%
  + geom_hline(yintercept = 0, colour = "black") %>%
  #+ scale_x_continuous(name = "Lag", breaks = seq(0,n_lags, by = 500)) %>%
  + scale_y_continuous(name = "Partial Autocorrelation", breaks = seq(-1,1, by = 0.2)) %>%
  + coord_cartesian(ylim = c(-0.2, 0.9)) %>%
  + theme_classic(12)

p


ggplot(plot_acf, aes(x=lag, y=pacf)) + geom_point() + geom_line()

### Theoretical autocovariance matrix
#shoo <- acf2AR(acf_theor)
### The diagonals are the pacf



foo <- arima.sim(list(order = c(0,0,(accum_period - 1)), ma = rep(1,(accum_period -1))), n = n_period, innov=innov_c/accum_period)







### Use a 92 day moving average MA(91) with innovations of sqrt(92)/n_roll, which produces N(0,1)
### Technically, the moving average would be on precip, not SPI, but this is a very close approximation
innov_c <- rnorm(n_period, 0, sqrt(accum_period))





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















###########################################################################
## Raad in data
###########################################################################
### Read in the Daily data
daily_path <- file.path(write_output_path, "daily")
  moments_daily <- readRDS(file.path(daily_path, paste0("moments_df.rds")))
  l_mom_daily <- readRDS(file.path(daily_path, paste0("l_mom_df.rds")))
  ln3_daily <- readRDS(file.path(daily_path, paste0("ln3_df.rds")))
  spi_return_daily <- readRDS(file.path(daily_path, paste0("spi_return_df.rds")))
  return_spi_daily <- readRDS(file.path(daily_path, paste0("return_spi_df.rds")))


### Read in the monthly data
monthly_path <- file.path(write_output_path, "month")
    moments_monthly <- readRDS(file.path(monthly_path, paste0("moments_df.rds")))
    l_mom_monthly <- readRDS(file.path(monthly_path, paste0("l_mom_df.rds")))
    ln3_monthly <- readRDS(file.path(monthly_path, paste0("ln3_df.rds")))
    spi_return_monthly <- readRDS(file.path(monthly_path, paste0("spi_return_df.rds")))
    return_spi_monthly <- readRDS(file.path(monthly_path, paste0("return_spi_df.rds")))



    ###########################################################################
    ## Set up colors
    ###########################################################################
    color_list <-  turbo(8, begin = 0.1, end = 0.9)
  color_levels <- c(0.5, 1, 2, 3, 6, 9, 12, 24)



    ###########################################################################
    ## Set up USDM thresholds
    ###########################################################################
    spi_thresh <- c(-0.5, -0.8, -1.3, -1.6, -2)


###########################################################################
## Prepare data
###########################################################################
spi_return_monthly <- spi_return_monthly %>%
        mutate(resolution = "monthly") %>%
        mutate(accum_months = accum_period) %>%
        filter(accum_period > 0)


spi_return_daily <- spi_return_daily %>%
          mutate(resolution = "daily") %>%
          mutate(accum_months = accum_period/30) %>%
          filter(accum_period > 0)

spi_return_df <- spi_return_daily %>%
    bind_rows(spi_return_monthly)

spi_return_df <- spi_return_df %>%
    mutate(accum_months_factor = round(accum_months)) %>%
    mutate(accum_months_factor = case_when(accum_months == 0.5 ~ 0.5, TRUE ~ accum_months_factor)) %>%
    mutate(accum_months_factor = factor(accum_months_factor, levels = color_levels)) %>%
    mutate(resolution = factor(resolution)) %>%
    mutate(plot_group = paste0(resolution, accum_period)) %>%
    mutate(p_empir = 1/return_period_empir)



###########################################################################
## Calculate Density
###########################################################################
ln3_daily <- ln3_daily %>%
  mutate(resolution = "daily")  %>%
  mutate(accum_months = accum_period/30)

ln3_monthly <- ln3_monthly %>%
  mutate(resolution = "monthly") %>%
  mutate(accum_months = accum_period) %>%
  filter(accum_period > 0)

ln3_df <- ln3_daily %>%
  bind_rows(ln3_monthly)

ln3_df <- ln3_df %>%
  mutate(nu = exp(mulog)) %>%
  mutate(loc = zeta + nu) %>%
  mutate(scale = nu * sigmalog) %>%
  mutate(shape = -sigmalog)

ln3_df <- ln3_df  %>%
    mutate(accum_months_factor = round(accum_months)) %>%
    mutate(accum_months_factor = case_when(accum_months == 0.5 ~ 0.5, TRUE ~ accum_months_factor)) %>%
    mutate(accum_months_factor = factor(accum_months_factor, levels = color_levels)) %>%
    mutate(resolution = factor(resolution)) %>%
    mutate(plot_group = paste0(resolution, accum_period))

dens_df <- expand_grid(data.frame(x=seq(-2.5,6,by = 0.01)), ln3_df) %>%
  mutate(spi = -x)

dens_df$dens <- NA_real_
#dens_df$dens_ln3 <- NA_real_
dens_df$cum_dens <- NA_real_

for(j in seq(1, dim(dens_df)[1])){
  dens_df$dens[[j]] <-  dgno(x =   dens_df$x[[j]], loc =   dens_df$loc[[j]], scale =   dens_df$scale[[j]], shape =   dens_df$shape[[j]])
  #dens_df$dens_ln3[[j]] <-  dlnorm3(x =   dens_df$x[[j]], shape =   dens_df$sigmalog[[j]], scale =   dens_df$mulog[[j]], thres =   dens_df$zeta[[j]])
  dens_df$cum_dens[[j]] <-  1- pgno(q =   dens_df$x[[j]], loc =   dens_df$loc[[j]], scale =   dens_df$scale[[j]], shape =   dens_df$shape[[j]])
}

dens_df <- dens_df%>%
  mutate(resolution = factor(resolution, levels = c("daily", "monthly"), labels = c("Daily", "Monthly")))

dens_df <- dens_df %>%
  mutate(return_period = 1/cum_dens)


write_csv(dens_df, file.path(write_output_path, "dens_df.csv"))

##########################################################################
## Plot return period for Monthly
###########################################################################
y_breaks <- c(10^seq(0, 9), 5*10^seq(0, 9))
y_breaks <- sort(y_breaks)


plot_df <- dens_df %>%
  filter(method == "lmoment" & resolution == "Monthly")

p <- ggplot(plot_df , aes(x=spi, y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
#+ geom_point(size = 0.5) %>%
+ geom_line(size = 0.3) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
#  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
#  +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x),   labels = scales::trans_format("log10", scales::math_format(10^.x))  ) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-5.5, 1.5),  ylim = c(0.8, 1000000), expand = FALSE)
p

### Save plot
ggsave(file.path(write_figures_path, "return_per_month_lmom.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_month_lmom.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_month_lmom.svg"), p,   width = 4.5, height = 3.5)

p <- p + theme_bw(8)

### Save plot
ggsave(file.path(write_figures_path, "return_per_month_lmom_grid.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_month_lmom_grid.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_month_lmom_grid.svg"), p,   width = 4.5, height = 3.5)


p <- ggplot(plot_df , aes(x=spi, y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-5.5, 1.5),  ylim = c(0.8, 1000000), expand = FALSE)
p

### Save plot
ggsave(file.path(write_figures_path, "return_per_month_lmom_thresh.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_month_lmom_thresh.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_month_lmom_thresh.svg"), p,   width = 4.5, height = 3.5)

p <- ggplot(plot_df , aes(x=spi, y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 0.5)) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-3.7, 0.25), ylim = c(0.8, 2000), expand = FALSE)
p


### Save plot
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable.svg"), p,   width = 4.5, height = 3.5)



p <- ggplot(plot_df , aes(x=spi)) %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3, aes(y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
+ geom_point(data = spi_return_df %>% filter(resolution == "monthly"), aes(y = return_period_empir) , shape = 1) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 0.5)) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-3.7, 0.25), ylim = c(0.8, 2000), expand = FALSE)
p


### Save plot
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable_empir.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable_empir.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable_empir.svg"), p,   width = 4.5, height = 3.5)


p <- ggplot(plot_df , aes(x=spi)) %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3, aes(y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
+ geom_point(data = spi_return_df %>% filter(resolution == "monthly"), aes(y = return_period_empir) , shape = 1) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
#  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
#  +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x),   labels = scales::trans_format("log10", scales::math_format(10^.x))  ) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-5.5, 1.5),  ylim = c(0.8, 1000000), expand = FALSE) %>%
  + theme_bw(8)
p

### Save plot
ggsave(file.path(write_figures_path, "return_per_month_lmom_empir.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_month_lmom_empir.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_month_lmom_empir.svg"), p,   width = 4.5, height = 3.5)

p <- p + coord_cartesian(xlim = c(-5.5, -3.8),  ylim = c(800, 1300000), expand = FALSE)

### Save plot
ggsave(file.path(write_figures_path, "return_per_month_lmom_upper_empir.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_month_lmom_upper_empir.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_month_lmom_upper_empir.svg"), p,   width = 4.5, height = 3.5)



p <- ggplot(plot_df , aes(x=return_period)) %>%
  + geom_hline(yintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_hline(yintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3, aes(y=spi, colour = accum_months_factor, group = accum_months_factor)) %>%
+ geom_point(data = spi_return_df %>% filter(resolution == "monthly"), aes(x = return_period_empir, y = spi) , shape = 1) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
+ scale_y_continuous(name = "SPI") %>%
#   +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::comma) %>%
+ scale_x_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
+ annotation_logticks( sides = "b")  %>%
+ coord_cartesian(ylim = c(-3.7, 0.25), xlim = c(0.8, 2000), expand = FALSE)
p
### Save plot
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable_flipped.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable_flipped.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_month_lmom_usable_flipped.svg"), p,   width = 4.5, height = 3.5)



##########################################################################
## Plot return period for Monthly
###########################################################################
y_breaks <- c(10^seq(0, 9), 5*10^seq(0, 9))
y_breaks <- sort(y_breaks)


plot_df <- dens_df %>%
  filter(method == "lmoment" & resolution == "Daily")

p <- ggplot(plot_df , aes(x=spi, y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
#+ geom_point(size = 0.5) %>%
+ geom_line(size = 0.3) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
#  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
#  +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x),   labels = scales::trans_format("log10", scales::math_format(10^.x))  ) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-5.5, 1.5),  ylim = c(0.8, 1000000), expand = FALSE)
p

### Save plot
ggsave(file.path(write_figures_path, "return_per_daily_lmom.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_daily_lmom.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_daily_lmom.svg"), p,   width = 4.5, height = 3.5)

p <- p + theme_bw(8)

### Save plot
ggsave(file.path(write_figures_path, "return_per_daily_lmom_grid.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_grid.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_grid.svg"), p,   width = 4.5, height = 3.5)


p <- ggplot(plot_df , aes(x=spi, y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-5.5, 1.5),  ylim = c(0.8, 1000000), expand = FALSE)
p

### Save plot
ggsave(file.path(write_figures_path, "return_per_daily_lmom_thresh.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_thresh.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_thresh.svg"), p,   width = 4.5, height = 3.5)

p <- ggplot(plot_df , aes(x=spi, y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 0.5)) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-3.7, 0.25), ylim = c(0.8, 2000), expand = FALSE)
p


### Save plot
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable.svg"), p,   width = 4.5, height = 3.5)



p <- ggplot(plot_df , aes(x=spi)) %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3, aes(y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
+ geom_point(data = spi_return_df %>% filter(resolution == "daily"), aes(y = return_period_empir) , shape = 1) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 0.5)) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-3.7, 0.25), ylim = c(0.8, 2000), expand = FALSE)
p


### Save plot
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable_empir.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable_empir.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable_empir.svg"), p,   width = 4.5, height = 3.5)


p <- ggplot(plot_df , aes(x=spi)) %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3, aes(y=return_period, colour = accum_months_factor, group = accum_months_factor)) %>%
+ geom_point(data = spi_return_df %>% filter(resolution == "daily"), aes(y = return_period_empir) , shape = 1) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
#  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
#  +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x),   labels = scales::trans_format("log10", scales::math_format(10^.x))  ) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
  + coord_cartesian(xlim = c(-5.5, 1.5),  ylim = c(0.8, 1000000), expand = FALSE) %>%
  + theme_bw(8)
p

### Save plot
ggsave(file.path(write_figures_path, "return_per_daily_lmom_empir.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_empir.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_empir.svg"), p,   width = 4.5, height = 3.5)

p <- p + coord_cartesian(xlim = c(-5.5, -3.8),  ylim = c(800, 1300000), expand = FALSE)

### Save plot
ggsave(file.path(write_figures_path, "return_per_daily_lmom_upper_empir.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_upper_empir.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_upper_empir.svg"), p,   width = 4.5, height = 3.5)



p <- ggplot(plot_df , aes(x=return_period)) %>%
  + geom_hline(yintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_hline(yintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3, aes(y=spi, colour = accum_months_factor, group = accum_months_factor)) %>%
+ geom_point(data = spi_return_df %>% filter(resolution == "daily"), aes(x = return_period_empir, y = spi) , shape = 1) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
+ scale_y_continuous(name = "SPI") %>%
#   +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::comma) %>%
+ scale_x_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
+ annotation_logticks( sides = "b")  %>%
+ coord_cartesian(ylim = c(-3.7, 0.25), xlim = c(0.8, 2000), expand = FALSE)
p
### Save plot
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable_flipped.png"), p,  width = 4.5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable_flipped.pdf"), p,   width = 4.5, height = 3.5)
ggsave(file.path(write_figures_path, "return_per_daily_lmom_usable_flipped.svg"), p,   width = 4.5, height = 3.5)



###########################################################################
#  Combine the plots
###########################################################################


plot_df <- dens_df %>%
  filter(method == "lmoment" & accum_months > 0.6)

p <- ggplot(plot_df , aes(x=spi, y=return_period, colour = accum_months_factor, group = plot_group, linetype = resolution)) %>%
+ geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
+ geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
+ geom_line(size = 0.3) %>%
+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_linetype_discrete(name = "Resolution") %>%
#  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
+ scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
#  +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x),   labels = scales::trans_format("log10", scales::math_format(10^.x))  ) %>%
 + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
   + annotation_logticks( sides = "l") %>%
   + coord_cartesian(xlim = c(-3.7, 0.25), ylim = c(0.8, 2000), expand = FALSE)
 p

 ### Save plot
 ggsave(file.path(write_figures_path, "return_per_comb_lmom_usable.png"), p,  width = 4.5, height = 3.5, dpi = 600)
 ggsave(file.path(write_figures_path, "return_per_comb_lmom_usable.pdf"), p,   width = 4.5, height = 3.5)
 ggsave(file.path(write_figures_path, "return_per_comb_lmom_usable.svg"), p,   width = 4.5, height = 3.5)

 p <- ggplot(plot_df %>% filter(accum_months_factor %in% c("1", "3", "12") ), aes(x=spi, y=return_period, colour = accum_months_factor, group = plot_group, linetype = resolution)) %>%
 + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
 + geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
 + geom_line(size = 0.5) %>%
 + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
   + scale_linetype_discrete(name = "Resolution") %>%
 #  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
 + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
 #  +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x),   labels = scales::trans_format("log10", scales::math_format(10^.x))  ) %>%
  + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
    + annotation_logticks( sides = "l") %>%
    + coord_cartesian(xlim = c(-3.7, 0.25), ylim = c(0.8, 2000), expand = FALSE)
  p

   ### Save plot
   ggsave(file.path(write_figures_path, "return_per_comb_lmom_subset.png"), p,  width = 4.5, height = 3.5, dpi = 600)
   ggsave(file.path(write_figures_path, "return_per_comb_lmom_subset.pdf"), p,   width = 4.5, height = 3.5)
   ggsave(file.path(write_figures_path, "return_per_comb_lmom_subset.svg"), p,   width = 4.5, height = 3.5)




   p <- ggplot(plot_df %>% filter(accum_months_factor %in% c("1", "3", "6", "12")) , aes(x=spi, y=return_period, group = plot_group, linetype = resolution)) %>%
   + geom_vline(xintercept = spi_thresh, colour = "grey",  alpha = 0.5, size = 0.2) %>%
   + geom_vline(xintercept = 0, colour = "black",  alpha = 0.5, size = 0.2) %>%
   + geom_line(size = 0.3) %>%
   #+ scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
     + scale_linetype_discrete(name = "Resolution") %>%
   #  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
   + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
   #  +  scale_y_log10(  breaks = scales::trans_breaks("log10", function(x) 10^x),   labels = scales::trans_format("log10", scales::math_format(10^.x))  ) %>%
    + scale_y_log10(name = "Return Period (Years)", breaks = y_breaks, labels = scales::comma_format(accuracy = 1)) %>%
      + annotation_logticks( sides = "l") %>%
      + coord_cartesian(xlim = c(-3.7, 0.25), ylim = c(0.8, 2000), expand = FALSE) %>%
      + facet_grid(accum_months_factor ~. ) %>%
      + theme_bw(8) %>%
      + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())
    p

    ### Save plot
    ggsave(file.path(write_figures_path, "return_per_comb_lmom_facet.png"), p,  width = 4, height = 6, dpi = 600)
    ggsave(file.path(write_figures_path, "return_per_comb_lmom_facet.pdf"), p,   width = 4, height = 6)
    ggsave(file.path(write_figures_path, "return_per_comb_lmom_facet.svg"), p,   width = 4, height = 6)
