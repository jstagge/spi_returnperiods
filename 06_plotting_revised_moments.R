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
write_output_path <- output_path
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

### To access GHCND
require(rnoaa)

require(lfstat)

### To save in SVG
require(svglite)
require(viridis)

### Packages for spi
require(fitdistrplus)
#library(evd)
require(lmomco)
require(FAdist)

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
## Raad in data
###########################################################################
### Read in the Daily data
daily_path <- file.path(write_output_path, "daily")
  moments_daily <- readRDS(file.path(daily_path, paste0("moments_df.rds")))
  l_mom_daily <- readRDS(file.path(daily_path, paste0("l_mom_df.rds")))
  ln3_daily <- readRDS(file.path(daily_path, paste0("ln3_df.rds")))
  spi_return_daily <- readRDS(file.path(daily_path, paste0("spi_return_df.rds")))
  return_spi_daily <- readRDS(file.path(daily_path, paste0("return_spi_df.rds")))
  l_mom_daily_folds <- readRDS(file.path(daily_path, paste0("l_mom_folds.rds")))

### Read in the monthly data
monthly_path <- file.path(write_output_path, "month")
    moments_monthly <- readRDS(file.path(monthly_path, paste0("moments_df.rds")))
    l_mom_monthly <- readRDS(file.path(monthly_path, paste0("l_mom_df.rds")))
    ln3_monthly <- readRDS(file.path(monthly_path, paste0("ln3_df.rds")))
    spi_return_monthly <- readRDS(file.path(monthly_path, paste0("spi_return_df.rds")))
    return_spi_monthly <- readRDS(file.path(monthly_path, paste0("return_spi_df.rds")))
  #  l_mom_monthly_folds <- readRDS(file.path(monthly_path, paste0("l_mom_folds.rds")))




###########################################################################
## Create L-moment plot
###########################################################################
lines_df <- lines_lmom()
limit_df <- limit_lmom()
points_df <- points_lmom()


plot_lines <- lines_df %>% filter(label == "pe3" | label == "gno" | label == "gev" | label ==  "glo" | label == "wei")
plot_points <- points_df %>% filter(label == "nor" | label == "gum")


my_breaks <- c(15, 30, 90, 180, 365, 730)
line_values <- c("dotdash", "F1", "solid", "longdash", "dotted", "dashed")
#line_breaks <- c()


color_list <-  turbo(8, begin = 0.05, end = 0.95)
plot_df <- l_mom_daily %>%
  mutate(accum_period = factor(accum_period))

p <- ggplot(plot_lines, aes(x=V1, y=V2)) %>%
  + geom_line(aes(group = label, linetype = label)) %>%
  + geom_point(data = plot_points, aes(shape = label), size = 3, colour = "grey40", fill = "grey40") %>%
  + geom_line(data = limit_df, colour = "black") %>%
  + geom_point(data = plot_df, aes(x=t_3, y=t_4, colour = accum_period), size = 4) %>%
  + scale_colour_manual(name = "Accum Period\n(Days)", values = color_list) %>%
  #+ scale_colour_viridis(name = "Accum Period\n(Days)", breaks = my_breaks, labels = my_breaks, trans = scales::pseudo_log_trans(sigma = 0.001), option = "turbo") %>%
  + scale_shape_manual(name = "", values = c(22, 24)) %>%
  + scale_linetype_manual(name = "Distribution", values = line_values) %>%
  + coord_fixed(xlim = c(0,0.17), ylim = c(0.06, 0.19)) %>%
  + scale_x_continuous(name = "L-skewness") %>%
  + scale_y_continuous(name = "L-kurtosis") %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "lmom_combined_daily.png"), p_nolegend,  width = 5, height = 4, dpi = 600)
ggsave(file.path(write_figures_path, "lmom_combined_daily.pdf"), p_nolegend,  width = 5, height = 4)
ggsave(file.path(write_figures_path, "lmom_combined_daily.svg"), p_nolegend,  width = 5, height = 4)

ggsave(file.path(write_figures_path, "lmom_combined_daily_legend.pdf"), legend,  width = 2.5, height = 8)
ggsave(file.path(write_figures_path, "lmom_combined_daily_legend.svg"), legend,  width = 2.5, height = 8)




plot_df <- l_mom_daily_folds %>%
  mutate(accum_period = factor(accum_period))

p <- ggplot(plot_lines, aes(x=V1, y=V2)) %>%
  + geom_line(aes(group = label, linetype = label)) %>%
  + geom_point(data = plot_points, aes(shape = label), size = 3, colour = "grey40", fill = "grey40") %>%
  + geom_line(data = limit_df, colour = "black") %>%
  + geom_point(data = plot_df, aes(x=t_3, y=t_4, colour = accum_period), size = 3, shape = 3) %>%
  + scale_colour_manual(name = "Accum Period\n(Days)", values = color_list) %>%
  #+ scale_colour_viridis(name = "Accum Period\n(Days)", breaks = my_breaks, labels = my_breaks, trans = scales::pseudo_log_trans(sigma = 0.001), option = "turbo") %>%
  + scale_shape_manual(name = "", values = c(22, 24)) %>%
  + scale_linetype_manual(name = "Distribution", values = line_values) %>%
  + coord_fixed(xlim = c(0,0.17), ylim = c(0.06, 0.19)) %>%
#    + coord_fixed(xlim = c(0,0.085), ylim = c(0.1, 0.15)) %>%
  + scale_x_continuous(name = "L-skewness") %>%
  + scale_y_continuous(name = "L-kurtosis") %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")

### Save plot
ggsave(file.path(write_figures_path, "lmom_combined_daily_folds.png"), p_nolegend,  width = 5, height = 4, dpi = 600)
ggsave(file.path(write_figures_path, "lmom_combined_daily_folds.pdf"), p_nolegend,  width = 5, height = 4)
ggsave(file.path(write_figures_path, "lmom_combined_daily_folds.svg"), p_nolegend,  width = 5, height = 4)




#my_breaks <- c(1, 3, 6, 12, 24)
plot_df <- l_mom_monthly %>%
  filter(accum_period > 0) %>%
  mutate(accum_period = factor(accum_period))

p <- ggplot(plot_lines, aes(x=V1, y=V2)) %>%
  + geom_line(aes(group = label, linetype = label)) %>%
  + geom_point(data = plot_points, aes(shape = label), size = 3, colour = "grey40", fill = "grey40") %>%
  + geom_line(data = limit_df, colour = "black") %>%
  + geom_point(data = plot_df, aes(x=t_3, y=t_4, colour = accum_period), size = 4) %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list[seq(2,8)]) %>%
#  + scale_colour_viridis(name = "Accum Period\n(Months)", breaks = my_breaks, labels = my_breaks, trans = scales::pseudo_log_trans(sigma = 1), option = "turbo") %>%
  + scale_shape_manual(name = "", values = c(22, 24)) %>%
  + scale_linetype_manual(name = "Distribution", values = line_values) %>%
  + coord_fixed(xlim = c(0,0.17), ylim = c(0.06, 0.19)) %>%
  + scale_x_continuous(name = "L-skewness") %>%
  + scale_y_continuous(name = "L-kurtosis") %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
p_nolegend

legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "lmom_combined_monthly.png"), p_nolegend,  width = 5, height = 4, dpi = 600)
ggsave(file.path(write_figures_path, "lmom_combined_monthly.pdf"), p_nolegend,  width = 5, height = 4)
ggsave(file.path(write_figures_path, "lmom_combined_monthly.svg"), p_nolegend,  width = 5, height = 4)

ggsave(file.path(write_figures_path, "lmom_combined_monthly_legend.pdf"), legend,  width = 2.5, height = 8)
ggsave(file.path(write_figures_path, "lmom_combined_monthly_legend.svg"), legend,  width = 2.5, height = 8)



  l_mom_daily <- l_mom_daily  %>%
    filter(accum_period > 20) %>%
    mutate(resolution = "daily")  %>%
    mutate(accum_months = round(accum_period/30))

  l_mom_monthly <- l_mom_monthly %>%
    mutate(resolution = "monthly") %>%
    mutate(accum_months = accum_period)

plot_df <- l_mom_daily %>%
  bind_rows(l_mom_monthly) %>%
  mutate(accum_months = factor(accum_months))  %>%
  mutate(resolution = factor(resolution))


  p <- ggplot(plot_lines, aes(x=V1, y=V2)) %>%
    + geom_line(aes(group = label, linetype = label)) %>%
  #  + geom_point(data = plot_points, aes(shape = label), size = 3, colour = "grey40", fill = "grey40") %>%
    + geom_line(data = limit_df, colour = "black") %>%
    + geom_point(data = plot_df, aes(x=t_3, y=t_4, colour = accum_months, shape = resolution ), size = 4) %>%
    + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list) %>%
  #  + scale_colour_viridis(name = "Accum Period\n(Months)", breaks = my_breaks, labels = my_breaks, trans = scales::pseudo_log_trans(sigma = 1), option = "turbo") %>%
    + scale_shape_manual(name = "", values = c(22, 24)) %>%
    + scale_linetype_manual(name = "Distribution", values = line_values) %>%
    + coord_fixed(xlim = c(0,0.17), ylim = c(0.06, 0.19)) %>%
    + scale_x_continuous(name = "L-skewness") %>%
    + scale_y_continuous(name = "L-kurtosis") %>%
    + theme_classic(12)

  p

  p_nolegend <- p + theme(legend.position="none")
  legend <- cowplot::get_legend(p + theme_classic(14))


  ### Save plot
  ggsave(file.path(write_figures_path, "lmom_combined.png"), p_nolegend,  width = 5, height = 4, dpi = 600)
  ggsave(file.path(write_figures_path, "lmom_combined.pdf"), p_nolegend,  width = 5, height = 4)
  ggsave(file.path(write_figures_path, "lmom_combined.svg"), p_nolegend,  width = 5, height = 4)



  ggsave(file.path(write_figures_path, "lmom_combined_legend.pdf"), legend,  width = 2.5, height = 8)
  ggsave(file.path(write_figures_path, "lmom_combined_legend.svg"), legend,  width = 2.5, height = 8)
