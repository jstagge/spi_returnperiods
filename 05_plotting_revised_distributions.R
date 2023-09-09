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
## Process the data files
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


  ###########################################################################
  ## Plot distribution parameters
  ###########################################################################

p <- ggplot(ln3_df , aes(x=accum_months, y=zeta, colour = resolution, shape = method, linetype = method)) %>%
  + geom_point() %>%
  + geom_line() %>%
  + scale_shape_manual(name = "Method", values =c(1,19), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  #+ scale_colour_manual(values = c("black", "grey70")) %>%
  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
  + scale_linetype_manual(name = "Method", values = c("dashed", "solid"), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  + scale_x_continuous(name = "Accum Period (Months)") %>%
  + scale_y_continuous(name = "Zeta Parameter") %>%
  + theme_classic(12) %>%
  + theme(legend.position="bottom")
#  + scale_colour_brewer(type = "qual", palette = "Set1")

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "zeta_param.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
ggsave(file.path(write_figures_path, "zeta_param.pdf"), p_nolegend,   width = 3, height = 2.5)
ggsave(file.path(write_figures_path, "zeta_param.svg"), p_nolegend,   width = 3, height = 2.5)

ggsave(file.path(write_figures_path, "param_legend.pdf"), legend,  width = 5, height = 2.5)
ggsave(file.path(write_figures_path, "param_legend.svg"), legend,  width = 5, height = 2.5)


p <- ggplot(ln3_df , aes(x=accum_months, y=mulog, colour = resolution, shape = method, linetype = method)) %>%
  + geom_point() %>%
  + geom_line() %>%
  + scale_shape_manual(name = "Method", values =c(1,19), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  #+ scale_colour_manual(values = c("black", "grey70")) %>%
  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
  + scale_linetype_manual(name = "Method", values = c("dashed", "solid"), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  + scale_x_continuous(name = "Accum Period (Months)") %>%
  + scale_y_continuous(name = "Mu log Parameter") %>%
  + theme_classic(12) %>%
  + theme(legend.position="bottom")
#  + scale_colour_brewer(type = "qual", palette = "Set1")

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "mulog_param.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
ggsave(file.path(write_figures_path, "mulog_param.pdf"), p_nolegend,   width = 3, height = 2.5)
ggsave(file.path(write_figures_path, "mulog_param.svg"), p_nolegend,   width = 3, height = 2.5)


p <- ggplot(ln3_df , aes(x=accum_months, y=sigmalog, colour = resolution, shape = method, linetype = method)) %>%
  + geom_point() %>%
  + geom_line() %>%
  + scale_shape_manual(name = "Method", values =c(1,19), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  #+ scale_colour_manual(values = c("black", "grey70")) %>%
  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
  + scale_linetype_manual(name = "Method", values = c("dashed", "solid"), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  + scale_x_continuous(name = "Accum Period (Months)") %>%
  + scale_y_continuous(name = "Sigma log Parameter") %>%
  + theme_classic(12) %>%
  + theme(legend.position="bottom")
#  + scale_colour_brewer(type = "qual", palette = "Set1")

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "sigmalog_param.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
ggsave(file.path(write_figures_path, "sigmalog_param.pdf"), p_nolegend,   width = 3, height = 2.5)
ggsave(file.path(write_figures_path, "sigmalog_param.svg"), p_nolegend,   width = 3, height = 2.5)



p <- ggplot(ln3_df , aes(x=accum_months, y=loc, colour = resolution, shape = method, linetype = method)) %>%
  + geom_point() %>%
  + geom_line() %>%
  + scale_shape_manual(name = "Method", values =c(1,19), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  #+ scale_colour_manual(values = c("black", "grey70")) %>%
  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
  + scale_linetype_manual(name = "Method", values = c("dashed", "solid"), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  + scale_x_continuous(name = "Accum Period (Months)") %>%
  + scale_y_continuous(name = "Location Parameter") %>%
#  + facet_grid(.~method) %>%
  + theme_classic(12) %>%
  + theme(legend.position="bottom")
#  + scale_colour_brewer(type = "qual", palette = "Set1")

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "loc_param.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
ggsave(file.path(write_figures_path, "loc_param.pdf"), p_nolegend,   width = 3, height = 2.5)
ggsave(file.path(write_figures_path, "loc_param.svg"), p_nolegend,   width = 3, height = 2.5)



p <- ggplot(ln3_df , aes(x=accum_months, y=scale, colour = resolution, shape = method, linetype = method)) %>%
  + geom_point() %>%
  + geom_line() %>%
  + scale_shape_manual(name = "Method", values =c(1,19), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  #+ scale_colour_manual(values = c("black", "grey70")) %>%
  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
  + scale_linetype_manual(name = "Method", values = c("dashed", "solid"), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  + scale_x_continuous(name = "Accum Period (Months)") %>%
  + scale_y_continuous(name = "Scale Parameter") %>%
  + theme_classic(12) %>%
  + theme(legend.position="bottom")
#  + scale_colour_brewer(type = "qual", palette = "Set1")

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "scale_param.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
ggsave(file.path(write_figures_path, "scale_param.pdf"), p_nolegend,   width = 3, height = 2.5)
ggsave(file.path(write_figures_path, "scale_param.svg"), p_nolegend,   width = 3, height = 2.5)



p <- ggplot(ln3_df , aes(x=accum_months, y=shape, colour = resolution, shape = method, linetype = method)) %>%
  + geom_point() %>%
  + geom_line() %>%
  + scale_shape_manual(name = "Method", values =c(1,19), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  #+ scale_colour_manual(values = c("black", "grey70")) %>%
  + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
  + scale_linetype_manual(name = "Method", values = c("dashed", "solid"), limits = c("lmoment", "mle"), labels = c("L-Moment", "MLE")) %>%
  + scale_x_continuous(name = "Accum Period (Months)") %>%
  + scale_y_continuous(name = "Shape Parameter") %>%
  + theme_classic(12) %>%
  + theme(legend.position="bottom")
#  + scale_colour_brewer(type = "qual", palette = "Set1")

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "shape_param.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
ggsave(file.path(write_figures_path, "shape_param.pdf"), p_nolegend,   width = 3, height = 2.5)
ggsave(file.path(write_figures_path, "shape_param.svg"), p_nolegend,   width = 3, height = 2.5)



###########################################################################
## Prepare Moments
###########################################################################
moments_daily <- moments_daily %>%
  mutate(resolution = "daily")  %>%
  mutate(accum_months = accum_period/30)

moments_monthly <- moments_monthly %>%
  mutate(resolution = "monthly") %>%
  mutate(accum_months = accum_period) %>%
  filter(accum_period > 0)

moments_df <- moments_daily %>%
  bind_rows(moments_monthly)%>%
  filter(accum_months > 0)

moments_df <- moments_df %>%
      mutate(accum_months_factor = round(accum_months)) %>%
      mutate(accum_months_factor = factor(accum_months_factor)) %>%
      mutate(resolution = factor(resolution)) %>%
      mutate(plot_group = paste0(resolution, accum_period))


###########################################################################
## Plot Moments
###########################################################################

p <- ggplot(moments_df , aes(x=accum_months, y=mean, colour = resolution)) %>%
    + geom_point() %>%
    + geom_line() %>%
      + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
    + scale_x_continuous(name = "Accum Period (Months)") %>%
    + scale_y_continuous(name = "Mean") %>%
    + theme_classic(12) %>%
    + theme(legend.position="bottom")
  #  + scale_colour_brewer(type = "qual", palette = "Set1")

  p

  p_nolegend <- p + theme(legend.position="none")
  legend <- cowplot::get_legend(p + theme_classic(14))


  ### Save plot
  ggsave(file.path(write_figures_path, "moment_mean.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
  ggsave(file.path(write_figures_path, "moment_mean.pdf"), p_nolegend,   width = 3, height = 2.5)
  ggsave(file.path(write_figures_path, "moment_mean.svg"), p_nolegend,   width = 3, height = 2.5)


  p <- ggplot(moments_df , aes(x=accum_months, y=var, colour = resolution)) %>%
      + geom_point() %>%
      + geom_line() %>%
        + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
      + scale_x_continuous(name = "Accum Period (Months)") %>%
      + scale_y_continuous(name = "Variance") %>%
      + theme_classic(12) %>%
      + theme(legend.position="bottom")
    #  + scale_colour_brewer(type = "qual", palette = "Set1")

    p

    p_nolegend <- p + theme(legend.position="none")
    legend <- cowplot::get_legend(p + theme_classic(14))


    ### Save plot
    ggsave(file.path(write_figures_path, "moment_var.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
    ggsave(file.path(write_figures_path, "moment_var.pdf"), p_nolegend,   width = 3, height = 2.5)
    ggsave(file.path(write_figures_path, "moment_var.svg"), p_nolegend,   width = 3, height = 2.5)


    p <- ggplot(moments_df , aes(x=accum_months, y=sd, colour = resolution)) %>%
        + geom_point() %>%
        + geom_line() %>%
          + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
        + scale_x_continuous(name = "Accum Period (Months)") %>%
        + scale_y_continuous(name = "Std Dev") %>%
        + theme_classic(12) %>%
        + theme(legend.position="bottom")
      #  + scale_colour_brewer(type = "qual", palette = "Set1")

      p

      p_nolegend <- p + theme(legend.position="none")
      legend <- cowplot::get_legend(p + theme_classic(14))


      ### Save plot
      ggsave(file.path(write_figures_path, "moment_sd.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
      ggsave(file.path(write_figures_path, "moment_sd.pdf"), p_nolegend,   width = 3, height = 2.5)
      ggsave(file.path(write_figures_path, "moment_sd.svg"), p_nolegend,   width = 3, height = 2.5)


      p <- ggplot(moments_df , aes(x=accum_months, y=skewness, colour = resolution)) %>%
          + geom_point() %>%
          + geom_line() %>%
            + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
          + scale_x_continuous(name = "Accum Period (Months)") %>%
          + scale_y_continuous(name = "Skewness") %>%
          + theme_classic(12) %>%
          + theme(legend.position="bottom")
        #  + scale_colour_brewer(type = "qual", palette = "Set1")

        p

        p_nolegend <- p + theme(legend.position="none")
        legend <- cowplot::get_legend(p + theme_classic(14))


        ### Save plot
        ggsave(file.path(write_figures_path, "moment_skew.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
        ggsave(file.path(write_figures_path, "moment_skew.pdf"), p_nolegend,   width = 3, height = 2.5)
        ggsave(file.path(write_figures_path, "moment_skew.svg"), p_nolegend,   width = 3, height = 2.5)

        p <- ggplot(moments_df , aes(x=accum_months, y=kurtosis, colour = resolution)) %>%
            + geom_point() %>%
            + geom_line() %>%
              + scale_colour_brewer(name = "Resolution", type = "qual", palette = "Set1", limits = c("daily", "monthly"), labels = c("Daily", "Monthly")) %>%
            + scale_x_continuous(name = "Accum Period (Months)") %>%
            + scale_y_continuous(name = "Kurtosis") %>%
            + theme_classic(12) %>%
            + theme(legend.position="bottom")
          #  + scale_colour_brewer(type = "qual", palette = "Set1")

          p

          p_nolegend <- p + theme(legend.position="none")
          legend <- cowplot::get_legend(p + theme_classic(14))

          ### Save plot
          ggsave(file.path(write_figures_path, "moment_kurtosis.png"), p_nolegend,  width = 3, height = 2.5, dpi = 600)
          ggsave(file.path(write_figures_path, "moment_kurtosis.pdf"), p_nolegend,   width = 3, height = 2.5)
          ggsave(file.path(write_figures_path, "moment_kurtosis.svg"), p_nolegend,   width = 3, height = 2.5)





###########################################################################
## Calculate Density
###########################################################################
dens_df <- expand_grid(data.frame(x=seq(-2.5,5,by = 0.05)), ln3_df) %>%
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

###########################################################################
## Plot distributions
###########################################################################
plot_df <- dens_df %>% filter(method == "lmoment" & resolution == "Daily")
p <- ggplot(plot_df, aes(x=spi, y=dens, colour = accum_months_factor, group = accum_months_factor))  %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "dens_daily_lmom.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "dens_daily_lmom.pdf"), p_nolegend,   width = 4, height = 3)
ggsave(file.path(write_figures_path, "dens_daily_lmom.svg"), p_nolegend,   width = 4, height = 3)

ggsave(file.path(write_figures_path, "density_legend.pdf"), legend,  width = 5, height = 5)
ggsave(file.path(write_figures_path, "density_legend.svg"), legend,  width = 5, height = 5)






# linetype = "dotted",
p <- ggplot(plot_df, aes(x=spi, y=dens, colour = accum_months_factor, group = accum_months_factor))  %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey35",  alpha = 0.9, linewidth = 0.2, linetype = "dashed") %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "dens_daily_lmom_thresh.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "dens_daily_lmom_thresh.pdf"), p_nolegend,   width =4, height = 3)
ggsave(file.path(write_figures_path, "dens_daily_lmom_thresh.svg"), p_nolegend,   width =4, height = 3)


p <- ggplot(plot_df, aes(x=spi, y=cum_dens, colour = accum_months_factor, group = accum_months_factor))  %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Cumulative Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom.png"), p_nolegend,  width = 5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom.pdf"), p_nolegend,   width =5, height = 3.5)
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom.svg"), p_nolegend,   width =5, height = 3.5)


p <- ggplot(plot_df, aes(x=spi, y=cum_dens, colour = accum_months_factor, group = accum_months_factor))  %>%
  + geom_vline(xintercept = spi_thresh,  colour = "grey35",  alpha = 0.9, linewidth = 0.2, linetype = "dashed") %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Cumulative Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom_thresh.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom_thresh.pdf"), p_nolegend,   width =4, height = 3)
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom_thresh.svg"), p_nolegend,   width =4, height = 3)

p <- ggplot(plot_df, aes(x=spi, y=cum_dens))  %>%
  + geom_vline(xintercept = spi_thresh,  colour = "grey35",  alpha = 0.9, linewidth = 0.2, linetype = "dashed") %>%
  + geom_line(aes( colour = accum_months_factor, group = accum_months_factor)) %>%
  + geom_point(data = spi_return_daily , aes(x = spi, y = 1/return_period_empir), shape = 1) %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Cumulative Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom_empirpoints.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom_empirpoints.pdf"), p_nolegend,   width =4, height = 3)
ggsave(file.path(write_figures_path, "cum_dens_daily_lmom_empirpoints.svg"), p_nolegend,   width =4, height = 3)



###############################
#### Monthly distributions
###############################
plot_df <- dens_df %>% filter(method == "lmoment" & resolution == "Monthly")
p <- ggplot(plot_df, aes(x=spi, y=dens, colour = accum_months_factor, group = accum_months_factor))  %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "dens_monthly_lmom.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "dens_monthly_lmom.pdf"), p_nolegend,   width =4, height = 3)
ggsave(file.path(write_figures_path, "dens_monthly_lmom.svg"), p_nolegend,   width =4, height = 3)

ggsave(file.path(write_figures_path, "density_legend_monthly.pdf"), legend,  width = 5, height = 5)
ggsave(file.path(write_figures_path, "density_legend_monthly.svg"), legend,  width = 5, height = 5)



# linetype = "dotted",
p <- ggplot(plot_df, aes(x=spi, y=dens, colour = accum_months_factor, group = accum_months_factor))  %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey35",  alpha = 0.9, linewidth = 0.2, linetype = "dashed") %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5))  %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "dens_monthly_lmom_thresh.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "dens_monthly_lmom_thresh.pdf"), p_nolegend,  width = 4, height = 3)
ggsave(file.path(write_figures_path, "dens_monthly_lmom_thresh.svg"), p_nolegend,  width = 4, height = 3)


p <- ggplot(plot_df, aes(x=spi, y=cum_dens, colour = accum_months_factor, group = accum_months_factor))  %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Cumulative Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5))  %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))

### Save plot
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom.pdf"), p_nolegend,  width = 4, height = 3)
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom.svg"), p_nolegend,  width = 4, height = 3)


p <- ggplot(plot_df, aes(x=spi, y=cum_dens, colour = accum_months_factor, group = accum_months_factor))  %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey35",  alpha = 0.9, linewidth = 0.2, linetype = "dashed") %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Cumulative Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom_thresh.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom_thresh.pdf"), p_nolegend,  width = 4, height = 3)
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom_thresh.svg"), p_nolegend,  width = 4, height = 3)


p <- ggplot(plot_df, aes(x=spi, y=cum_dens))  %>%
  + geom_vline(xintercept = spi_thresh, colour = "grey35",  alpha = 0.9, linewidth = 0.2, linetype = "dashed") %>%
  + geom_line(aes( colour = accum_months_factor, group = accum_months_factor)) %>%
  + geom_point(data = spi_return_monthly , aes(x = spi, y = 1/return_period_empir), shape = 1) %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Cumulative Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + theme_classic(12)

p

p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom_empirpoints.png"), p_nolegend,  width = 4, height = 3, dpi = 600)
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom_empirpoints.pdf"), p_nolegend,  width = 4, height = 3)
ggsave(file.path(write_figures_path, "cum_dens_monthly_lmom_empirpoints.svg"), p_nolegend,  width = 4, height = 3)


###############################
#### Combined distributions
###############################

plot_df <- dens_df %>%
  filter(method == "lmoment")

p <- ggplot(plot_df %>% filter(accum_months > 0.6), aes(x=spi, y=dens, colour = accum_months_factor, group = plot_group, linetype = resolution))  %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_linetype_discrete(name = "Resolution") %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) #%>%
  #+ theme_bw(8)

p
### Save plot
ggsave(file.path(write_figures_path, "dens_comb_lmom.png"), p,  width = 5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "dens_comb_lmom.pdf"), p,   width =5, height = 3.5)
ggsave(file.path(write_figures_path, "dens_comb_lmom.svg"), p,   width =5, height = 3.5)


p <- ggplot(plot_df %>% filter(accum_months_factor %in% c("1", "3", "12")), aes(x=spi, y=dens, colour = accum_months_factor, group = plot_group, linetype = resolution))  %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_linetype_discrete(name = "Resolution") %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) #%>%
  #+ theme_bw(8)

p
### Save plot
ggsave(file.path(write_figures_path, "dens_comb_lmom_subset.png"), p,  width = 5, height = 3.5, dpi = 600)
ggsave(file.path(write_figures_path, "dens_comb_lmom_subset.pdf"), p,   width =5, height = 3.5)
ggsave(file.path(write_figures_path, "dens_comb_lmom_subset.svg"), p,   width =5, height = 3.5)



p <- ggplot(plot_df , aes(x=spi, y=dens, colour = accum_months_factor, group = plot_group))  %>%
  + geom_vline(xintercept = spi_thresh,  colour = "grey35",  alpha = 0.9, linewidth = 0.2, linetype = "dashed") %>%
  + geom_line() %>%
  + scale_colour_manual(name = "Accum Period\n(Months)", values = color_list, breaks = color_levels) %>%
  + scale_x_continuous(name = "SPI", breaks = seq(-5, 5, by = 1)) %>%
  + scale_y_continuous(name = "Density") %>%
  + coord_cartesian(xlim = c(-5, 1.5)) %>%
  + facet_grid(resolution~.) %>%
  + theme_bw(12) %>%
  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p


p_nolegend <- p + theme(legend.position="none")
legend <- cowplot::get_legend(p + theme_classic(14))


### Save plot
ggsave(file.path(write_figures_path, "dens_facet_lmom.png"), p_nolegend,  width = 5, height = 4, dpi = 600)
ggsave(file.path(write_figures_path, "dens_facet_lmom.pdf"), p_nolegend,   width =5, height = 4)
ggsave(file.path(write_figures_path, "dens_facet_lmom.svg"), p_nolegend,   width =5, height = 4)


ggsave(file.path(write_figures_path, "dens_facet_legend.pdf"), legend,   width =5, height = 6)
ggsave(file.path(write_figures_path, "dens_facet_legend.svg"), legend,   width =5, height = 6)
