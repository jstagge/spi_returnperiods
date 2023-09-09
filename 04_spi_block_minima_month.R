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
output_path <- file.path(here(), "output/month")

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



n_years <- 500000
#n_years <- 100
accum_list <- c(0, 1, 2, 3, 6, 9, 12, 24)
n_accum <- length(accum_list)

return_spi_temp <- data.frame(return_period = c(2, 5, 10, 20, 50, 100, 500, 1000, 5000)) %>%
  mutate(p_exceed = 1/return_period)

spi_return_temp <- data.frame(spi = seq(-5, -0.5, 0.5))


for (j in seq(1,n_accum)){

  accum_j <- accum_list[[j]]

  ###########################################################################
  ###  Load Results
  ###########################################################################
  spi_folder <- file.path(write_output_path, paste0("spi_", accum_j))

  ### Read in
  for (k in seq(1,20)){
    ann_min_temp <- readRDS(file.path(spi_folder, paste0("ann_min_", accum_j, "_month_fold",k, ".rds")))
    ann_min_temp <- ann_min_temp %>%
      mutate(fold = k) %>%
      mutate(unique_id = paste0(fold, "_", year))

    if(k ==1){
      ann_min_j <- ann_min_temp
    } else {
      ann_min_j <- ann_min_j %>%
        bind_rows(ann_min_temp)
    }
  }

  head(ann_min_j)
  rm(ann_min_temp)

  ###########################################################################
  ###  Process the empirical
  ###########################################################################
  ### Process the empirical estimates
  return_spi_j <- return_spi_temp %>%
    mutate(spi_empir = quantile(ann_min_j$ann_min, p_exceed)) %>%
    mutate(accum_period = accum_j)

    ### You might need to do plotting position instead
  spi_return_j <- spi_return_temp
  spi_return_j$p_empir <- NA
  for (k in seq(1, dim(spi_return_j)[1])){
    spi_return_j$p_empir[[k]] <- mean(ann_min_j$ann_min <= spi_return_j$spi[[k]])
  }

  spi_return_j <- spi_return_j %>%
      mutate(return_period_empir = 1/p_empir) %>%
      mutate(accum_period = accum_j)

    ###########################################################################
    ## Calculate moments
    ###########################################################################
    ### Calculate moments
  moments_j <- data.frame(accum_period = accum_j) %>%
      mutate(mean = mean(ann_min_j$ann_min)) %>%
      mutate(var = var(ann_min_j$ann_min)) %>%
      mutate(sd = sd(ann_min_j$ann_min)) %>%
      mutate(skewness =  e1071::skewness(ann_min_j$ann_min)) %>%
      mutate(kurtosis =  e1071::kurtosis(ann_min_j$ann_min))

      ###########################################################################
      ## Calculate L moments and estimate parameters
      ###########################################################################
      ### Calculate the sample L moments
      sample_mom <- lmoms(0-ann_min_j$ann_min)
      sample_mom

      ### Create a zoomed in version
      png(file.path(write_figures_path, paste0("lmom_zoom_spi_", accum_j, ".png")), width = 5, height = 5, units = "in", res = 600)
      plotlmrdia(lmrdia(), autolegend=TRUE, xleg=0.02, yleg=.16, xlim=c(0,.1), ylim=c(0.08,0.16), noaep4 = TRUE, noglo = TRUE, noray = TRUE, nouni =TRUE, noexp = TRUE, nogum=TRUE, nogpa = TRUE, nogov=TRUE)
      points(sample_mom$ratios[3], sample_mom$ratios[4], col = "black", pch = 19, cex = 1.5)
      dev.off()

      ### Create a zoomed in version
      png(file.path(write_figures_path, paste0("lmom_spi_", accum_j, ".png")), width = 5, height = 5, units = "in", res = 600)
      plotlmrdia(lmrdia(), autolegend=TRUE, xleg=0, yleg=.41, xlim=c(-.1,.5), ylim=c(-.1,.4))
      # Follow up by plotting the {t3,t4} values and the mean of these.
      points(sample_mom$ratios[3], sample_mom$ratios[4], col = "black", pch = 19, cex = 1.5)
      dev.off()


      ### Combine into a dataframe
      l_mom_j <- data.frame(accum_period = accum_j, Moment = c("l_1", "l_2", "l_3", "l_4", "t_3", "t_4")) %>%
        	mutate(Population = c(sample_mom$lambdas[1:4], sample_mom$ratios[3:4]))	%>%
          pivot_wider(names_from = "Moment", values_from = "Population")

      ###########################################################################
      ## Estimate parameters for GNO (3 parameter log normal distribution)
      ###########################################################################

      ### Estimate parameters
      #pe3 <- parpe3(sample_mom)
      #gno <- pargno(sample_mom)   #Generalized Normal (Log-Normal3) distribution
      ### doesn't look like the right distribution
      #gev <- pargev(sample_mom)
      ln3_lmom <- parln3(sample_mom)

      start <- list(shape = 0.5, scale = ln3_lmom$para[[2]], thres = ln3_lmom$para[[1]])
      #fit_gno <- fitdist(0-ann_min_j$ann_min, "gno", start = start)
      ln3_mle <- fitdist(0-ann_min_j$ann_min,dlnorm3,start= start)

      ### Plot distribution
      png(file.path(write_figures_path, paste0("ln3_spi_", accum_j, ".png")), width = 10, height = 10, units = "in", res = 600)
        plot(ln3_mle)
      dev.off()


      ###########################################################################
      ###  Fit the Tukey GH
      ###########################################################################

    #  start <- list(a = 1,  b = 0.6, g = 0, h = 0.5)
    #  fit_tgh <- fitdist(0-ann_min_j$ann_min, "gh", start = start)

      ### Plot distribution
    #  png(file.path(write_figures_path, paste0("tgh_spi_", accum_j, ".png")), width = 10, height = 10, units = "in", res = 600)
    #    plot(fit_tgh)
    #  dev.off()

#      fit_tgh <- fitGH(0-ann_min_j$ann_min, method = "mle")
#      summary(fit_tgh)

      ###########################################################################
      ###  Save models
      ###########################################################################
      saveRDS(ln3_lmom, file.path(write_output_path, paste0("spi_", accum_j, "_ln3_lmom.rds")))
      saveRDS(ln3_mle, file.path(write_output_path, paste0("spi_", accum_j, "_ln3_mle.rds")))

      ###########################################################################
      ###  Extract parameters
      ###########################################################################
      ln3_j <- data.frame(accum_period = accum_j, zeta = ln3_mle$estimate[[3]], mulog = ln3_mle$estimate[[2]], sigmalog = ln3_mle$estimate[[1]], method = "mle") %>%
        bind_rows(data.frame(accum_period = accum_j, zeta =  ln3_lmom$para[[1]], mulog =  ln3_lmom$para[[2]], sigmalog =  ln3_lmom$para[[3]], method = "lmoment"))

  ###########################################################################
  ###  Add in from GNO
  ###########################################################################
  ### Zeta = thresh
  ### mulog = scale
  ### sigmalog = shape
  return_spi_j <- return_spi_j %>%
    mutate(spi_ln3_mle = -qlnorm3(1-p_exceed, shape = ln3_mle$estimate[[1]], scale = ln3_mle$estimate[[2]], thres = ln3_mle$estimate[[3]])) %>%
    mutate(spi_ln3_lmom = -qlnorm3(1-p_exceed, shape = ln3_lmom$para[[3]], scale = ln3_lmom$para[[2]], thres = ln3_lmom$para[[1]])) %>%
    select(accum_period, return_period, p_exceed, spi_empir, spi_ln3_mle, )

  spi_return_j <- spi_return_j %>%
    mutate(p_ln3_mle = 1-plnorm3(-spi, shape = ln3_mle$estimate[[1]], scale = ln3_mle$estimate[[2]], thres = ln3_mle$estimate[[3]])) %>%
    mutate(p_ln3_lmom = 1-plnorm3(-spi, shape = ln3_lmom$para[[3]], scale = ln3_lmom$para[[2]], thres = ln3_lmom$para[[1]])) %>%
    mutate(return_period_mle = 1/p_ln3_mle) %>%
    mutate(return_period_lmom = 1/p_ln3_lmom) %>%
    select(accum_period, spi, p_ln3_mle,  p_ln3_lmom, return_period_empir,return_period_mle, return_period_lmom)


    ###########################################################################
    ###  Save results
    ###########################################################################
    if(j ==1) {
        return_spi_df <- return_spi_j
        spi_return_df <- spi_return_j
        ln3_df <- ln3_j
        l_mom_df <- l_mom_j
        moments_df <- moments_j
    } else {
      return_spi_df <- return_spi_df %>%
        bind_rows(return_spi_j )

        spi_return_df <- spi_return_df %>%
          bind_rows(spi_return_j )

      ln3_df <- ln3_df %>%
        bind_rows(ln3_j  )

      l_mom_df <- l_mom_df %>%
          bind_rows(l_mom_j)

      moments_df <- moments_df %>%
          bind_rows(moments_j)
    }

    saveRDS(moments_df, file.path(write_output_path, paste0("moments_df.rds")))
    saveRDS(l_mom_df, file.path(write_output_path, paste0("l_mom_df.rds")))
    saveRDS(ln3_df, file.path(write_output_path, paste0("ln3_df.rds")))
    saveRDS(spi_return_df, file.path(write_output_path, paste0("spi_return_df.rds")))
    saveRDS(return_spi_df, file.path(write_output_path, paste0("return_spi_df.rds")))

    write_csv(moments_df, file.path(write_output_path, paste0("moments_df.csv")))
    write_csv(l_mom_df, file.path(write_output_path, paste0("l_mom_df.csv")))
    write_csv(ln3_df, file.path(write_output_path, paste0("ln3_df.csv")))
    write_csv(spi_return_df, file.path(write_output_path, paste0("spi_return_df.csv")))
    write_csv(return_spi_df, file.path(write_output_path, paste0("return_spi_df.csv")))
}
