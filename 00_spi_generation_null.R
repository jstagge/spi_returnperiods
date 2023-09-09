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
library(evd)

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


n_folds <- 20
n_years <- 500000
#n_years <- 100
accum_list <- c(0)

run_df <- expand_grid(accum = accum_list, folds = seq(1,n_folds))

for (j in seq(1,dim(run_df)[1])){
  cat(paste0(j, " of ", dim(run_df)[1]))
  cat("\n")
tic()
  accum_j <- run_df$accum[[j]]
  fold_j <- run_df$folds[[j]]
  accum_period <- accum_j

###########################################################################
###  Create a true SPI time series
###########################################################################

spi_df <- expand_grid(year = seq(1,n_years + 10), month = seq(1,12), accum_period = accum_j, fold = fold_j)

### Simulate the SPI
n_period <- dim(spi_df)[1]

innov_c <- rep(NA, n_period)
spi <- rnorm(n_period, 0, 1)

### Add back to the dataframe
spi_df <- spi_df %>%
    mutate(innov = innov_c) %>%
    mutate(spi = c(spi)) %>%
    select(year, month, innov, spi)

###########################################################################
###  Remove the partial years and only keep n_years
###########################################################################
spi_summary <- spi_df %>%
  drop_na(spi) %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  filter(count == 12) %>%
  head(n_years)

spi_df <- spi_df %>%
  filter(year %in% spi_summary$year)

min_year <- min(spi_df$year)
spi_df$year <- spi_df$year - min_year + 1

###########################################################################
###  Extract annual
###########################################################################
ann_min_j <- spi_df %>%
  group_by(year) %>%
  summarize(ann_min = min(spi, na.rm=TRUE))

###########################################################################
###  Save Results
###########################################################################
write_output_j <- file.path(write_output_path, paste0("spi_", accum_j))
dir.create(write_output_j, recursive=TRUE, showWarnings = FALSE)
saveRDS(ann_min_j, file.path(write_output_j, paste0("ann_min_", accum_j, "_month_fold", fold_j, ".rds")))
write_csv(ann_min_j, file.path(write_output_j, paste0("ann_min_", accum_j, "_month_fold", fold_j, ".csv")))

saveRDS(spi_df, file.path(write_output_j, paste0("spi_", accum_j, "_month_fold", fold_j, ".rds")))

rm(ann_min_j)
rm(spi_df)

toc()
}
