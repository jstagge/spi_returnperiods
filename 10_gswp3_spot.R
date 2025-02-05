
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
#data_path <- file.path(here(), "data")
data_path <- "/fs/ess/PAS1921/gswp3"
function_path <- file.path(here(), "functions")
#output_path <- "/fs/ess/PAS1921"
output_path <- file.path(here(), "output")

### Set up output folders
write_output_path <- file.path(output_path, "gswp3_persist/pr")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)

### Set up figure folder
write_figures_path <- file.path(output_path, "figures/site")
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

require(zoo)

require(ncdf4)


### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

theme_set(theme_bw(12))

###########################################################################
###  Load in the GSWP3 data
###########################################################################
#cru_folder <- "F:/cru"
#gswp_folder <- "/fs/ess/PAS1921/CRUTS"
#gswp_folder <- file.path(data_path, "gswp3")
gswp_folder <- data_path
gswp_file <- file.path(gswp_folder, "pr_gswp3_1901_2010.nc")

### Open the netcdf file
cat("before first ncopen")
pm_gswp<- nc_open(gswp_file)
cat("after first ncopen")

### Create a list of the lat and lon for each grid cell
lat_list <- ncvar_get(pm_gswp, "lat")
lon_list <- ncvar_get(pm_gswp, "lon")
loc <- expand_grid(lat = lat_list, lon = lon_list )

# Find variable names in ncdf
var_name <- attributes(pm_gswp$var)$names

nc_close(pm_gswp)

###########################################################################
###  Read in land mask
###########################################################################
land_df <- readRDS(file.path(gswp_folder, "land_gswp3_df.Rds"))

### Create the land object
land <- list(land_df =land_df, lat_list = lat_list, lon_list = lon_list)




  ###########################################################################
  ### Read and process Koppen Geiger
  ###########################################################################
  require(raster)

  # Read raster files
  period='1986-2010'
  r <- raster(file.path(here(), paste0('data/sites/Map_KG-Global/KG_', period, '.grd')))


  # Color palette for climate classification
  climate.colors=c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64", "#007800", "#005000", "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00", "#550055", "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF", "#64FFFF", "#F5FFFF")

  # Legend must correspond to all climate classes, insert placeholders
  r0 <- r[1:32]; r[1:32] <- seq(1,32,1)

  # Converts raster field to categorical data
  r <- ratify(r); rat <- levels(r)[[1]]

  # Legend is always drawn in alphabetic order
  rat$climate <- c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc', 'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET', 'Ocean')

  # Remove the placeholders
  r[1:32] <- r0; levels(r) <- rat

plot(r, col.regions=climate.colors)

###########################################################################
###  Read in spot location
###########################################################################
sites_df <- read_csv(file.path(here(), "data/sites/sites.csv")) %>%
  rename(lon = Lon) %>%
  rename(lat = Lat) %>%
  rename(name = Name) %>%
  mutate(kg_main = substr(KG,1,1))


  # Find the climate class for each location
#  sites_df$kg_raster <- NA
#  for(j in seq(1, dim(sites_df)[1])){
#    kg_temp <- r[cellFromXY(r, c(sites_df$lon[[j]], sites_df$lat[[j]]))]
#    sites_df$kg_raster[[j]] <- kg_temp
#    rm(kg_temp)
#  }

#  sites_df$kg_raster <- factor(sites_df$kg_raster, levels = rat$ID, labels = rat$climate)
#  sites_df$kg_raster_main <- substr(as.character(sites_df$kg_raster), 1, 1)


ggplot(sites_df, aes(x=lon, y=lat)) + geom_point() + geom_text(aes(label = name), nudge_x = 25, nudge_y = 3) + theme_bw(8)


#ggplot(sites_df, aes(x=lon, y=lat)) %>%
#  + geom_point() %>%
#  + geom_point(data = spot_df, colour = "red") %>%
#  + geom_text(aes(label = name), nudge_x = 25, nudge_y = 3) %>%
#  + geom_text(data = spot_df, aes(label = name), nudge_x = 25, nudge_y = 3, colour = "red") %>%
#  + theme_bw(6)


###########################################################################
###
###########################################################################

### Read in the data
precip_file <- file.path(gswp_folder, "pr_gswp3_1901_2010.nc")
#pet_file <- file.path(gswp_folder, "cru_ts4.06.1901.2021.pet.dat.nc")
ncdf_precip <- nc_open(precip_file)

### Extract time
begin_date <- ncatt_get(ncdf_precip, "time", attname = "units")$value
begin_date <- substring(begin_date,11,nchar(begin_date))
date_list <- ncvar_get(ncdf_precip, "time") + as.Date(begin_date)

land_df <- land$land_df
lon_list <- land$lon_list
lat_list <- land$lat_list


sites_df$lat_center <- NA
sites_df$lon_center <- NA
sites_df$center_kg <- NA

for(j in seq(1, dim(sites_df)[1])){
land_j <- sites_df[j,]
lat_j <- land_j$lat[[1]]
lon_j <- land_j$lon[[1]]

### Find the lat and lon column
lat_col <- which.min(abs(lat_list - lat_j))
lon_col <- which.min(abs(lon_list - lon_j))

lat_center <- lat_list[lat_col]
lon_center <- lon_list[lon_col]

sites_df$lat_center[[j]] <- lat_center
sites_df$lon_center[[j]] <- lon_center
sites_df$center_kg[[j]] <- r[cellFromXY(r, c(lon_center, lat_center))]

precip_j <- ncvar_get(ncdf_precip, "pr",
                         start=c(lon_col,lat_col,1),
                         count=c(1,1,-1))

precip_df <- data.frame(date = date_list, precip = precip_j) %>%
  rename(prcp_mm_daily = precip) %>%
  mutate(year = year(date), month = month(date), day = day(date), jdate = yday(date)) %>%
  mutate(name = land_j$name)

file_name <- paste0(land_j$name, "_", lat_center, "_", lon_center)
saveRDS( precip_df, file.path(write_output_path, paste0("precip_", file_name, ".Rds")))

}


nc_close(ncdf_precip)


sites_df$center_kg <- factor(sites_df$center_kg, levels = rat$ID, labels = rat$climate)
sites_df$center_kg_main <- substr(as.character(sites_df$center_kg), 1, 1)

saveRDS( sites_df, file.path(write_output_path, paste0("sites_df.Rds")))



###########################################################################
### Process and plot the maps
###########################################################################
#sites_df <- readRDS("./output/pr/sites_df.Rds")

require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
require(rnaturalearthhires)
require(ggsflabel)

### Fix Punta Arenas
sites_df$center_kg[sites_df$name == "Punta Arenas, Chile"] <- "ET"
sites_df$center_kg_main[sites_df$name == "Punta Arenas, Chile"] <- "E"

sites_sf <- st_as_sf(sites_df, coords = c("lon", "lat"), crs = 4326 )



countries <- ne_countries(scale = "small", returnclass = 'sf') #%>% st_transform(2163)
coastline <- ne_coastline(scale = "small", returnclass = 'sf') #%>% st_transform(2163)
#lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', returnclass = 'sf')
#rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = 'sf')

p <- ggplot(data = sites_sf) %>%
  + geom_sf(data = countries, fill = "grey70", alpha = 0.1, colour = "grey60") %>%
#  + geom_sf(data = rivers, fill = NA, colour = "#74a9cf", alpha = 0.8) %>%
#  + geom_sf(data = lakes, fill = "#74a9cf", colour = "#74a9cf", alpha = 0.5) %>%
#  + geom_sf(data = states, fill = NA, alpha = 0.5) %>%
  + geom_sf(aes(colour = center_kg), size = 2) %>%
  + coord_sf(ylim = c(-6.5e6, 9.9e6),  crs = 4088, expand = FALSE)  %>%
  + scale_colour_manual(limits = rat$climate, labels = rat$climate, values = climate.colors) %>%
  + xlab("Longitude") %>%
  + ylab("Latitude") %>%
  + theme_bw(11) %>%
  + theme(legend.position="bottom") %>%
  +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p


### Save plot
ggsave(file.path(write_figures_path, "site_map_kg.png"), p,  width = 6.5, height = 6, dpi = 600)
ggsave(file.path(write_figures_path, "site_map_kg.svg"), p,  width = 6.5, height = 6)

p <- p + theme(legend.position = "none")
### Save plot
ggsave(file.path(write_figures_path, "site_map_kg_nolegend.png"), p,  width = 6.5, height = 4, dpi = 600)
ggsave(file.path(write_figures_path, "site_map_kg_nolegend.svg"), p,  width = 6.5, height = 4)

p <- p + geom_sf_text_repel(aes(label = name), size = 1.8, nudge_y = 15)
### Save plot
ggsave(file.path(write_figures_path, "site_map_kg_nolegend_labels.png"), p,  width = 6.5, height = 4, dpi = 600)
ggsave(file.path(write_figures_path, "site_map_kg_nolegend_labels.svg"), p,  width = 6.5, height = 4)



  p <- ggplot(data = sites_sf) %>%
    + geom_sf(data = countries, fill = "grey70", alpha = 0.1, colour = "grey60") %>%
  #  + geom_sf(data = rivers, fill = NA, colour = "#74a9cf", alpha = 0.8) %>%
  #  + geom_sf(data = lakes, fill = "#74a9cf", colour = "#74a9cf", alpha = 0.5) %>%
  #  + geom_sf(data = states, fill = NA, alpha = 0.5) %>%
    + geom_sf(aes(colour = center_kg_main), size = 2) %>%
    + coord_sf(ylim = c(-6.5e6, 9.9e6),  crs = 4088, expand = FALSE)  %>%
    + scale_colour_manual(limits = c("A", "B", "C", "D", "E"), labels = c("A", "B", "C", "D", "E"), values = climate.colors[c(2, 7, 14, 20, 31)]) %>%
    + xlab("Longitude") %>%
    + ylab("Latitude") %>%
    + theme_bw(11) %>%
    + theme(legend.position="bottom") %>%
    +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  p

  ### Save plot
  ggsave(file.path(write_figures_path, "site_map_kgmain.png"), p,  width = 6.5, height = 6, dpi = 600)
  ggsave(file.path(write_figures_path, "site_map_kgmain.svg"), p,  width = 6.5, height = 6)

  p <- p + theme(legend.position = "none")
  ### Save plot
  ggsave(file.path(write_figures_path, "site_map_kgmain_nolegend.png"), p,  width = 6.5, height = 4, dpi = 600)
  ggsave(file.path(write_figures_path, "site_map_kgmain_nolegend.svg"), p,  width = 6.5, height = 4)

  p <- p + geom_sf_text_repel(aes(label = name), size = 1.8, nudge_y = 15)
  ### Save plot
  ggsave(file.path(write_figures_path, "site_map_kgmain_nolegend_labels.png"), p,  width = 6.5, height = 4, dpi = 600)
  ggsave(file.path(write_figures_path, "site_map_kgmain_nolegend_labels.svg"), p,  width = 6.5, height = 4)











    ###########################################################################
    ### Simulate theoretical
    ###########################################################################

  theor_vline <- data.frame(lag = c(30, 91, 182, 365, 730), metric = spi_levels, short_label = c("spi_1", "spi_3", "spi_6", "spi_12", "spi_24")) %>%
    mutate(plot_max = round(lag*1.4))
    theor_vline$metric <- factor(theor_vline$metric, levels = spi_levels, labels =  spi_levels)

  n_rep <- 1000

  tic()
  for(j in seq(1, dim(theor_vline)[1])){

    spi_theor_j <- theor_sci_corr(n_years = 110, resolution = "daily", n_rep = n_rep, window = theor_vline$lag[[j]], lag_list = lag_list, ci = 0.95)
    saveRDS(spi_theor_j, file.path(output_path, paste0("gswp3_persist/spi_", theor_vline$lag[[j]], "_theor_110years.Rds")))

    spi_theor_j <- spi_theor_j$summary %>%
      mutate(metric = theor_vline$metric[[j]])
    if(j == 1){
      spi_theor <- spi_theor_j
    } else {
      spi_theor <- spi_theor %>%
        bind_rows(spi_theor_j)
    }
    rm(spi_theor_j)
  }

  toc()

saveRDS( spi_theor,  file.path(write_output_path, paste0("theor_lag.Rds")))

  ###########################################################################
  ### Calculate SPI
  ###########################################################################
  #install_github("jstagge/SCI@version2024")
  #devtools::install_github("jstagge/SCI",  ref = "version2024", subdir = "pkg")

  require(SCI)

  for(j in seq(1, dim(sites_df)[1])){
  land_j <- sites_df[j,]
  latcenter_j <- land_j$lat_center[[1]]
  loncenter_j <- land_j$lon_center[[1]]

  file_name <- paste0(land_j$name, "_", latcenter_j, "_", loncenter_j)
  precip_j <- readRDS(file.path("./output/pr", paste0("precip_", file_name, ".Rds")))

  ts_j <- zoo(precip_j$prcp_mm_daily, precip_j$date)
  ts_j <- sci_ts(ts = ts_j, resolution = "daily")

  fit_1 <- fitSCI(ts_j,  window = 30, distr = "gamma", method = "spline", p0 = TRUE, max_na_prop = 0.05, n_knots = 18)
  spi_1 <- transformSCI(fit = fit_1, new_ts=NA, sci.limit=3.5, ci = NA)

  fit_3 <- fitSCI(ts_j,  window = 91, distr = "gamma", method = "spline", p0 = TRUE, max_na_prop = 0.05, n_knots = 18)
  spi_3 <- transformSCI(fit = fit_3, new_ts=NA, sci.limit=3.5, ci = NA)

  fit_6 <- fitSCI(ts_j,  window = 182, distr = "gamma", method = "spline", p0 = TRUE, max_na_prop = 0.05, n_knots = 18)
  spi_6 <- transformSCI(fit = fit_6, new_ts=NA, sci.limit=3.5, ci = NA)

  fit_12 <- fitSCI(ts_j,  window = 365, distr = "gamma", method = "spline", p0 = TRUE, max_na_prop = 0.05, n_knots = 18)
  spi_12 <- transformSCI(fit = fit_12, new_ts=NA, sci.limit=3.5, ci = NA)

  fit_24 <- fitSCI(ts_j,  window = 730, distr = "gamma", method = "spline", p0 = TRUE, max_na_prop = 0.05, n_knots = 18)
  spi_24 <- transformSCI(fit = fit_24, new_ts=NA, sci.limit=3.5, ci = NA)


  saveRDS( list(fit_1 = fit_1, fit_3 = fit_3, fit_6 = fit_6, fit_12 = fit_12, fit_24 = fit_24), file.path(write_output_path, paste0("spi_fit_", file_name, ".Rds")))
  saveRDS( list(spi_1 = spi_1$df,  spi_3 = spi_3$df, spi_6 = spi_6$df, spi_12 = spi_12$df, spi_24 = spi_24$df), file.path(write_output_path, paste0("spi_", file_name, ".Rds")))

}



  ###########################################################################
  ### Functions
  ###########################################################################

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE)
  {
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
      } else {
        x[1:nth != 1]
        }
    } else {
      if(empty) {
        x[1:nth != 1] <- ""
        x
        } else {
          x[1:nth == 1]
        }
    }
}

# Function to calculate correlation for a given lag
calculate_lag_correlation <- function(lag_value, data, method) {
  lagged_data <- data %>%
    mutate(lagged = lag(sci, lag_value))
  cor_value <- cor(lagged_data$sci, lagged_data$lagged, use = "pairwise.complete.obs", method = method)
  return(c(lag = lag_value, correlation = cor_value))
}



lag_list <- seq(0,round(365*2.3))




  ###########################################################################
  ### Process Columbus
  ###########################################################################

site_data <-  readRDS(file.path(write_output_path, "spi_Columbus, USA_39.75_-82.75.Rds"))

plot_df <- site_data$spi_1 %>%
  mutate(metric = "spi_1") %>%
  bind_rows(site_data$spi_3 %>% mutate(metric = "spi_3")) %>%
  bind_rows(site_data$spi_6 %>% mutate(metric = "spi_6")) %>%
  bind_rows(site_data$spi_12 %>% mutate(metric = "spi_12")) %>%
  bind_rows(site_data$spi_24 %>% mutate(metric = "spi_24"))

plot_df$metric <- factor(plot_df$metric, levels = c("spi_1", "spi_3", "spi_6", "spi_12", "spi_24"), labels = c("SPI 1", "SPI 3", "SPI 6", "SPI 12", "SPI 24"))
date_breaks <- seq(as.Date("1900-01-01"), as.Date("2020-01-01"), by = "10 years")

p <- ggplot(plot_df, aes(x=date, y=sci)) %>%
  + facet_grid(metric ~ .) %>%
  + geom_hline(yintercept = 0, colour = "grey70", alpha = 0.5, linewidth = 0.1) %>%
  + geom_line(linewidth = 0.2) %>%
  + theme_bw(10) %>%
  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) %>%
  + scale_y_continuous(name = "SPI", breaks = seq(-3,3, by = 1.5), labels = c("", -1.5, 0, 1.5, "")) %>%
  + scale_x_continuous(name = "Date", breaks = date_breaks, labels = every_nth(substr(as.character(date_breaks),1,4), nth = 2,inverse = TRUE) ) %>%
  + coord_cartesian(xlim = c(as.Date("1899-01-01"), as.Date("2011-12-31")), ylim = c(-3.6,3.6), expand = FALSE)

p

  ### Save plot
  ggsave(file.path(write_figures_path, "columbus_ts.png"), p,  width = 6.5, height = 4, dpi = 600)
  ggsave(file.path(write_figures_path, "columbus_ts.svg"), p,  width = 6.5, height = 4)


### Calculate lagged correlation for observations
# Use lapply to calculate correlations for all lags from 1 to max_lag
spi1_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_1, method = "pearson")))
spi3_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_3, method = "pearson")))
spi6_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_6, method = "pearson")))
spi12_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_12, method = "pearson")))
spi24_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_24, method = "pearson")))

plot_lagcor <-  spi1_lagcor %>%
  mutate(metric = "SPI 1") %>%
  bind_rows(spi3_lagcor %>% mutate(metric = "SPI 3")) %>%
  bind_rows(spi6_lagcor %>% mutate(metric = "SPI 6")) %>%
  bind_rows(spi12_lagcor %>% mutate(metric = "SPI 12")) %>%
  bind_rows(spi24_lagcor %>% mutate(metric = "SPI 24")) %>%
  mutate(site = "Columbus, USA")

spi_levels <- c("SPI 1", "SPI 3", "SPI 6", "SPI 12", "SPI 24")
  plot_lagcor$metric <- factor(plot_lagcor$metric, levels = spi_levels, labels =  spi_levels)


plot_theor <- spi_theor %>%
  mutate(site = "Columbus, USA")
plot_theor$metric <- factor(plot_theor$metric, levels = spi_levels, labels =  spi_levels)


p <- ggplot(plot_lagcor, aes(x=lag)) %>%
    + geom_hline(yintercept = 0, colour = "grey60", linetype = "dotted", linewidth = 0.3) %>%
    + geom_vline(data = theor_vline, aes(xintercept = lag), linetype = "dotted", colour = "grey60", linewidth = 0.3) %>%
    + geom_ribbon(data = plot_theor, aes(ymin = lower_ci, ymax = upper_ci), fill = "grey80", alpha = 0.5) %>%
    + geom_line(data = plot_theor, aes(y=mean), colour = "black") %>%
    + geom_line(aes(y=correlation), colour = "red") %>%
    + facet_grid(site ~ metric) %>%
    + scale_x_continuous(name = "Lag (days)") %>%
    + scale_y_continuous(name = "Correlation", breaks = seq(-1, 1, by = 0.25), labels = every_nth(seq(-1, 1, by = 0.25), nth = 2,inverse = TRUE)) %>%
    + coord_cartesian(ylim = c(-0.25, 1.05), xlim = c(-5,735), expand = FALSE) %>%
    + theme_bw(11) %>%
    + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p

### Save plot
ggsave(file.path(write_figures_path, "columbus_lag.png"), p,  width = 8.5, height = 2.5, dpi = 600)
ggsave(file.path(write_figures_path, "columbus_lag.svg"), p,  width = 8.5, height = 2.5)


#### Subset each bit
for(j in seq(1, dim(theor_vline)[1])){
  metric_j <- theor_vline$metric[j]
  plotmax_j <- theor_vline$plot_max[j]

  plot_j <- plot_lagcor %>%
    filter(metric == metric_j) %>%
    filter(lag <= plotmax_j)

  theor_j <- plot_theor %>%
    filter(metric == metric_j) %>%
    filter(lag <= plotmax_j)

  if(j == 1){
    plot_lagcor_subset <- plot_j
    plot_theor_subset <- theor_j
  } else {
    plot_lagcor_subset <- plot_lagcor_subset %>%
      bind_rows(plot_j)
    plot_theor_subset <- plot_theor_subset %>%
      bind_rows(theor_j)
  }
}


p <- ggplot(plot_lagcor_subset, aes(x=lag)) %>%
    + geom_hline(yintercept = 0, colour = "grey60", linetype = "dotted") %>%
    + geom_vline(data = theor_vline, aes(xintercept = lag), linetype = "dotted", colour = "grey60") %>%
    + geom_ribbon(data = plot_theor_subset, aes(ymin = lower_ci, ymax = upper_ci), fill = "grey80", alpha = 0.5) %>%
    + geom_line(data = plot_theor_subset, aes(y=mean), colour = "black") %>%
    + geom_line(aes(y=correlation), colour = "red") %>%
    + facet_grid(site ~ metric, scales = "free_x") %>%
    + scale_x_continuous(name = "Lag (days)") %>%
    + scale_y_continuous(name = "Correlation", breaks = seq(-1, 1, by = 0.25), labels = every_nth(seq(-1, 1, by = 0.25), nth = 2,inverse = TRUE)) %>%
    + coord_cartesian(ylim = c(-0.25, 1.05), expand = FALSE) %>%
    + theme_bw(11) %>%
    + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p
    ### Save plot
    ggsave(file.path(write_figures_path, "columbus_lag_subset.png"), p,  width = 8.5, height = 2.5, dpi = 600)
    ggsave(file.path(write_figures_path, "columbus_lag_subset.svg"), p,  width = 8.5, height = 2.5)





###########################################################################
### All sites
###########################################################################

for(j in seq(1, dim(sites_df)[1])){

    land_j <- sites_df[j,]
    latcenter_j <- land_j$lat_center[[1]]
    loncenter_j <- land_j$lon_center[[1]]

    file_name <- paste0(land_j$name, "_", latcenter_j, "_", loncenter_j)


    site_data <- readRDS(file.path(write_output_path, paste0("spi_", file_name, ".Rds")))

    plot_df <- site_data$spi_1 %>%
      mutate(metric = "spi_1") %>%
      bind_rows(site_data$spi_3 %>% mutate(metric = "spi_3")) %>%
      bind_rows(site_data$spi_6 %>% mutate(metric = "spi_6")) %>%
      bind_rows(site_data$spi_12 %>% mutate(metric = "spi_12")) %>%
      bind_rows(site_data$spi_24 %>% mutate(metric = "spi_24"))

    plot_df$metric <- factor(plot_df$metric, levels = c("spi_1", "spi_3", "spi_6", "spi_12", "spi_24"), labels = c("SPI 1", "SPI 3", "SPI 6", "SPI 12", "SPI 24"))
    date_breaks <- seq(as.Date("1900-01-01"), as.Date("2020-01-01"), by = "10 years")

    p <- ggplot(plot_df, aes(x=date, y=sci)) %>%
      + facet_grid(metric ~ .) %>%
      + geom_hline(yintercept = 0, colour = "grey70", alpha = 0.5, linewidth = 0.1) %>%
      + geom_line(linewidth = 0.2) %>%
      + theme_bw(10) %>%
      + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) %>%
      + scale_y_continuous(name = "SPI", breaks = seq(-3,3, by = 1.5), labels = c("", -1.5, 0, 1.5, "")) %>%
      + scale_x_continuous(name = "Date", breaks = date_breaks, labels = every_nth(substr(as.character(date_breaks),1,4), nth = 2,inverse = TRUE) ) %>%
      + coord_cartesian(xlim = c(as.Date("1899-01-01"), as.Date("2011-12-31")), ylim = c(-3.6,3.6), expand = FALSE)

    p

      ### Save plot
      ggsave(file.path(write_figures_path, paste0(file_name, "_ts.png")), p,  width = 6.5, height = 4, dpi = 600)
      ggsave(file.path(write_figures_path, paste0(file_name, "_ts.svg")), p,  width = 6.5, height = 4)


    ### Calculate lagged correlation for observations
    # Use lapply to calculate correlations for all lags from 1 to max_lag
    spi1_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_1, method = "pearson")))
    spi3_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_3, method = "pearson")))
    spi6_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_6, method = "pearson")))
    spi12_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_12, method = "pearson")))
    spi24_lagcor <- as.data.frame(do.call(rbind, lapply(lag_list, calculate_lag_correlation, data = site_data$spi_24, method = "pearson")))

    plot_lagcor <-  spi1_lagcor %>%
      mutate(metric = "SPI 1") %>%
      bind_rows(spi3_lagcor %>% mutate(metric = "SPI 3")) %>%
      bind_rows(spi6_lagcor %>% mutate(metric = "SPI 6")) %>%
      bind_rows(spi12_lagcor %>% mutate(metric = "SPI 12")) %>%
      bind_rows(spi24_lagcor %>% mutate(metric = "SPI 24")) %>%
      mutate(site = land_j$name)

    spi_levels <- c("SPI 1", "SPI 3", "SPI 6", "SPI 12", "SPI 24")
      plot_lagcor$metric <- factor(plot_lagcor$metric, levels = spi_levels, labels =  spi_levels)


    plot_theor <- spi_theor %>%
      mutate(site = land_j$name)
    plot_theor$metric <- factor(plot_theor$metric, levels = spi_levels, labels =  spi_levels)


    p <- ggplot(plot_lagcor, aes(x=lag)) %>%
        + geom_hline(yintercept = 0, colour = "grey60", linetype = "dotted", linewidth = 0.3) %>%
        + geom_vline(data = theor_vline, aes(xintercept = lag), linetype = "dotted", colour = "grey60", linewidth = 0.3) %>%
        + geom_ribbon(data = plot_theor, aes(ymin = lower_ci, ymax = upper_ci), fill = "grey80", alpha = 0.5) %>%
        + geom_line(data = plot_theor, aes(y=mean), colour = "black") %>%
        + geom_line(aes(y=correlation), colour = "red") %>%
        + facet_grid(site ~ metric) %>%
        + scale_x_continuous(name = "Lag (days)") %>%
        + scale_y_continuous(name = "Correlation", breaks = seq(-1, 1, by = 0.25), labels = every_nth(seq(-1, 1, by = 0.25), nth = 2,inverse = TRUE)) %>%
        + coord_cartesian(ylim = c(-0.25, 1.05), xlim = c(-5,735), expand = FALSE) %>%
        + theme_bw(11) %>%
        + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    p

    ### Save plot
    ggsave(file.path(write_figures_path, paste0(file_name, "_lag.png")), p,  width = 8.5, height = 2.5, dpi = 600)
    ggsave(file.path(write_figures_path, paste0(file_name, "_lag.svg")), p,  width = 8.5, height = 2.5)


    #### Subset each bit
    for(j in seq(1, dim(theor_vline)[1])){
      metric_j <- theor_vline$metric[j]
      plotmax_j <- theor_vline$plot_max[j]

      plot_j <- plot_lagcor %>%
        filter(metric == metric_j) %>%
        filter(lag <= plotmax_j)

      theor_j <- plot_theor %>%
        filter(metric == metric_j) %>%
        filter(lag <= plotmax_j)

      if(j == 1){
        plot_lagcor_subset <- plot_j
        plot_theor_subset <- theor_j
      } else {
        plot_lagcor_subset <- plot_lagcor_subset %>%
          bind_rows(plot_j)
        plot_theor_subset <- plot_theor_subset %>%
          bind_rows(theor_j)
      }
    }


    p <- ggplot(plot_lagcor_subset, aes(x=lag)) %>%
      + geom_hline(yintercept = 0, colour = "grey60", linetype = "dotted", linewidth = 0.3) %>%
      + geom_vline(data = theor_vline, aes(xintercept = lag), linetype = "dotted", colour = "grey60", linewidth = 0.3) %>%
        + geom_ribbon(data = plot_theor_subset, aes(ymin = lower_ci, ymax = upper_ci), fill = "grey80", alpha = 0.5) %>%
        + geom_line(data = plot_theor_subset, aes(y=mean), colour = "black") %>%
        + geom_line(aes(y=correlation), colour = "red") %>%
        + facet_grid(site ~ metric, scales = "free_x") %>%
        + scale_x_continuous(name = "Lag (days)") %>%
        + scale_y_continuous(name = "Correlation", breaks = seq(-1, 1, by = 0.25), labels = every_nth(seq(-1, 1, by = 0.25), nth = 2,inverse = TRUE)) %>%
        + coord_cartesian(ylim = c(-0.25, 1.05), expand = FALSE) %>%
        + theme_bw(11) %>%
        + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    p
        ### Save plot
        ggsave(file.path(write_figures_path, paste0(file_name, "_lag_subset.png")), p,  width = 8.5, height = 2.5, dpi = 600)
        ggsave(file.path(write_figures_path, paste0(file_name, "_lag_subset.svg")), p,  width = 8.5, height = 2.5)

  }




  ###########################################################################
  ### Plot theor
  ###########################################################################
  n_runs <- 20

  for(j in seq(1, dim(theor_vline)[1])){
    spi_theor_j <- readRDS(file.path(output_path, paste0("gswp3_persist/spi_", theor_vline$lag[[j]], "_theor_110years.Rds")))

    spi_theor_j <- spi_theor_j$raw %>%
      filter(run <= n_runs)  %>%
        mutate(metric = theor_vline$metric[[j]])

      if(j == 1){
        spi_theor_runs <- spi_theor_j
      } else {
        spi_theor_runs <- spi_theor_runs %>%
          bind_rows(spi_theor_j)
      }
      rm(spi_theor_j)
  }


  spi_theor_runs$metric <- factor(spi_theor_runs$metric, levels = spi_levels, labels =  spi_levels)
  spi_theor_runs$site <- "Simulated"

  p <- ggplot(spi_theor_runs, aes(x=lag)) %>%
      + geom_hline(yintercept = 0, colour = "grey60", linetype = "dotted", linewidth = 0.3) %>%
      + geom_vline(data = theor_vline, aes(xintercept = lag), linetype = "dotted", colour = "grey60", linewidth = 0.3) %>%
      + geom_ribbon(data = spi_theor, aes(ymin = lower_ci, ymax = upper_ci), fill = "grey80", alpha = 0.5) %>%
      + geom_line(data = spi_theor_runs, aes(y=correlation, group = run), colour = "black", linewidth = 0.15, alpha = 0.5) %>%
      + facet_grid(site ~ metric) %>%
      + scale_x_continuous(name = "Lag (days)") %>%
      + scale_y_continuous(name = "Correlation", breaks = seq(-1, 1, by = 0.25), labels = every_nth(seq(-1, 1, by = 0.25), nth = 2,inverse = TRUE)) %>%
      + coord_cartesian(ylim = c(-0.25, 1.05), xlim = c(-5,735), expand = FALSE) %>%
      + theme_bw(11) %>%
      + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  p
  ### Save plot
  ggsave(file.path(write_figures_path, paste0("simulated_lag.png")), p,  width = 8.5, height = 2.5, dpi = 600)
  ggsave(file.path(write_figures_path, paste0("simulated_lag.svg")), p,  width = 8.5, height = 2.5)


  #### Subset each bit
  for(j in seq(1, dim(theor_vline)[1])){
    metric_j <- theor_vline$metric[j]
    plotmax_j <- theor_vline$plot_max[j]

    plot_j <- spi_theor_runs %>%
      filter(metric == metric_j) %>%
      filter(lag <= plotmax_j)

    theor_j <- plot_theor %>%
      filter(metric == metric_j) %>%
      filter(lag <= plotmax_j)

    if(j == 1){
      plot_lagcor_subset <- plot_j
      plot_theor_subset <- theor_j
    } else {
      plot_lagcor_subset <- plot_lagcor_subset %>%
        bind_rows(plot_j)
      plot_theor_subset <- plot_theor_subset %>%
        bind_rows(theor_j)
    }
  }



    plot_lagcor_subset$metric <- factor(plot_lagcor_subset$metric, levels = spi_levels, labels =  spi_levels)
    plot_lagcor_subset$site <- "Simulated"
    plot_theor_subset$metric <- factor(plot_theor_subset$metric, levels = spi_levels, labels =  spi_levels)
    plot_theor_subset$site <- "Simulated"



      p <- ggplot(plot_lagcor_subset, aes(x=lag)) %>%
          + geom_hline(yintercept = 0, colour = "grey60", linetype = "dotted", linewidth = 0.3) %>%
          + geom_vline(data = theor_vline, aes(xintercept = lag), linetype = "dotted", colour = "grey60", linewidth = 0.3) %>%
          + geom_ribbon(data = plot_theor_subset, aes(ymin = lower_ci, ymax = upper_ci), fill = "grey80", alpha = 0.5) %>%
          + geom_line(data = plot_lagcor_subset, aes(y=correlation, group = run), colour = "black", linewidth = 0.15, alpha = 0.5) %>%
          + facet_grid(site ~ metric, scales = "free_x") %>%
          + scale_x_continuous(name = "Lag (days)") %>%
          + scale_y_continuous(name = "Correlation", breaks = seq(-1, 1, by = 0.25), labels = every_nth(seq(-1, 1, by = 0.25), nth = 2,inverse = TRUE)) %>%
          + coord_cartesian(ylim = c(-0.25, 1.05), expand = FALSE) %>%
          + theme_bw(11) %>%
          + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

      p

      ### Save plot
      ggsave(file.path(write_figures_path, paste0("simulated_lag_subset.png")), p,  width = 8.5, height = 2.5, dpi = 600)
      ggsave(file.path(write_figures_path, paste0("simulated_lag_subset.svg")), p,  width = 8.5, height = 2.5)
