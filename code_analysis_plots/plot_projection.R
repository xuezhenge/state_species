rm()
rm(list=ls())
library(terra)
library(rgdal)
library(dismo)
library(tools)
library(dplyr)
library(biomod2)
library(parallel)
library(doParallel)
library(argparse)
library(raster)
library(tictoc)
library(introdataviz)
library(sf)


setwd("/Volumes/UoG_xuezhen/state_species_2024")
ForI = 'flowers'

if (ForI == 'flowers'){
  out_folder = 'outplots_flowers'
  outplot_folder = 'figure_flowers'
}else{
  out_folder = 'outplots_insects'
  outplot_folder = 'figure_insects'
}

#us map
us_map <- st_as_sf(tigris::states(cb = TRUE))
# continental_us
us_map <- us_map %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

plot_proj_map<- function(stateID,state.name,species,time.period){
  if (time.period == '1971-2000'){
    raster <- raster(paste0(out_folder,'/',species,'/ensmean_hist/omth10.tif'))
  }else{
    raster <- raster(paste0(out_folder,'/',species,'/2081-2100_ssp585/omth10.tif'))
  }

  crs_raster <- crs(raster)
  if (st_crs(us_map) != crs_raster) {
    us_map_transformed <- st_transform(us_map, crs_raster)
  } else {
    us_map_transformed <- us_map
  }
  
  raster_data2 <- crop(raster, extent(us_map_transformed))
  raster_us <- mask(raster_data2, us_map_transformed)
  
  state<- us_map_transformed[us_map_transformed$NAME == state.name, ]
  raster_data2 <- crop(raster, extent(state))
  raster_state <- mask(raster_data2, state)
  
  #Check here
  #get data
  raster_values <- getValues(raster_us)
  raster_coords <- xyFromCell(raster_us, 1:ncell(raster_us))
  df <- data.frame(longitude = raster_coords[, 1], latitude = raster_coords[, 2], value = raster_values)
  df <- na.omit(df)
  
  my.palette <- brewer.pal(n = 9, name = "OrRd")
  my_plot <- ggplot() +
    # geom_sf(data = us_map_transformed, fill = "#E8E8E8", color = "white", size = 0.5) +
    geom_raster(data = df, aes(x = longitude, y = latitude, fill = value)) +
    # Border for the us
    geom_sf(data = us_map_transformed, fill = NA, color = "black", size = 0.5) +
    geom_sf(data = state, fill = NA, color = "lightblue", size = 4) +
    scale_fill_gradientn(colours = my.palette, limits = c(0, 1)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # Removes grid lines
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2),
      panel.background = element_rect(fill = 'white') #"#E6E6FA"
    )
  
  plot.dir = paste0(outplot_folder,'/',species,'/proj_US_',time.period,'.jpg')
  ggsave(plot.dir, my_plot, dpi = 300, width = 8, height = 5, units = "in")
  
  #Make the plot for the state
  #get data
  raster_values <- getValues(raster_state)
  raster_coords <- xyFromCell(raster_state, 1:ncell(raster_state))
  df <- data.frame(longitude = raster_coords[, 1], latitude = raster_coords[, 2], value = raster_values)
  df <- na.omit(df)
  my_plot <- ggplot() +
    # geom_sf(data = us_map_transformed, fill = "#E8E8E8", color = "white", size = 0.5) +
    geom_raster(data = df, aes(x = longitude, y = latitude, fill = value)) +
    # Border for the counties
    geom_sf(data = state, fill = NA, color = "black", size = 2) +
    scale_fill_gradientn(colours = my.palette, limits = c(0, 1)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # Removes grid lines
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0),
      panel.background = element_rect(fill = 'white') #"#E6E6FA"
    )
  
  plot.dir = paste0(outplot_folder,'/',species,'/proj_state_',time.period,'.jpg')
  ggsave(plot.dir, my_plot, dpi = 300, width = 4, height = 2.5, units = "in")
}

all_stateID_species = read.csv(paste0('inputs/state_',ForI,'_all_plots.csv'))
len = nrow(all_stateID_species)

for (i in (1:len)){
  stateID <- all_stateID_species$ID[i]
  state.name <- all_stateID_species$state[i]
  species<- all_stateID_species$spec_name[i]
  plot_proj_map(stateID,state.name,species,'1971-2000')
  plot_proj_map(stateID,state.name,species,'2081-2100')
}



