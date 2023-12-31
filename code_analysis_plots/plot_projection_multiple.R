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
library(sf)

tic("total")
# create parser object
parser <- ArgumentParser()
# specify our desired options 
# by default ArgumentParser will add an help option
parser$add_argument("--account", default='xge', type="character")
parser$add_argument("--stateID", default='IA', type="character")
parser$add_argument("--speciestype", default='state_flowers_single', type="character")
parser$add_argument("--run_loc", default='loc', type="character", help='loc or cc(compute canada)')
parser$add_argument("--no_cores", default=8, type='integer')

args <- parser$parse_args()
stateID <- args$stateID
speciestype <-args$speciestype
account<- args$account
run_loc <- args$run_loc

if (run_loc=='cc'){
  source(paste0("/home/",account,"/scratch/state_species_2023/codes/all_functions.R"))
  setwd(paste0("/home/",account,"/scratch/state_species_2023"))
  n.cores <- args$no_cores
}else{
  # source("/Users/xuezhenge/Desktop/state_species_2023/codes/all_functions.R")
  # setwd("/Users/xuezhenge/Desktop/state_species_2023")
  source("/Volumes/UoG_xuezhen/state_species_2023/codes/all_functions.R")
  setwd("/Volumes/UoG_xuezhen/state_species_2023")
  n.cores <- 2
}


#us map
us_map <- st_as_sf(tigris::states(cb = TRUE))
# continental_us
us_map <- us_map %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

species_list <- read.csv(paste0('inputs/','state_flowers_multiple','.csv'),header = TRUE)

plot_proj_map<- function(stateIDs,time.period){
  state.name <- filter(species_list,ID==stateIDs[1])
  stateID = substr(stateIDs[1], 1,2)
  no.rasters <- length(stateIDs)
  if (time.period == '1971-2000'){
    rast_subfolder = '/ensmean_hist/'
  }else{
    rast_subfolder = '/2081-2100_ssp585/'
  }
  
  species_all <- species_list[which(species_list$ID %in% stateIDs),]$spec_name
  raster_files <- paste0('outplots_v1/',species_all,rast_subfolder,'omth10.tif')
  # Read and stack the rasters
  rasters <- lapply(raster_files, rast)
  raster_stack <- do.call(c, rasters)
  # Calculate the maximum value for each pixel across all layers
  raster_max <- max(raster_stack)
  raster_max <- raster(raster_max)
  
  crs_raster <- crs(raster_max)
  if (st_crs(us_map) != crs_raster) {
    us_map_transformed <- st_transform(us_map, crs_raster)
  } else {
    us_map_transformed <- us_map
  }
  
  raster_data2 <- crop(raster_max, extent(us_map_transformed))
  raster_us <- mask(raster_data2, us_map_transformed)
  
  state<- us_map_transformed[us_map_transformed$STUSPS== stateID, ]
  raster_data2 <- crop(raster_us, extent(state))
  raster_state <- mask(raster_data2, state)
  
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
  
  plot.dir = paste0('outplots/',stateID,'/proj_US_',time.period,'.jpg')
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
  
  plot.dir = paste0('outplots/',stateID,'/proj_state_',time.period,'.jpg')
  ggsave(plot.dir, my_plot, dpi = 300, width = 4, height = 2.5, units = "in")
}

multipleID <- c('TX','MI','IN','ND','NM')
stateIDlist <- list(states_NA, states_ND, states_TX)
for (stateIDs in stateIDlist){
  plot_proj_map(stateIDs,'1971-2000')
  plot_proj_map(stateIDs,'2081-2100')
}


