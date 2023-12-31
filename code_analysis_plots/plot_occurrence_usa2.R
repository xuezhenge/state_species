rm(list=ls())
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tigris)
library(usmap)
options(tigris_use_cache = TRUE)

ForI <- 'flowers'

setwd("/Volumes/UoG_xuezhen/state_species_2024/git/state_species")

if (ForI == 'flowers'){
  out_folder = 'outputs_flowers'
  outplot_folder = 'figure_flowers'
}else{
  out_folder = 'outputs_insects'
  outplot_folder = 'figure_insects'
}

plot_occ_map <- function(stateID,state.name,species){
  #-----plot thinned occurrence records---------------
  data<- read.csv(file.path(out_folder,species,'data_thinning/data_thinned.csv'))
  
  data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
  num.occ <- nrow(data_sf)
  
  summary.dir <- file.path(outplot_folder)
  if(!exists(summary.dir)) dir.create(summary.dir)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  usa <- world[world$admin == "United States of America", ]
  # Load US state boundaries
  states <- tigris::states(cb = TRUE, class = "sf")
  continental_us <- states %>% 
    filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))
  
  state<- states[states$NAME == state.name, ]
  
  # Plot the occurrence data on the world map
  p <- ggplot() +
    geom_sf(data = world, fill = "lightgray", color = "white") +  # World map
    geom_sf(data = data_sf, shape = 19, alpha = 0.5, size = 0.5) +  # 1981-2010 data
    geom_sf(data = usa, fill = NA, color = "black", size = 1) +   # Highlight USA
    geom_sf(data = state, fill = NA, color = "black", size = 2) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # Removes grid lines
          panel.background = element_blank(), # Removes panel background
          axis.ticks = element_line(colour = "black"), # Adds ticks
          panel.border = element_rect(colour = "black", fill=NA, size=1), # Adds box boundary
          legend.position = c(0.2, 0.2)) + # Moves legend to bottom left
    labs(title = paste0("GBIF occurrence records (", species,")"),
         x = "Longitude", y = "Latitude",
         color = "Year Range") # Legend title
  
  # Save the plot
  ggsave(file.path(summary.dir, paste0(stateID,"_",species), "occ_thinned_worldmap.jpg"), plot = p, width = 7, height = 3.5)
  
  # Create a plot focusing on US
  # Transform the CRS of your datasets to match that of the US boundry
  data_US <- st_transform(data_sf, crs = st_crs(states))
  # Spatial join to check which points are within US
  data_US <- st_join(data_US, states, join = st_within)
  # Filter to keep only those points within US
  data_within_US <- data_US[!is.na(data_US$NAME), ]
  
  # Create a plot focusing on state
  occs <- read.csv(file.path(out_folder,species,'data_thinning/data_thinned.csv'))
  occ_transformed <- usmap_transform(occs,
                                     input_names = c("lon","lat"),
                                     output_names = c("x","y"))
  occ_transformed <- filter(occ_transformed,lat >= 24.396308, lat<= 49.384358,
                            lon>= -125.001651, lon <= -66.93457)
  p<- plot_usmap() +
    geom_point(data = occ_transformed, aes(x = x, y = y),
               color = "red", alpha = 0.25) +
    theme(legend.position = "right") + 
    theme(panel.background = element_rect(colour = "black")) + 
    labs(title = paste0("GBIF occurrence data in Unite States"),
         subtitle = paste0("Species: ",species)) +
    theme(legend.position = "right",title = element_text(size=16))
  ggsave(file.path(summary.dir,paste0(stateID,"_",species),"occ_thinned_USmap1.jpg"),plot=p)
  
  # Plot the occurrence data on the world map
  p <- ggplot() +
    geom_sf(data = continental_us, fill = "lightgray", color = "white") +  # World map
    geom_sf(data = data_within_US, shape = 19, alpha = 0.5, size = 0.5) +  # occ data
    geom_sf(data = state, fill = NA, color = "red", size = 2) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # Removes grid lines
          panel.background = element_blank(), # Removes panel background
          axis.ticks = element_line(colour = "black"), # Adds ticks
          panel.border = element_rect(colour = "black", fill=NA, size=1), # Adds box boundary
          legend.position = c(0.2, 0.2)) + # Moves legend to bottom left
    labs(title = paste0("GBIF occurrence records (", species,")"),
         x = "Longitude", y = "Latitude",
         color = "Year Range") # Legend title
  
  # Save the plot
  ggsave(file.path(summary.dir,paste0(stateID,"_",species),"occ_thinned_USmap2.jpg"),plot=p)
  
  # Create a plot focusing on state
  # Transform the CRS of your datasets to match that of the state boundary
  data_sf <- st_transform(data_sf, crs = st_crs(state))
  # Spatial join to check which points are within state
  data_within_state <- st_join(data_sf, state, join = st_within)
  # Filter to keep only those points within state
  data_within_state <- data_within_state[!is.na(data_within_state$NAME), ]
  
  p2 <- ggplot() +
    geom_sf(data = state, fill = "lightgray", color = "black", size = 1) +  # Alabama map
    geom_sf(data = data_within_state, shape = 19, alpha = 0.5, size = 3) +  # occurrance data within state
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  # Remove grid lines
      panel.background = element_blank(),  # Remove panel background
      plot.background = element_rect(fill='transparent', color=NA),
      panel.border = element_rect(colour = "black", fill=NA, size=1), # Adds box boundary
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),  # Remove axis texts
      legend.position = "none",  # Remove legend
    )
  ggsave(file.path(summary.dir,paste0(stateID,"_",species),"occ_thinned_statemap.jpg"),plot=p2,bg='transparent')
}


all_stateID_species = read.csv(paste0('inputs/state_',ForI,'_all_plots.csv'))
len = nrow(all_stateID_species)
for (i in (1:len)){
  stateID <- all_stateID_species$ID[i]
  state.name <- all_stateID_species$state[i]
  species<- all_stateID_species$spec_name[i]
  print(paste0(i,"_",stateID,"_",state.name,"_",species))
  plot_occ_map(stateID,state.name,species)
}

