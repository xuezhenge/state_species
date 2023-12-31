rm()
rm(list=ls())
library(terra)
library(rgdal)
library(dismo)
library(tools)
library(dplyr)
library(raster)
library(tictoc)
library(ggridges)

ForI <- 'insects'
setwd("/Volumes/UoG_xuezhen/state_species_2024/git/state_species")

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
# Load US state boundaries
states <- tigris::states(cb = TRUE, class = "sf")

get_state_proj <- function(stateID,state.name,species){
  state<- states[states$NAME == state.name, ]
  # crop the region of continental_us
  hist_proj <- terra::rast(paste0(out_folder,'/',species,'/ensmean_hist/omth10.tif'))
  hist_projUS <- crop(hist_proj, extent(us_map))  
  hist_projState <- crop(hist_projUS, extent(state)) 
  fut_proj <- terra::rast(paste0(out_folder,'/',species,'/2071-2100_ssp585/omth10.tif'))
  fut_projUS <- crop(fut_proj, extent(us_map))  
  fut_projState <- crop(fut_projUS, extent(state))
  hist_state_value <- values(hist_projState)
  fut_state_value <- values(fut_projState)
  
  hist_state <- as.data.frame(hist_state_value)
  hist_state$Climate <- '1981-2010'
  hist_state$State <- stateID
  fut_state <- as.data.frame(fut_state_value)
  fut_state$Climate <- '2071-2100'
  fut_state$State <- stateID
  
  hist_state2 <- as.data.frame(hist_state_value)
  hist_state2$Climate <- '1981-2010'
  hist_state2$State <- stateID
  fut_state2 <- as.data.frame(fut_state_value)
  fut_state2$Climate <- '2071-2100'
  fut_state2$State <- stateID
  df <- rbind(hist_state,fut_state)
  names(df) <- c('value','climate','state')
  return(df)
}

all_stateID_species = read.csv(paste0('inputs/state_',ForI,'_analysis.csv'))
len = nrow(all_stateID_species)

df_all <- data.frame()
for (i in (1:len)){
  stateID <- all_stateID_species$ID[i]
  state.name <- all_stateID_species$state[i]
  species<- all_stateID_species$spec_name[i]
  print(paste0(i,"_",stateID,"_",state.name,"_",species))
  df <- get_state_proj(stateID,state.name,species)
  df_all <- rbind(df_all,df)
}

df_all <- na.omit(df_all) 

ggplot(df_all, aes(x = value, y = state)) + 
  geom_density_ridges(aes(fill = climate), alpha = 0.5) + # Use alpha for transparency
  scale_fill_viridis_d() + # A color scale that works well for factors
  theme_ridges()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(colour = "black", fill=NA, size=1) # Add panel border
  ) # A theme that's appropriate for ridge plots

