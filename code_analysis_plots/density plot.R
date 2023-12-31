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

tic("total")
# create parser object
parser <- ArgumentParser()
# specify our desired options 
# by default ArgumentParser will add an help option
parser$add_argument("--account", default='xge', type="character")
parser$add_argument("--speciestype", default='state_flowers_single', type="character")
parser$add_argument("--run_loc", default='loc', type="character", help='loc or cc(compute canada)')
parser$add_argument("--no_cores", default=8, type='integer')

args <- parser$parse_args()
stateID <- args$stateID
speciestype <-args$speciestype
account<- args$account
run_loc <- args$run_loc

species_list <- read.csv(paste0('inputs/',speciestype,'.csv'),header = TRUE)
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

get_state_proj <- function(stateID){
  # crop the region of continental_us
  state.name <- filter(species_list, ID==stateID)$state
  state<- us_map[us_map$NAME == state.name, ]
  species <- filter(species_list, ID==stateID)$spec_name
  
  hist_proj <- terra::rast(paste0('outplots/',species,'/ensmean_hist/omth10.tif'))
  hist_projUS <- crop(hist_proj, extent(us_map))  
  hist_projState <- crop(hist_projUS, extent(state)) 
  fut_proj <- terra::rast(paste0('outplots/',species,'/2081-2100_ssp585/omth10.tif'))
  fut_projUS <- crop(fut_proj, extent(us_map))  
  fut_projState <- crop(fut_projUS, extent(state))
  hist_state_value <- values(hist_projState)
  fut_state_value <- values(fut_projState)
  
  hist_state <- as.data.frame(hist_state_value)
  hist_state$Climate <- '1971-2000'
  hist_state$State <- stateID
  fut_state <- as.data.frame(fut_state_value)
  fut_state$Climate <- '2081-2100'
  fut_state$State <- stateID
  
  hist_state2 <- as.data.frame(hist_state_value)
  hist_state2$Climate <- '1971-2000'
  hist_state2$State <- stateID
  fut_state2 <- as.data.frame(fut_state_value)
  fut_state2$Climate <- '2081-2100'
  fut_state2$State <- stateID
  df <- rbind(hist_state,fut_state)
  names(df) <- c('value','climate','state')
  return(df)
}

stateIDlist <- c('AL','CA','CO','ID','KS','ME','MN','MO','NC','NV','OR','SC','TN','VA','WV','WY')

df_all <- c()
for (stateID in stateIDlist){
  print(stateID)
  df <- get_state_proj(stateID)
  df_all <- rbind(df_all,df)
}
df_all <- na.omit(df_all) 

'DE', 'GA'
library(ggridges)

ggplot(df_all, aes(x = value, y = state)) + 
  geom_density_ridges(aes(fill = climate), alpha = 0.5) + # Use alpha for transparency
  scale_fill_viridis_d() + # A color scale that works well for factors
  theme_ridges()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(colour = "black", fill=NA, size=1) # Add panel border
  ) # A theme that's appropriate for ridge plots



