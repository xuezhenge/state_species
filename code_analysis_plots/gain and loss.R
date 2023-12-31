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
library(reshape2)
library(dplyr)
library(tidyr)


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

species_list <- read.csv(paste0('inputs/',speciestype,'.csv'),header = TRUE)
# species <- filter(species_list, ID==stateID)$spec_name
# state <- filter(species_list, ID==stateID)$state

#us map
us_map <- st_as_sf(tigris::states(cb = TRUE))
# continental_us
us_map <- us_map %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))



get_rangechange <- function(stateID){
  # crop the region of continental_us
  state.name <- filter(species_list, ID==stateID)$state
  species <- filter(species_list, ID==stateID)$spec_name
  
  hist_proj <- terra::rast(paste0('outplots/',species,'/ensmean_hist/omth10.tif'))
  #make sure all the maps have the same crs
  crs_raster <- crs(hist_proj)
  if (st_crs(us_map) != crs_raster) {
    us_map_transformed <- st_transform(us_map, crs_raster)
  } else {
    us_map_transformed <- us_map
  }
  state <- us_map_transformed[us_map_transformed$STUSPS == stateID, ]
  
  # clip raster based on US map and state map
  hist_projUS <- crop(hist_proj, extent(us_map_transformed))
  hist_projUS <- mask(hist_projUS, us_map_transformed)
  plot(hist_projUS)
  hist_projState <- crop(hist_projUS, extent(state))
  hist_projState <- mask(hist_projState,state)
  plot(hist_projState)
  
  # Convert the continious raster to binary raster
  threshold = 0.2
  binary_hist_raster <- app(hist_projState, function(x) as.integer(x >= threshold))
  plot(binary_hist_raster)
  
  # repeat the same thing for the future raster plot
  fut_proj <- terra::rast(paste0('outplots/',species,'/2081-2100_ssp585/omth10.tif'))
  fut_projUS <- crop(fut_proj, extent(us_map_transformed)) 
  hist_projUS <- mask(fut_projUS, us_map_transformed)
  fut_projState <- crop(fut_projUS, extent(state))
  fut_projState <- mask(fut_projState,state)
  binary_fut_raster <- app(fut_projState, function(x) as.integer(x >= threshold))
  plot(binary_fut_raster)
  
  myBiomodRangeSize <- BIOMOD_RangeSize(proj.current = binary_hist_raster, proj.future = binary_fut_raster)
  PercLoss <- myBiomodRangeSize$Compt.By.Models[5]
  PercGain <- myBiomodRangeSize$Compt.By.Models[6]
  PercStable <- 100 - PercLoss - PercGain
  RangeChange <- data.frame(State = stateID, Species = species, Loss = PercLoss, Stable = PercStable, Gain = PercGain)
  return(RangeChange)
}

stateIDlist <- c('IA','GA','ID','MA','MN','SC','UT','WV','WY')
species_list <- read.csv(paste0('inputs/','state_flowes_single','.csv'),header = TRUE)
RangeChange_all <- data.frame()
for (stateID in stateIDlist){
  RangeChange <- get_rangechange(stateID)
  RangeChange_all <- rbind(RangeChange_all,RangeChange)
}
stateIDlist = c('LA')
species_list <- read.csv(paste0('inputs/','state_flowers_shared','.csv'),header = TRUE)
for (stateID in stateIDlist){
  RangeChange <- get_rangechange(stateID)
  RangeChange_all <- rbind(RangeChange_all,RangeChange)
}
stateIDlist = c('MI','SD')
species_list <- read.csv(paste0('inputs/','state_flowers_syns','.csv'),header = TRUE)
for (stateID in stateIDlist){
  RangeChange <- get_rangechange(stateID)
  RangeChange_all <- rbind(RangeChange_all,RangeChange)
}

df = RangeChange_all
# Reshape the data from wide to long
long_df <- melt(df, id.vars = c("State", "Species"))
# Create the Likert-like plot
ggplot(long_df, aes(x = State, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips the axes so that states are on the y-axis
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")  # Uses a color palette that is visually appealing

# Assuming your data frame is named 'df'
df <- df %>%
  mutate(Loss = -Loss) # Make Loss negative for plotting on the left

# Reshape the data from wide to long
long_df <- df %>%
  gather(key = "Category", value = "Percentage", -State, -Species)

# Create the Likert-like plot
ggplot(long_df, aes(x = State, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", abs(Percentage))), position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = abs, breaks = seq(-100, 100, by = 25)) + # Correct the labels to be positive
  coord_flip() +  # Flip the coordinates to put states on the y-axis
  labs(x = NULL, y = "Percentage", fill = "Category") + # Add labels
  scale_fill_manual(values = c("Loss" = "lightblue", "Stable" = "grey92", "Gain" = "tan3")) + # Set custom colors
  theme_minimal()+ # Use a minimal theme for a clean look
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(colour = "black", fill=NA, size=1) # Add panel border
  )

