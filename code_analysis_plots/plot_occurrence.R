rm(list=ls())
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(argparse)
library(tigris)
options(tigris_use_cache = TRUE)

# create parser object
parser <- ArgumentParser()
# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("--stateID", default='AL', type="character")
args <- parser$parse_args()
stateID <- args$stateID

source("/Users/xuezhenge/Desktop/state_species/codes/all_functions.R")
setwd("/Volumes/UoG_xuezhen/state_species_2023")
species_list <- read.csv(paste0('inputs/state_flowers_single.csv'),header = TRUE)
species <- filter(species_list, stateID==stateID)$spec_name

#-----Step 2.3: plot thinned occurrence records in Unite States---------------
data_1981_2010 <- read.csv(paste0('outputs/',species,'/data_thinning/data_thinned_1981-2010.csv'))
data_2011_2023 <- read.csv(paste0('outputs/',species,'/data_thinning/data_thinned_2011-2023.csv'))

data_1981_2010_sf <- st_as_sf(data_1981_2010, coords = c("lon", "lat"), crs = 4326)
data_2011_2023_sf <- st_as_sf(data_2011_2023, coords = c("lon", "lat"), crs = 4326)
num.train <- nrow(data_1981_2010_sf)
num.valid <- nrow(data_2011_2023_sf)

summary.dir <- file.path("outplots",'occ_plots')
if(!exists(summary.dir)) dir.create(summary.dir)

world <- ne_countries(scale = "medium", returnclass = "sf")
usa <- world[world$admin == "United States of America", ]
# Load US state boundaries
states <- tigris::states(cb = TRUE, class = "sf")
state.name <- get_statename(stateID)
state<- states[states$NAME == state.name, ]


# Plot the occurrence data on the world map
p <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +  # World map
  geom_sf(data = data_1981_2010_sf, aes(color = "1981-2010"), shape = 19, alpha = 0.5, size = 0.5) +  # 1981-2010 data
  geom_sf(data = data_2011_2023_sf, aes(color = "2011-2023"), shape = 19, alpha = 0.5, size = 0.5) +   # 2011-2023 data
  geom_sf(data = usa, fill = NA, color = "black", size = 1) +   # Highlight USA
  geom_sf(data = state, fill = NA, color = "black", size = 2) +
  scale_color_manual(values = c("1981-2010" = "blue", "2011-2023" = "red"),
                     name = "Time period", 
                     labels = c(paste0("1981-2010 (Training: ", num.train," records)"), 
                                paste0("2011-2023 (Validation: ", num.valid," records)"))) +
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
ggsave(paste0(summary.dir,"/", species, "_thinned_occ_worldmap.jpg"), plot = p, width = 7, height = 3.5)


# Transform the CRS of your datasets to match that of the state boundary
data_1981_2010_sf <- st_transform(data_1981_2010_sf, crs = st_crs(state))
data_2011_2023_sf <- st_transform(data_2011_2023_sf, crs = st_crs(state))

# Spatial join to check which points are within state
data_1981_2010_within_state <- st_join(data_1981_2010_sf, state, join = st_within)
data_2011_2023_within_state <- st_join(data_2011_2023_sf, state, join = st_within)


# Filter to keep only those points within state
data_1981_2010_within_state <- data_1981_2010_within_state[!is.na(data_1981_2010_within_state$NAME), ]
data_2011_2023_within_state <- data_2011_2023_within_state[!is.na(data_2011_2023_within_state$NAME), ]

# Create a plot focusing on state
p2 <- ggplot() +
  geom_sf(data = state, fill = "lightgray", color = "black", size = 1) +  # Alabama map
  geom_sf(data = data_1981_2010_within_state, aes(color = "1981-2010"), shape = 19, alpha = 0.5, size = 3) +  # 1981-2010 data within state
  geom_sf(data = data_2011_2023_within_state, aes(color = "2011-2023"), shape = 19, alpha = 0.5, size = 3) +  # 2011-2023 data within state
  scale_color_manual(values = c("1981-2010" = "blue", "2011-2023" = "red"),
                     name = "Dataset", 
                     labels = c("1981-2010", "2011-2023")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_blank(),  # Remove panel background
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),  # Remove axis texts
    legend.position = "none",  # Remove legend
  )
ggsave(paste0(summary.dir,"/",species,"_thinned_occ_statemap.jpg"),plot=p2)
