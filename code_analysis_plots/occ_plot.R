library(ggplot2)
library(sf)
library(dplyr)

setwd("/Volumes/UoG_xuezhen/state_species_2023")
species <- 'Camellia japonica'
df <- read.csv(paste0('outputs/',species,'/data_thinning/data_thinned.csv'))

us_shape <- st_read("inputs/shapefile/States_shapefile-shp/States_shapefile.shp")

points_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = st_crs(us_shape))
filtered_points <- st_join(points_sf, us_shape, join = st_within)
filtered_points <- filtered_points[!is.na(filtered_points$id), ]  # Assuming 'id' is a column in 'shape'

shape <- st_read("path/to/your/shapefile.shp")


# Get US map data
us_map <- map_data("state")

# Plot
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +  # US boundaries
  geom_point(data = occs, aes(x = lon, y = lat), 
             color = "red") +  # Your data points
  coord_fixed(1.3) +  # Aspect ratio
  theme_minimal() +
  labs(title = "Points on US Map") +
  theme(panel.background = element_rect(fill = "lightblue"), 
        plot.background = element_rect(fill = "lightblue"))

