library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)
library(dplyr)
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)


setwd('/Users/xuezhenge/Desktop/application_paper/comparison/')
species_list <- list.files('inputs')
for (id in 1:5){
  species <- species_list[id]
  
  # remove data in Antarctica
  world_map <- ne_countries(scale = "small", returnclass = "sf")
  world_map <- world_map %>% filter(iso_a3 != "ATA")
  world_map  <- as(world_map, "Spatial")
  
  # Replace 'path/to/your/raster.tif' with the path to your raster file
  raster_data <- raster(paste0("inputs/",species,"/2081-2100_ssp585_10/omth10.tif"))
  #raster_data <- raster(paste0("inputs/",species,"/ensmean_hist_10/omth10.tif"))
  
  raster_data2 <- crop(raster_data, extent(world_map))
  raster_data <- mask(raster_data2, world_map)
  #get data
  raster_values <- getValues(raster_data)
  raster_coords <- xyFromCell(raster_data, 1:ncell(raster_data))
  df <- data.frame(longitude = raster_coords[, 1], latitude = raster_coords[, 2], value = raster_values)
  df <- na.omit(df)
  # plot range
  world_map <- ne_countries(scale = "small", returnclass = "sf")
  # Filter out Antarctica
  world_map <- world_map %>% filter(iso_a3 != "ATA")
  
  my.palette <- brewer.pal(n = 9, name = "OrRd")
  
  my_plot <- ggplot() +
    geom_sf(data = world_map, fill = "#E8E8E8", color = "white", size = 0.5) +
    geom_raster(data = df, aes(x = longitude, y = latitude, fill = value)) +
    # Border for the countries
    geom_sf(data = world_map, fill = NA, color = "white", size = 0.5) +
    scale_fill_gradientn(colours = my.palette, limits = c(0, 1)) +
    theme_minimal() +
    theme(
      legend.position = "None",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2),
      panel.background = element_rect(fill = 'white') #"#E6E6FA"
    ) +
    coord_sf(expand = c(0, 0))+
    annotate("text", x = -155, y = -40, label = paste0("(c",id,") "), size = 12, color = "black")
  
  plot.dir = paste0('outplots/correlative_plots/',species,'_2081-2100.jpg')
  #plot.dir = paste0('outplots/correlative_plots/',species,'_1971-2000.jpg')
  ggsave(plot.dir, my_plot, dpi = 300, width = 9.6, height = 4, units = "in")
}
