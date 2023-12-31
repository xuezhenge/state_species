rm(list = ls())
library(raster)
library(rgdal)
library(raster)
library(terra)
library(sf)

run_loc = 'loc'

if (run_loc=='cc'){
  source(paste0("/home/",account,"/scratch/state_species_2024/codes/all_functions.R"))
  setwd(paste0("/home/",account,"/scratch/state_species_2024"))
  n.cores <- args$no_cores
}else{
  # source("/Users/xuezhenge/Desktop/state_species_2024/codes/all_functions.R")
  # setwd("/Users/xuezhenge/Desktop/state_species_2024")
  source("/Volumes/UoG_xuezhen/state_species_2024/codes/all_functions.R")
  setwd("/Volumes/UoG_xuezhen/state_species_2024")
  n.cores <- 2
}

time.periods <- c('1981-2010','2071-2100')
rcps <- c('hist','ssp585')
#usa <- readOGR(dsn ='inputs/shapefile/usa_shapefile', layer = "usa_state_shapefile")
usa <- st_read('inputs/shapefile/usa_shapefile/usa_state_shapefile.shp')
#projection(usa) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
climate_dir <- paste0("inputs/chelsa/world/1981-2010/ensmean/hist/bio")
fns<- list.files(climate_dir)

time.period <- time.periods[1]
rcp <- rcps[1]
input.dir <- file.path("inputs/chelsa/world",time.period,'ensmean',rcp,'bio')
output.dir <- file.path("inputs/chelsa/usa",time.period,'ensmean',rcp,'bio')
if(!exists(output.dir)) dir.create(output.dir,recursive = TRUE)
for (fn in fns){
  bio <- raster(paste0(input.dir,'/',fn))
  # Crop elevation data by extent of state
  bio.sub <- crop(bio, extent(usa))
  bio.sub <- mask(bio.sub, usa)
  writeRaster(bio.sub, filename=file.path(output.dir, fn), overwrite=TRUE)
}
