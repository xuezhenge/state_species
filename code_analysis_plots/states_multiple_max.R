rm()
rm(list=ls())
library(terra)
library(rgdal)
library(dismo)
library(tools)
library(dplyr)
library(tictoc)
library(introdataviz)
library(sf)
library(RColorBrewer)
library(ggplot2)


setwd("/Volumes/UoG_xuezhen/state_species_2024/git/state_species")
ForI = 'flowers'

if (ForI == 'flowers'){
  out_folder = 'outplots_flowers'
  outplot_folder = 'figure_flowers'
}else{
  out_folder = 'outplots_insects'
  outplot_folder = 'figure_insects'
}

get_multi_max<- function(multi_spec_ID,species_list,genus,time.period,scenario){
  rasters <- rast(paste0(out_folder,'/',species_list,'/',time.period,'_',scenario,'/omth10.tif'))
  raster_max <- max(rasters)
  # save the raster file
  outraster.dir = paste0(out_folder,'/',genus,'/',time.period,'_',scenario)
  if(!exists(outraster.dir)) dir.create(outraster.dir,recursive = TRUE)
  writeRaster(raster_max, filename=file.path(outraster.dir,'omth10.tif'), overwrite=TRUE)
}

# input files
all_stateID_species = read.csv(paste0('inputs/state_',ForI,'_all_plots.csv'))
multi_genus = read.csv(paste0('inputs/state_',ForI,'_multi_plots.csv'))
multi_spec_IDs <- c('TX','MI','IN','ND','NM')

for (multi_spec_ID in multi_spec_IDs){
  print(multi_spec_ID)
  sel_species <- filter(all_stateID_species,ID==multi_spec_ID)
  species_list <- sel_species$spec_name
  genus <- filter(multi_genus,ID==multi_spec_ID)$spec_name
  #this run is actually for historical period (1981-2010)
  get_multi_max(multi_spec_ID,species_list,genus,'ensmean','hist')
  get_multi_max(multi_spec_ID,species_list,genus,'2071-2100','ssp585')
}


