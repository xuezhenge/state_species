rm(list=ls())
library(rgbif)
library(usdm)
library(CoordinateCleaner)
library(raster)
library(BiodiversityR)
library(spThin)
library(ggplot2)
library(biomod2)
library(tidyverse)
library(blockCV)
library(sf)
library(terra)
library(tidyr)
library(plyr)
library(rgdal)
library(dismo)
library(tools)
library(dplyr)
library(RColorBrewer)
library(rnaturalearth)
library(parallel)
library(foreach)
library(doParallel)
library(argparse)
library(tictoc)

library(tigris)
options(tigris_use_cache = TRUE)

tic("total")
# create parser object
parser <- ArgumentParser()
# specify our desired options 
# by default ArgumentParser will add an help option
parser$add_argument("--account", default='xge', type="character")
parser$add_argument("--stateID", default='MI1', type="character")
parser$add_argument("--ForI", default='flowers', type="character")
parser$add_argument("--speciestype", default='state_flowers_multiple', type="character")
parser$add_argument("--run_loc", default='loc', type="character", help='loc or cc(compute canada)')
parser$add_argument("--no_cores", default=12, type='integer')

args <- parser$parse_args()
stateID <- args$stateID
speciestype <-args$speciestype
account<- args$account
run_loc <- args$run_loc
ForI <- args$ForI # flowers or insects

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

if (ForI == 'flowers'){
  out_folder = 'outputs_flowers'
}else{
  out_folder = 'outputs_insects'
}

spec_list <- list.files(path = out_folder, full.names = TRUE, recursive = FALSE)
for (species in spec_list){
  om_alls <- read.csv(file.path(species,'summary_doc','omrates.csv'))
  om_sel <- filter(om_alls,om10 <= 0.1)
  len <- nrow(om_sel)
  print(paste0(species,":", len))
}
