rm()
rm(list=ls())
library(dplyr)
library(raster)
library(usdm)
library(biomod2)
library(argparse)
library(tictoc)

tic("total")
# create parser object
parser <- ArgumentParser()
# specify our desired options 
# by default ArgumentParser will add an help option
parser$add_argument("--account", default='xge', type="character")
parser$add_argument("--run_loc", default='cc', type="character", help='loc or cc(compute canada)')
args <- parser$parse_args()
account<- args$account
run_loc <- args$run_loc

if (run_loc=='cc'){
  source(paste0("/home/",account,"/scratch/state_species_2023/codes/all_functions.R"))
  setwd(paste0("/home/",account,"/scratch/state_species_2023"))
}else{
  source("/Volumes/UoG_xuezhen/state_species_2023/codes/all_functions.R")
  setwd("/Volumes/UoG_xuezhen/state_species_2023")
}

speciestype = 'state_flowers'
species.folder <- "outputs"
spec_list<- list.files(species.folder)

bio1 <- raster("inputs/worldclim2/usa/1971-2000/ensmean/hist/bio/bio1.tif")
no.records <- data.frame()
for (species in spec_list){
  #get cleaneda occ data
  occ.cleaned <- read.csv(file.path('outputs',species,'data_cleaning/data_cleaned_coord_global.csv'))
  no.global_cleaned <- nrow(occ.cleaned)
  no.us_cleaned <- nrow(filter(occ.cleaned, countryCode=='US'))
  # get species thinned occ data
  spg.cc <- read.csv(file.path('outputs',species,'data_thinning/data_thinned.csv'))
  no.global_thinned <- nrow(spg.cc)
  ex_env <- raster::extract(bio1,spg.cc)
  spg.env <-cbind(spg.cc,ex_env)
  spg.cc <- na.omit(spg.env)
  spg.cc <- spg.cc[,c('lon','lat')]
  spg.cc$species.id <- 1
  no.us_thinned <- nrow(spg.cc)
  no.record <- data.frame(Species = species, Global_cleaned = no.global_cleaned, US_cleaned = no.us_cleaned, Global_thinned=no.global_thinned, US_thinned = no.us_thinned)
  no.records <- rbind(no.records, no.record)
}

if(!exists('summary_table')) dir.create('summary_table',recursive = TRUE)
summary.file <- file.path(paste0('summary_table/', speciestype, '_occ_records_summary.csv'))
write.csv(no.records,summary.file ,row.names = FALSE)