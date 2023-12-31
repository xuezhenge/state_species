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
parser$add_argument("--stateID", default='WV', type="character")
parser$add_argument("--speciestype", default='state_flowers_single', type="character")
parser$add_argument("--run_loc", default='cc', type="character", help='loc or cc(compute canada)')
args <- parser$parse_args()
stateID <- args$stateID
speciestype <-args$speciestype
account<- args$account
run_loc <- args$run_loc

if (run_loc=='cc'){
  source(paste0("/home/",account,"/scratch/state_species_2023/codes/all_functions.R"))
  setwd(paste0("/home/",account,"/scratch/state_species_2023"))
}else{
  source("/Volumes/UoG_xuezhen/state_species_2023/codes/all_functions.R")
  setwd("/Volumes/UoG_xuezhen/state_species_2023")
}

species_list <- read.csv(paste0('inputs/',speciestype,'.csv'),header = TRUE)
species <- filter(species_list, ID==stateID)$spec_name
state <- filter(species_list, ID==stateID)$state

if(!exists(paste0("log_files/",speciestype))) dir.create(paste0("log_files/",speciestype),recursive = TRUE)
log_con <- file(paste0("log_files/",speciestype,"/",stateID,"_",species,".txt"), open="a")

#-----Step 1: select climate variables--------------------------------------
# 1.load occurrence data
spg <- read.csv(file.path('outputs',species,'data_thinning/data_thinned.csv'),header=TRUE)
spg$species.id <- 1
spg.cc <- spg
no.records <- nrow(spg)
coordinates(spg) <- c('lon','lat')
# 2.prepare environment data-------
#chelsa climate data
hist.dir <- "inputs/chelsa/world/1981-2010/ensmean/hist/bio"
sp.bios.dir <- file.path('outputs',species,"sp_bios_names")
if(!exists(sp.bios.dir)) dir.create(sp.bios.dir)
fns<- list.files(hist.dir)
climate.dir <- file.path(hist.dir,fns)
bios.hist <- raster::stack(climate.dir)
names(bios.hist) <- c('bio1','bio10','bio11','bio12','bio13','bio14','bio15',
                       'bio16','bio17','bio18','bio19','bio2','bio3','bio4','bio5',
                       'bio6','bio7','bio8','bio9')
# 3.select historical environment variables based on VIF (multi-collinearity)
# multicollinearity
# multicollinearity (vifstep or vifcor): variance inflation factor
#vifstep: if the VIF is greater than the threshold (default:10), the variable should be excluded
sel_ev_hist<- function(bios.hist){
  ex <- raster::extract(bios.hist,spg)
  head(ex)
  ex <- na.omit(ex)
  head(ex)
  #identify the variables which have collinearity problem
  v <- vifstep(ex, th=4)
  v
  #exclude these variables
  bios.hist <- exclude(bios.hist,v)
  return(bios.hist)
}
bios.hist <- sel_ev_hist(bios.hist)
sel.evnames <- names(bios.hist)
#save the select climate variables names
write.csv(sel.evnames,file.path(sp.bios.dir,'hist_bio_names.csv'))

#----Step 2: generate dataset that contains presence and pseudo-absence data----
#1. load climate variables
# import the selected climate variable names
hist.dir <- "inputs/chelsa/world/1981-2010/ensmean/hist/bio"
sel.evnames <- read.csv(file.path('outputs',species,'sp_bios_names/hist_bio_names.csv'))
sel.evnames <- as.vector(sel.evnames$x)
fns<- paste0(sel.evnames,".tif")
climate.dir <- paste0(hist.dir,'/',fns)
bios.hist <- raster::stack(climate.dir)
#2.format occurrence data----------
# Get corresponding presence/absence data
# load occurrence data
spg.cc <- read.csv(file.path('outputs',species,'data_thinning/data_thinned.csv'))
ex_env <- raster::extract(bios.hist,spg.cc)
spg.env <-cbind(spg.cc,ex_env)
spg.cc <- na.omit(spg.env)
spg.cc <- spg.cc[,c('lon','lat')]
spg.cc$species.id <- 1
no.records <- nrow(spg.cc)
myResp <- spg.cc['species.id']
# Get corresponding XY coordinates
myRespXY <- spg.cc[, c('lon', 'lat')]
no.records <- nrow(spg.cc)
#3. generate presence pseudo-absence data
#4. generate presence pseudo-absence data and write pre-pa data into csv format
# Get pseudo -absence for different models when generating pseudo-absence
#Group A: 'MaxEnt','BIOCLIM','SRE','GLM','GAM','GBM',
#GroupB: 'MARS'
#Group C: 'CTA','FDA','ANN', 'RF'
get_pre_pa('GroupA',no.records,reps=20,myResp,myRespXY,bios.hist,species)
get_pre_pa('GroupB',no.records,reps=20,myResp,myRespXY,bios.hist,species)
get_pre_pa('GroupC',no.records,reps=20,myResp,myRespXY,bios.hist,species)
cat(paste0("-----Step 2: Have generated PA data-------------"), file = log_con,sep="\n")
time <- toc()
cat(time$callback_msg,file = log_con,sep="\n")

