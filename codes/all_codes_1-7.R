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
parser$add_argument("--stateID", default='DE', type="character")
parser$add_argument("--ForI", default='flowers', type="character")
parser$add_argument("--speciestype", default='state_flowers_single', type="character")
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
species_list <- read.csv(paste0('inputs/',speciestype,'.csv'),header = TRUE)
species <- filter(species_list, ID==stateID)$spec_name
state <- filter(species_list, ID==stateID)$state


if(!exists(paste0("log_files/",speciestype))) dir.create(paste0("log_files/",speciestype),recursive = TRUE)
log_con <- file(paste0("log_files/",speciestype,"/",stateID,"_",species,".txt"), open="a")


# #---------Code 01: 01occ_download_cleaning_thinning.R
# #=========Step 1: Download raw occ==================
# cleaned.file <- file.path(out_folder,species,'data_cleaning/data_cleaned_coord_global.csv')
# if (!file.exists(cleaned.file)){
#   occs <- occ_search(scientificName = species,hasCoordinate = TRUE,limit = 50000)
#   occs <- occs$data
#   no.raw_records <- nrow(occs) #number of records with coordinates
#   if (no.raw_records == 50000){
#     cat(paste0("-----------Step 1: Please download the raw occ data for ",species," mannully-------------"), file = log_con,sep="\n")
#   }else{
#     colnames(occs) #Column names returned from gbif follow the DarwinCore standard
#     #exporting raw data
#     clean.dir <- file.path(out_folder,species,'data_cleaning')
#     if(!exists(clean.dir)) dir.create(clean.dir, recursive = TRUE)
#     write.csv(occs, raw.file, row.names = FALSE)
#     cat(paste0("-----------Have downloaded the raw occ data for ",species,"-------------"), file = log_con,sep="\n")
#   }
# }else{
#   cat(paste0("-----Raw file exists!-------------"), file = log_con,sep="\n")
# }

# #==========Step 2: Occ records cleanning =================
# if (!file.exists(cleaned.file)){
#   occs <- read.csv(file.path(out_folder,species,'data_cleaning/raw_data_global.csv'))
#   no.raw_records <- nrow(occs)
#   occs$decimalLatitude <- as.numeric(occs$decimalLatitude)
#   occs$decimalLongitude <- as.numeric(occs$decimalLongitude)
#   occs <- filter(occs, decimalLongitude < 180 & decimalLongitude > -180)
#   occs <- filter(occs, decimalLatitude < 90 & decimalLatitude > -90)
#   #--------2.1 Checking species' coordinates-----------
#   geo.clean <- clean_coordinates(x = occs,
#                                  lon = "decimalLongitude",
#                                  lat = "decimalLatitude",
#                                  species = "species",
#                                  countries = "countryCode",
#                                  value = "clean",
#                                  tests = c("capitals","centroids","equal","gbif","institutions","seas","zeros"))
#   # export the global data after coordinate check
#   occs.out<- geo.clean %>% dplyr::select(scientificName,species, decimalLongitude,decimalLatitude,countryCode,year)
#   # remove spatial duplicates
#   occs.out<- occs.out[!duplicated(occs.out),]
#   # export the global data after coordinate check
#   write.csv(occs.out, cleaned.file, row.names = FALSE)
#   cat(paste0("-----------Have cleaned the raw occ data for ",species,"-------------"), file = log_con,sep="\n")
# }else{
#   cat(paste0("-----Cleaned file exists!-------------"), file = log_con,sep="\n")
# }
# #-----3 data thinning------------------------------
# occs.clean <- read.csv(file.path(out_folder,species,'data_cleaning/data_cleaned_coord_global.csv'))
# if (species == 'Citrus sinensis'){
#   occ.orange_production <- read.csv(file.path(out_folder,species,'data_cleaning/county_coordinates_all.csv'))
#   no.cleaned_records <- nrow(occs.clean) + nrow(occ.orange_production)
#   no.us_records <- nrow(filter(occs.clean, countryCode=='US')) + nrow(occ.orange_production)
# }else{
#   no.cleaned_records <- nrow(occs.clean)
#   no.us_records <- nrow(filter(occs.clean, countryCode=='US'))
# }

# cat(paste0("-----Number of world cleaned data:",no.cleaned_records,"-------------"), file = log_con,sep="\n")
# cat(paste0("-----Number of us cleaned data:",no.us_records,"-------------"), file = log_con,sep="\n")

# thinned.file <- file.path(out_folder,species,'data_thinning/data_thinned.csv')
# if (!file.exists(thinned.file)){
#   occs_all <- read.csv(file.path(out_folder,species,'data_cleaning/data_cleaned_coord_global.csv'))
#   colnames(occs_all) <- c("scientificname","spec","lon",'lat','countryCode','year')
#   output.dir = file.path(out_folder,species,'data_thinning')
#   if(!exists(output.dir)) dir.create(output.dir,recursive = TRUE)
#   occs <- data.frame(occs_all$lon,occs_all$lat)
#   colnames(occs) <- c('lon','lat')
#   occs <- distinct(occs)
#   if (species == 'Citrus sinensis'){
#     occs_add <- read.csv(file.path(out_folder,species,'data_cleaning/county_coordinates_all.csv'))
#     occs <- rbind(occs,occs_add)
#   }
#   no.records <- nrow(occs)
#   print(no.records)
#   no.records <- nrow(occs)
#   print(no.records)
#   if (no.records > 50000){
#     data_chunks <- split(occs, cut(1:nrow(occs), 200)) # Split data
#     thinned_all <- c()
#     for (i in 1:200){
#       thinned <- ensemble.spatialThin.quant(data_chunks[[i]], thin.km = 10, runs = 1,
#                                             silent = FALSE, verbose = FALSE, LON.length = 21, LAT.length = 41)
#       thinned_all <- rbind(thinned_all, thinned)
#     }

#     data_chunks <- split(thinned_all, cut(1:nrow(thinned_all), 50)) # Split data
#     thinned_all <- c()
#     for (i in 1:50){
#       thinned <- ensemble.spatialThin.quant(data_chunks[[i]], thin.km = 10, runs = 1,
#                                             silent = FALSE, verbose = FALSE, LON.length = 21, LAT.length = 41)
#       thinned_all <- rbind(thinned_all, thinned)
#     }

#     data_chunks <- split(thinned_all, cut(1:nrow(thinned_all), 10)) # Split data
#     thinned_all <- c()
#     for (i in 1:10){
#       thinned <- ensemble.spatialThin.quant(data_chunks[[i]], thin.km = 10, runs = 1,
#                                             silent = FALSE, verbose = FALSE, LON.length = 21, LAT.length = 41)
#       thinned_all <- rbind(thinned_all, thinned)
#     }

#     thinned_data <- ensemble.spatialThin.quant(thinned_all, thin.km = 10,
#                                                runs = 1, silent = FALSE, verbose = FALSE,
#                                                LON.length = 21, LAT.length = 41)
#   }else if(no.records > 10000){
#     data_chunks <- split(occs, cut(1:nrow(occs), 50)) # Split data
#     cl <- makeCluster(n.cores)
#     registerDoParallel(cl)
#     thinned_all <- foreach(chunk = data_chunks, .combine = rbind, .packages = c("BiodiversityR")) %dopar% {
#       processed_chunk <- ensemble.spatialThin.quant(chunk, thin.km = 10, runs = 1,
#                                                     silent = FALSE, verbose = FALSE, LON.length = 21, LAT.length = 41)
#       return(processed_chunk)
#     }
#     data_chunks <- split(occs, cut(1:nrow(thinned_all), 10)) # Split data
#     cl <- makeCluster(n.cores)
#     registerDoParallel(cl)
#     thinned_all <- foreach(chunk = data_chunks, .combine = rbind, .packages = c("BiodiversityR")) %dopar% {
#       processed_chunk <- ensemble.spatialThin.quant(chunk, thin.km = 10, runs = 1,
#                                                     silent = FALSE, verbose = FALSE, LON.length = 21, LAT.length = 41)
#       return(processed_chunk)
#     }
#     thinned_data <- ensemble.spatialThin.quant(thinned_all, thin.km = 10,
#                                                runs = 1, silent = FALSE, verbose = FALSE,
#                                                LON.length = 21, LAT.length = 41)
#   }else{
#     thinned_data <- ensemble.spatialThin.quant(occs, thin.km = 10,
#                                                runs = 1, silent = FALSE, verbose = FALSE,
#                                                LON.length = 21, LAT.length = 41)
#   }
#   no.thinned <- nrow(thinned_data)
#   print(no.thinned)
#   write.csv(thinned_data,thinned.file,row.names = FALSE)
#   cat(paste0("-----------Step 1: Have cleaned and thinned raw data for",species,"-------------"), file = log_con,sep="\n")
# }else{
#   cat(paste0("-----Thinned file exists!-------------"), file = log_con,sep="\n")
# }
# occs.thin <- read.csv(file.path(out_folder,species,'data_thinning/data_thinned.csv'))
# no.thinned <- nrow(occs.thin)
# cat(paste0("-----Number of thinned data:",no.thinned,"-------------"), file = log_con,sep="\n")
# time <- toc()
# cat(time$callback_msg,file = log_con,sep="\n")

# #----------Code 02: 02generate_pseudo_absence.R
# tic("total")
# spg <- read.csv(file.path(out_folder,species,'data_thinning/data_thinned.csv'))
# spg <- na.omit(spg) 
# spg$species.id <- 1
# spg.cc <- spg
# no.records <- nrow(spg)
# coordinates(spg) <- c('lon','lat')
# # 2.prepare environment data-------
# #chelsa climate data
# hist.dir <- "inputs/chelsa/usa/1981-2010/ensmean/hist/bio"
# output.file <- file.path(out_folder,species,'pre_pa_csv/GroupC/pre_pa_GroupC20.csv')
# if (!file.exists(output.file)){
#   sp.bios.dir <- file.path(out_folder,species,"sp_bios_names")
#   if(!exists(sp.bios.dir)) dir.create(sp.bios.dir)
#   fns<- list.files(hist.dir)
#   climate.dir <- file.path(hist.dir,fns)
#   bios.hist <- raster::stack(climate.dir)
#   # 3.select historical environment variables based on VIF (multi-collinearity)
#   bios.hist <- sel_ev_hist(bios.hist)
#   sel.evnames <- names(bios.hist)
#   #save the select climate variables names
#   write.csv(sel.evnames,file.path(sp.bios.dir,'hist_bio_names.csv'))

#   #----Step 2: generate dataset that contains presence and pseudo-absence data----
#   #1. load climate variables
#   # import the selected climate variable names
#   hist.dir <- "inputs/chelsa/usa/1981-2010/ensmean/hist/bio"
#   sel.evnames <- read.csv(file.path(out_folder,species,'sp_bios_names/hist_bio_names.csv'))
#   sel.evnames <- as.vector(sel.evnames$x)
#   fns<- paste0(sel.evnames,".tif")
#   climate.dir <- paste0(hist.dir,'/',fns)
#   bios.hist <- raster::stack(climate.dir)
#   #2.format occurrence data----------
#   # Get corresponding presence/absence data
#   # load occurrence data
#   spg.cc <- read.csv(file.path(out_folder,species,'data_thinning/data_thinned.csv'))
#   ex_env <- raster::extract(bios.hist,spg.cc)
#   spg.env <-cbind(spg.cc,ex_env)
#   spg.cc <- na.omit(spg.env)
#   spg.cc <- spg.cc[,c('lon','lat')]
#   spg.cc$species.id <- 1
#   no.records <- nrow(spg.cc)
#   myResp <- spg.cc['species.id']
#   # Get corresponding XY coordinates
#   myRespXY <- spg.cc[, c('lon', 'lat')]
#   no.records <- nrow(spg.cc)
#   #3. generate presence pseudo-absence data
#   #4. generate presence pseudo-absence data and write pre-pa data into csv format
#   # Get pseudo -absence for different models when generating pseudo-absence
#   #Group A: 'MaxEnt','BIOCLIM'/'SRE','GLM','GAM','GBM',
#   #GroupB: 'MARS'
#   #Group C: 'CTA','FDA','ANN', 'RF'
#   get_pre_pa('GroupA',no.records,reps=20,myResp,myRespXY,bios.hist,species)
#   get_pre_pa('GroupB',no.records,reps=20,myResp,myRespXY,bios.hist,species)
#   get_pre_pa('GroupC',no.records,reps=20,myResp,myRespXY,bios.hist,species)
#   cat(paste0("-----Step 2: Have generated PA data-------------"), file = log_con,sep="\n")
# }else{
#   cat(paste0("-----PA data exists!-------------"), file = log_con,sep="\n")
# }

# time <- toc()
# cat(time$callback_msg,file = log_con,sep="\n")


# #------------Code 03: 03generate_DST.R
# tic("total")
# #get data split table (blockCV) and preparation for null model
# get_DST <- function(pa.id,model.group){
#   #create folders
#   DST.dir <- file.path(out_folder,species,'DataSplitTable',model.group)
#   if(!exists(DST.dir)) dir.create(DST.dir,recursive = TRUE)
#   # Null.train_pre.dir <- file.path(out_folder,species,'nullmodel/train_pre',model.group)
#   # if(!exists(Null.train_pre.dir)) dir.create(Null.train_pre.dir,recursive = TRUE)
#   # Null.rest.dir <- file.path(out_folder,species,'nullmodel/rest_data',model.group)
#   # if(!exists(Null.rest.dir)) dir.create(Null.rest.dir,recursive = TRUE)
#   #--------Step1: Load pre-pa data---------
#   pre_pa.dir <- file.path(out_folder,species,"pre_pa_csv",model.group)
#   #---------Step2: Load historical climate data -------
#   hist.dir <- "inputs/chelsa/usa/1981-2010/ensmean/hist/bio"
#   sel.evnames <- read.csv(file.path(out_folder,species,'sp_bios_names/hist_bio_names.csv'))
#   sel.evnames <- as.vector(sel.evnames$x)
#   fns<- paste0(sel.evnames,".tif")
#   climate.dir <- file.path(hist.dir,fns)
#   bios.hist <- raster::stack(climate.dir)
#   #---------Step3: Load pesudo-absence and presence data -------
#   pre_pa.dir <- file.path(out_folder,species,"pre_pa_csv",model.group)
#   file.list <- list.files(pre_pa.dir)
#   file <- paste0('pre_pa_',model.group,pa.id,'.csv')
#   pre_pa <- read.csv(file.path(pre_pa.dir,file))
#   pre.records <- nrow(pre_pa[which(pre_pa$Species==1), ])
#   #---------Step4: Splitting between training and test records (blockCV)-------
#   pa_data <- st_as_sf(pre_pa, coords = c("x", "y"), crs = crs(bios.hist))
#   # spatial blocking by rows with systematic assignment
#   if (species == 'Yucca madrensis' | species =='Yucca schottii'){
#     t = 5
#   }else{
#     t = 1
#   }
#   sb2 <- spatialBlock(speciesData = pa_data, # presence-background data
#                       species = "Species",
#                       rasterLayer = bios.hist,
#                       rows = 3*t,
#                       cols = 5*t,
#                       k = 5,
#                       selection = "systematic",
#                       biomod2Format = TRUE)

#   # spatial blocking by rows and columns with checkerboard assignment
#   sb3 <- spatialBlock(speciesData = pa_data,
#                       species = "Species",
#                       rasterLayer = bios.hist,
#                       rows = 10*t,
#                       cols = 15*t,
#                       selection = "checkerboard",
#                       biomod2Format = TRUE)

#   # environmental clustering
#   eb <- envBlock(rasterLayer = bios.hist,
#                  speciesData = pa_data,
#                  species = "Species",
#                  k = 5,
#                  standardization = "standard", # rescale variables between 0 and 1
#                  rasterBlock = FALSE,
#                  numLimit = 1)

#   # 2. Defining the folds for DataSplitTable
#   # note that biomodTable should be used here not folds
#   # use generated folds from spatialBlock in previous section
#   #DST.sb1 <- sb1$biomodTable
#   DST.sb2 <- sb2$biomodTable
#   DST.sb3 <- sb3$biomodTable
#   DST.eb <- eb$biomodTable
#   #select the runs which meets the following requirements
#   #cols.sb1 <- which(sb1$records[2] > 0.5*pre.records & sb1$records[4] > 0.1*pre.records)
#   cols.sb2 <- which(sb2$records[2] > 0.5*pre.records & sb2$records[4] > 0.1*pre.records)
#   cols.sb3 <- which(sb3$records[2] > 0.5*pre.records & sb3$records[4] > 0.1*pre.records)
#   cols.eb <- which(eb$records[2] > 0.5*pre.records & eb$records[4] > 0.1*pre.records)
#   #DST2.sb1 <- DST.sb1[,cols.sb1]
#   DST2.sb2 <- DST.sb2[,cols.sb2]
#   DST2.sb3 <- DST.sb3[,cols.sb3]
#   DST2.eb <- DST.eb[,cols.eb]
#   DST.all <- as.data.frame(cbind(DST2.sb2,DST2.sb3,DST2.eb))
#   col.DST <- ncol(DST.all)
#   if (col.DST != 0){
#     for (i in 1:col.DST){
#       colnames(DST.all)[i] <- paste0('RUN',i)
#       #save data split table
#       DST <- DST.all[,i]
#       DST <- cbind(pre_pa,DST)
#       filename <- paste0(model.group,'_pa',pa.id,'_DST',i,'.csv')
#       write.csv(DST,file.path(DST.dir,filename))
#       #   #training presence data (for null model)
#       #   run.pre_pa <- cbind(pre_pa,DST.all[,i])
#       #   train.pre <- filter(run.pre_pa,Species==1 & DST.all[,i] == 'TRUE')
#       #   filename <- paste0(model.group,'_pa',pa.id,'_DST',i,'.csv')
#       #   write.csv(train.pre,paste0(Null.train_pre.dir,'/',filename))
#       #   #test presence data and all pseduo-absence data (for null model)
#       #   test.pre <- filter(run.pre_pa,Species==1 & DST.all[,i] == 'FALSE')
#       #   pa <- filter(run.pre_pa,Species==0)
#       #   rest_data <- rbind(test.pre,pa)
#       #   filename <- paste0(model.group,'_pa',pa.id,'_DST',i,'.csv')
#       #   write.csv(rest_data,paste0(Null.rest.dir,'/',filename))
#     }
#   }
# }

# model.groups <- c('GroupA','GroupB','GroupC')
# for (model.group in model.groups){
#   for (pa.id in 1:20){
#     print(paste0(species, model.group,pa.id))
#     DST.all <- get_DST(pa.id,model.group)
#   }
# }

# cat(paste0("-----Step 3: Have generated DST data-------------"), file = log_con,sep="\n")
# time <- toc()
# cat(time$callback_msg,file = log_con,sep="\n")


# #------Code 04: 04realmodeloutputs_hist.R
# tic("total")
# speciesV2 <- gsub(" ", ".", species)
# if(!exists("BiomodOutput")) dir.create("BiomodOutput",recursive = TRUE)
# if(!exists(paste0("log_files/",speciestype))) dir.create(paste0("log_files/",speciestype),recursive = TRUE)
# log_con <- file(paste0("log_files/",speciestype,"/",stateID,"_",species,".txt"), open="a")
# 
# #---------Step1: Load climate data -------
# #-- historical climate data
# hist.dir <- "inputs/chelsa/usa/1981-2010/ensmean/hist/bio"
# sel.evnames <- read.csv(file.path(out_folder,species,'sp_bios_names/hist_bio_names.csv'))
# sel.evnames <- as.vector(sel.evnames$x)
# fns<- paste0(sel.evnames,".tif")
# climate.dir <- paste0(hist.dir,'/',fns)
# bios.hist <- raster::stack(climate.dir)
# 
# get_real.prepa.data <- function(species,model.group,fn.id){
#   if (run_loc=='cc'){
#     setwd(paste0("/home/",account,"/scratch/state_species_2024"))
#   }else{
#     setwd("/Volumes/UoG_xuezhen/state_species_2024")
#   }
#   prepa.dir<- file.path(out_folder,species,'DataSplitTable',model.group)
#   filelist <- list.files(prepa.dir)
#   filename <- filelist[fn.id]
#   prepa <- read.csv(file.path(prepa.dir,filename))
#   return(prepa)
# }
# 
# get_BiomodData <- function(pre.pa,bios.hist){
#   myResp <- as.numeric(pre.pa$Species)
#   myRespXY <- pre.pa[, c('x', 'y')]
#   myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
#                                        expl.var = bios.hist, # explanatory raster data
#                                        resp.xy = myRespXY,
#                                        resp.name = species,
#                                        na.rm = TRUE)
#   return(myBiomodData)
# }
# 
# # define individual models options ----
# Sp_opt <-
#   BIOMOD_ModelingOptions(
#   )
# 
# get_om.test <- function(species,pre.pa,pre.test,model.name,bios.hist,fn.id){
#   myBiomodData <- get_BiomodData(pre.pa,bios.hist)
#   DST <- as.matrix(pre.pa$DST)
#   if (model.name == 'MAXENT'){
#     model.name = 'MAXENT.Phillips.2'
#   }
#   if (run_loc=='cc'){
#     setwd(paste0("/home/",account,"/scratch/state_species_2024/BiomodOutput"))
#   }else{
#     setwd("/Volumes/UoG_xuezhen/state_species_2024/BiomodOutput")
#   }
# 
#   myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
#                                       modeling.id = paste0(model.name,fn.id),
#                                       models = model.name,
#                                       bm.options = Sp_opt,
#                                       data.split.table = DST,
#                                       metric.eval = c('POD'),
#                                       var.import = 0,
#                                       prevalence = 0.5,
#                                       CV.perc = 0.8,
#                                       do.full.models = FALSE,
#                                       scale.models = TRUE,
#                                       # save.output = FALSE,
#                                       nb.cpu = 1)
# 
#   myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
#                                     proj.name = paste0('hist',model.name,fn.id),
#                                     new.env = bios.hist,
#                                     models.chosen = 'all',
#                                     metric.binary = 'all',
#                                     metric.filter = 'all',
#                                     build.clamping.mask = FALSE)
#   pre.train <- filter(pre.pa, DST == 'TRUE' & Species == '1')
#   #get coordinates
#   pre.train.coord <- data.frame(pre.train$x,pre.train$y)
#   pre.test.coord <- data.frame(pre.test$x,pre.test$y)
#   #get projected value for each train and test data
#   proj.value <- terra::unwrap(myBiomodProj@proj.out@val)[[1]] #myBiomodProj@proj.out@val[[1]]
#   pre.train.proj.value <- raster::extract(proj.value,pre.train.coord)
#   pre.test.proj.value <- raster::extract(proj.value,pre.test.coord)
#   names(pre.train.proj.value) <- c('ID','value')
#   names(pre.test.proj.value) <- c('ID','value')
#   pre.train.proj.value <- pre.train.proj.value$value
#   pre.test.proj.value <- pre.test.proj.value$value
#   pre.train.om10 <- quantile(pre.train.proj.value,0.1)
#   pre.train.om5 <- quantile(pre.train.proj.value,0.05)
#   pre.train.om0 <- quantile(pre.train.proj.value,0)
#   t <- length(pre.test.proj.value)
#   om10.test = length(pre.test.proj.value[pre.test.proj.value < pre.train.om10])/t
#   om5.test = length(pre.test.proj.value[pre.test.proj.value < pre.train.om5])/t
#   om0.test = length(pre.test.proj.value[pre.test.proj.value < pre.train.om0])/t
#   POD <- get_evaluations(myBiomodModelOut)
#   om_POD <- 1 - POD$validation
#   Th_POD <- POD$cutoff
#   oms <- data.frame(fn.id,om_POD,om0.test,om5.test,om10.test,Th_POD, pre.train.om0, pre.train.om5, pre.train.om10)
#   return(oms)
# }
# 
# 
# get_real_oms <- function(fn.id){
#   fn_om <- filelist[fn.id]
#   om_outfile.dir <- file.path(real.all.dir,fn_om)
#   if (run_loc=='cc'){
#     setwd(paste0("/home/",account,"/scratch/state_species_2024"))
#   }else{
#     setwd("/Volumes/UoG_xuezhen/state_species_2024")
#   }
# 
#   biomodoutput.path <- paste0('BiomodOutput/',speciesV2,'/proj_hist',model.name,fn.id,'/proj_hist',model.name,fn.id,'_',speciesV2,'.tif')
#   biomodoutput.path <-  gsub("MAXENT", "MAXENT.Phillips.2", biomodoutput.path)
#   if (!file.exists(biomodoutput.path)) {
#     real.pre.pa <- get_real.prepa.data(species,model.group,fn.id)
#     real.pre.test <- filter(real.pre.pa, Species == '1' & DST == 'FALSE')
#     oms <- get_om.test(species,real.pre.pa,real.pre.test,model.name,bios.hist,fn.id)
#     names(oms) <- c('fnid','ompod','om0','om5','om10','thpod','th0','th5','th10')
#     if (run_loc=='cc'){
#       setwd(paste0("/home/",account,"/scratch/state_species_2024"))
#     }else{
#       setwd("/Volumes/UoG_xuezhen/state_species_2024")
#     }
#     write.csv(oms,om_outfile.dir)
#     cat(paste0("---",model.name,fn.id," Done!----"), file = log_con,sep="\n")
#   }else{
#     cat(paste0("---",model.name,fn.id," Already Done!----"), file = log_con,sep="\n")}
# }
# 
# modellist <- c('MAXENT','MARS','CTA','FDA','RF','GLM','GAM','GBM')
# for (model.name in modellist) {
#   if (model.name == 'MAXENT' | model.name == 'GLM' | model.name == 'GAM' | model.name == 'GBM') {
#     model.group = 'GroupA'
#   } else if (model.name == 'MARS') {
#     model.group = 'GroupB'  # Changed variable name to 'modelgroup' for consistency
#   } else if (model.name == 'CTA' || model.name == 'FDA' || model.name == 'RF') {
#     model.group = 'GroupC'
#   }
#   #
#   cat(paste0("---",stateID, "-", species,"-",model.name, "----"), file = log_con,sep="\n")
#   # do parallel
#   real.all.dir <- file.path(out_folder,species,'omission_rates',model.name)
#   if(!exists(real.all.dir)) dir.create(real.all.dir,recursive = TRUE)
#   prepa.dir<- file.path(out_folder,species,'DataSplitTable',model.group)
#   filelist <- list.files(prepa.dir)
#   len <- length(filelist)
#   reps = seq(1:len)
#   mclapply(reps, get_real_oms, mc.cores = n.cores)
#   cat(paste0("---", model.name, " model hist Done!----"), file = log_con, sep = "\n")
# }
# 
# cat(paste0("-----Step 4: Have fit all the models-------------"), file = log_con,sep="\n")
# time <- toc()
# cat(time$callback_msg,file = log_con,sep="\n")
# 
# 
# #----------Code 05: 05real_oms_summary_table.R
# tic("total")
# modelnames = c("GLM", "GBM", "GAM", "CTA","FDA", "MARS", "RF","MAXENT")
# 
# if(!exists(paste0(out_folder,",",species,"/summary_doc"))) dir.create(paste0(out_folder,"/",species,"/summary_doc"),recursive = TRUE)
# 
# om_alls <- {}
# for (model.name in modelnames){
#   print(model.name)
#   om.dir<- file.path(out_folder,species,"omission_rates",model.name)
#   fns <- list.files(om.dir)
#   len <- length(fns)
#   for (i in 1:len){
#     om_data <- read.csv(file.path(om.dir, fns[i]))
#     om_data$model <- model.name
#     om_alls <- rbind(om_alls,om_data)
#   }
#   ompods <- filter(om_alls, ompod == 0)
#   om0s <- filter(om_alls, om0 == 0)
#   om5s <- filter(om_alls, om5 <= 0.05)
#   om10s <- filter(om_alls,om10 <= 0.1)
# }
# 
# #write summary table
# out.dir<- file.path(out_folder,species,'summary_doc','omrates.csv')
# write.csv(om_alls,out.dir)
# 
# # check how many runs were selected from each model category
# model_no <- count(om_alls,'model')
# select_om10_no <- count(om10s, 'model')
# comparisons <- merge(model_no, select_om10_no, all = TRUE,by = "model")
# names(comparisons) <- c('modelname','freq_all','freq_om10')
# comparisons$ratio <- comparisons$freq_om10/comparisons$freq_all
# out.dir<- file.path(out_folder,species,'summary_doc','selected_runs_permodel.csv')
# write.csv(comparisons,out.dir)
# 
# cat(paste0("-----Step 5: sum omission rate tables Done!-------------"), file = log_con,sep="\n")
# time <- toc()
# cat(time$callback_msg,file = log_con,sep="\n")



#------------Code 06: 06modelprojections.R
tic("total")
speciesV2 <- gsub(" ", ".", species)

time.periods <- c('2071-2100')
scenarios <- c('ssp585')

#---------Step 1: get climate------
get_climate <- function(time.period,scenario){
  if (run_loc=='cc'){
    setwd(paste0("/home/",account,"/scratch/state_species_2024"))
  }else{
    setwd("/Volumes/UoG_xuezhen/state_species_2024")}
  data.dir <- file.path('inputs/chelsa/usa',time.period,'ensmean',scenario,'bio')
  sel.evnames <- read.csv(file.path(out_folder,species,'sp_bios_names/hist_bio_names.csv'))
  sel.evnames <- as.vector(sel.evnames$x)
  fns<- paste0(sel.evnames,".tif")
  climate.dir <- paste0(data.dir,'/',fns)
  bios <- raster::stack(climate.dir)
  names(bios) <- sel.evnames
  if (run_loc=='cc'){
    setwd(paste0("/home/",account,"/scratch/state_species_2024/BiomodOutput"))
  }else{
    setwd("/Volumes/UoG_xuezhen/state_species_2024/BiomodOutput")}
  return(bios)
}

#---------Step 2: do predictions------
get_projections <- function(i){
  if (run_loc=='cc'){
    setwd(paste0("/home/",account,"/scratch/state_species_2024/BiomodOutput"))
  }else{
    setwd("/Volumes/UoG_xuezhen/state_species_2024/BiomodOutput")}
  fn.id <- om_sel$fnid[i]
  model.name <- om_sel$model[i]
  biomodoutput.name <- paste0(speciesV2,'.',time.period,scenario,model.name,fn.id,'.projection.out')
  biomodoutput.dir <- paste0('proj_',time.period,scenario,model.name,fn.id)
  biomodoutput.path <- file.path(speciesV2,biomodoutput.dir,biomodoutput.name)
  if (file.exists(biomodoutput.path)==TRUE){
    cat(paste0(biomodoutput.name,' file exists!!'), file = log_con,sep="\n")
  }else{
    bios <- get_climate(time.period,scenario)
    if (model.name == 'MAXENT'){
      model.name = 'MAXENT.Phillips.2'
    }
    biomodoutput.name <- paste0(speciesV2,'.',model.name,fn.id,'.models.out')
    biomodoutput.path <- file.path(speciesV2,biomodoutput.name)
    load(biomodoutput.path)
    myBiomodModelOut <- get(load(biomodoutput.path))
    myBiomodModelOut@dir.name <- getwd()
    myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                      proj.name = paste0(time.period,scenario,model.name,fn.id),
                                      new.env = bios,
                                      build.clamping.mask = FALSE)
    cat(paste0(biomodoutput.name,' file just done!!'), file = log_con,sep="\n")
  }
}

om_alls <- read.csv(file.path(out_folder,species,'summary_doc','omrates.csv'))
# Check if the BiomodOutput files are all complete
hist_modelID <- paste0(om_alls$model, om_alls$fnid)
hist_modelID <- gsub("MAXENT", "MAXENT.Phillips.2", hist_modelID)
hist_filenames <- paste0('BiomodOutput/',speciesV2,'/proj_hist',hist_modelID,'/proj_hist',hist_modelID,'_',speciesV2,'.tif')

if(!exists(paste0("log_files/check_BiomodOutput_",speciestype))) dir.create(paste0("log_files/check_BiomodOutput_",speciestype),recursive = TRUE)
log_check <- file(paste0("log_files/check_BiomodOutput_",speciestype,"/",stateID,"_",species,".txt"), open="a")

cat('---1. Check if the hist BiomodOutput files are all completed---', file = log_check,sep="\n")
Needcheck='N'
for (fn in hist_filenames){
  if (!file.exists(fn)){
    cat(paste0('Missing file: ',fn), file = log_check,sep="\n")
    Needcheck='Y'
  }
}
print("hist BiomodOutput files are all completed, no need to check/rerun")
cat(paste0('---Done checking, Check?', Needcheck, '---'), file = log_check,sep="\n")

cat('---2. Check if the future BiomodOutput files are all completed---', file = log_check,sep="\n")
if (Needcheck == 'N'){
  om_sel <- filter(om_alls,om10 <= 0.1)
  if (nrow(om_sel) <= 10){
    om_sel <- filter(om_alls,om10 <= 0.15)
  }
  fut_modelID <- paste0(om_sel$model, om_sel$fnid)
  fut_modelID <- gsub("MAXENT", "MAXENT.Phillips.2", fut_modelID)
  len <- nrow(om_sel)
  reps = seq(1:len)

  for (time.period in time.periods){
    for (scenario in scenarios){
      cat(paste0("---",time.period,' ', scenario," Running!----"), file = log_con,sep="\n")
      mclapply(reps,get_projections, mc.cores = n.cores)
      fut_filenames <- paste0('BiomodOutput/',speciesV2,'/proj_',time.period,scenario,fut_modelID,'/proj_',time.period,scenario,fut_modelID,'_',speciesV2,'.tif')
      cat(paste0('---Check if the future(',time.period,') BiomodOutput files are all completed---'), file = log_check,sep="\n")
      Needcheck='N'
      # Check if the BiomodOutput files are all complete
      for (fn in fut_filenames){
        if (!file.exists(fn)){
          cat(paste0('Missing file: ',fn), file = log_check,sep="\n")
          Needcheck='Y'
        }
      }
      cat(paste0('---Done checking, Check?', Needcheck, '---'), file = log_check,sep="\n")
      if (Needcheck == 'Y'){
        cat(paste0("---",time.period,' ', scenario," Not completed!----"), file = log_con,sep="\n")
      }else{
        cat(paste0("---",time.period,' ', scenario," Done!----"), file = log_con,sep="\n")
      }
    }
  }
}else{
  cat(paste0("-----Step 6: hist BiomodOut is NOT completed-------------"), file = log_con,sep="\n")
}

time <- toc()
cat(time$callback_msg,file = log_con,sep="\n")

#------Code: 07: 07ensemble_model.R
tic("total")
speciesV2 <- gsub(" ", ".", species)

time.periods <- c('2071-2100')
scenarios <- c('ssp585')

om_idxs <- c(0,5,10)
om_idxs <- c(10)
# get model projections under different conditions
get_projections <- function(species,time.period,scenario,om_sel,i){
  if (run_loc=='cc'){
    setwd(paste0("/home/",account,"/scratch/state_species_2024/BiomodOutput"))
  }else{
    setwd("/Volumes/UoG_xuezhen/state_species_2024/BiomodOutput")}
  fn.id <- om_sel$fnid[i]
  model.name <- om_sel$model[i]
  if (model.name == 'MAXENT'){
    model.name = 'MAXENT.Phillips.2'
  }
  if (scenario == 'hist'){
    biomodoutput.name <- paste0(speciesV2,'.',scenario,model.name,fn.id,'.projection.out')
    biomodoutput.dir <- paste0('proj_',scenario,model.name,fn.id)
  }else{
    biomodoutput.name <- paste0(speciesV2,'.',time.period,scenario,model.name,fn.id,'.projection.out')
    biomodoutput.dir <- paste0('proj_',time.period,scenario,model.name,fn.id) 
  }
  biomodoutput.path <- file.path(speciesV2,biomodoutput.dir,biomodoutput.name)
  print(biomodoutput.path)
  load(biomodoutput.path)
  myBiomodModelProj <- get(load(biomodoutput.path))
  myBiomodModelProj <- rast(myBiomodModelProj@proj.out@val)
  if (run_loc=='cc'){
    setwd(paste0("/home/",account,"/scratch/state_species_2024"))
  }else{
    setwd("/Volumes/UoG_xuezhen/state_species_2024")}
  return(myBiomodModelProj)
}

get_ensplot <- function(species,time.period,scenario,om_idx){
  if (run_loc=='cc'){
    setwd(paste0("/home/",account,"/scratch/state_species_2024"))
  }else{
    setwd("/Volumes/UoG_xuezhen/state_species_2024")}
  #create outraster folder
  outraster.dir = paste0('outplots','_',ForI,'/',species,'/',time.period,'_',scenario)
  if(!exists(outraster.dir)) dir.create(outraster.dir,recursive = TRUE)
  fn = paste0('omth',om_idx,'.tif')
  outraster <- file.path(outraster.dir,fn)
  plot.dir <- paste0(outraster.dir,'/omth',om_idx,'.png')
  if (file.exists(plot.dir)==TRUE){
    cat(paste0(time.period,' ', scenario, ' ',om_idx,' Plot existed!'), file = log_con,sep="\n") 
  }else{
    #select the runs that meet the om requirement
    om_alls <- read.csv(file.path(out_folder,species,'summary_doc','omrates.csv'))
    if (om_idx == 10){
      om_sel <- filter(om_alls,om10 <= 0.1)
      if (nrow(om_sel)<=10){
        om_sel <- filter(om_alls,om10 <= 0.15)
      }
    }else if(om_idx == 5){
      om_sel <- filter(om_alls,om5 <= 0.05)
    }else if(om_idx == 0){
      om_sel <- filter(om_alls,om0 == 0)
    }else if(om_idx == 'pod'){
      om_sel <- filter(om_alls,ompod == 0)
    }else if(om_idx == 'all'){
      om_sel <- om_alls
    }
    #stack these runs
    len <- nrow(om_sel)
    myBiomodModelProjs <- get_projections(species,time.period,scenario,om_sel,1)
    if (len > 1){
      for (i in 2:len){
        print(i)
        myBiomodModelProj <- get_projections(species,time.period,scenario,om_sel,i)
        myBiomodModelProjs <- c(myBiomodModelProjs, myBiomodModelProj)
      }
    }
    #rescale the projections and get the mean
    ens <- mean(myBiomodModelProjs)/1000
    #save the ensemble projection
    # save raster
    writeRaster(ens, filename=outraster, overwrite=TRUE)
    # save plot
    cols <- brewer.pal(9, "OrRd")
    pal <- colorRampPalette(cols)
    png(file=plot.dir, width=600*5, height=300*5,res=300)
    plot(ens,zlim = c(0,1),col=pal(50),main=paste0(species,' ',time.period,' ',scenario,' omth',om_idx))
    dev.off()
    cat(paste0(time.period,' ', scenario, ' ',om_idx,'Plot saved!'), file = log_con,sep="\n") 
  }
}


cat(paste0('------------Species: ',species,' ensemble-------------'), file = log_con,sep="\n") 
# get_ensplot(species,'ensmean','hist','all')
# get_ensplot(species,'ensmean','hist','pod')
for (om_idx in om_idxs){
  print(paste0('Select the runs based on om',om_idx))
  get_ensplot(species,'ensmean','hist',om_idx)
  print('1981-2010 Plot saved!')
  for (time.period in time.periods){
    for (scenario in scenarios){
      get_ensplot(species,time.period,scenario,om_idx)
    }
  }
}

cat(paste0("-----Step 7: Have generated all the ensemble predictions-------------"), file = log_con,sep="\n")
time <- toc()
cat(time$callback_msg,file = log_con,sep="\n")
