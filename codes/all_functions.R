##FUNCTIONS
#get and clean occurrence data from GBIF website
get_us_records <- function(species,year1,year2){
  occs <- read.csv(paste0(species,'/data_cleaning/raw_data_us.csv'))
  no.raw_records <- nrow(occs)
  #--------Step 2: Checking species' coordinates-----------
  geo.clean <- clean_coordinates(x = occs, 
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 species = "species", 
                                 countries = "countryCode",
                                 value = "clean",
                                 tests = c("capitals","centroids","equal","gbif","institutions","seas","zeros"))
  
  # export the global data after coordinate check
  occs.out<- geo.clean %>% dplyr::select(scientificName,species, decimalLongitude,decimalLatitude, country,year) 
  # remove spatial duplicates
  occs.out<- occs.out[!duplicated(occs.out),]
  no.cleaned_global_records <- nrow((occs.out))
  
  # export the global data after coordinate check
  write.csv(occs.out, 
            paste0(species,'/data_cleaning/data_cleaned_coord_US.csv'), 
            row.names = FALSE)
  
  # export the data in certain time period after coordinate check 
  occs.out2 <- filter(occs.out, year <= year2 & year >= year1)
  write.csv(occs.out2,
            paste0(species,'/data_cleaning/data_cleaned_coord_US_years.csv'),
            row.names = FALSE)
  no.cleaned_US_records <- nrow((occs.out))
  no.cleaned_US_records_period <- nrow((occs.out2))
  no.records <- c(species, no.raw_records, no.cleaned_global_records,no.cleaned_US_records,no.cleaned_US_records_period)
  return(no.records)
}

get_global_records <- function(species,year1,year2){
  occs <- read.csv(file.path('inputs',species,'data_cleaning/raw_data_global.csv'))
  no.raw_records <- nrow(occs)
  #--------Step 2: Checking species' coordinates-----------
  geo.clean <- clean_coordinates(x = occs, 
                                 lon = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 species = "species", 
                                 countries = "countryCode",
                                 value = "clean",
                                 tests = c("capitals","centroids","equal","gbif","institutions","seas","zeros"))
  
  # export the global data after coordinate check
  occs.out<- geo.clean %>% dplyr::select(scientificName,species, decimalLongitude,decimalLatitude, country,year) 
  # remove spatial duplicates
  occs.out<- occs.out[!duplicated(occs.out),]
  no.cleaned_global_records <- nrow((occs.out))
  
  # export the global data after coordinate check
  write.csv(occs.out, 
            file.path('inputs',species,'data_cleaning/data_cleaned_coord_global.csv'), 
            row.names = FALSE)
  no.records <- c(species, no.raw_records, no.cleaned_global_records)
  return(no.records)
}

#thinning occurrence data to reduce autocorrelation
get_thinned_data <- function(species){
  occs <- read.csv(file.path('inputs',species,'data_cleaning/data_cleaned_coord_global.csv'))
  colnames(occs) <- c("scientificname","spec","lon",'lat','country')
  ## Run spatial thinning on the Heteromys data set, using a nnd of 10 km
  dir.create(file.path('inputs',species,'data_thinning'))
  thinnned_dataset_full <-
    thin(loc.data = occs,
         lat.col = "lat", long.col = "lon",
         spec.col = "spec",
         thin.par = 10, reps = 1, 
         locs.thinned.list.return = TRUE,
         write.files = FALSE,
         out.dir = file.path('inputs',species,'data_thinning'),
         write.log.file = TRUE)
  no.cleaned_records = nrow(occs)
  no.thin_records = nrow(thinnned_dataset_full[[1]])
  no.records <- c(no.cleaned_records, no.thin_records)
  # exported the thinned data set
  thinned_data <- thinnned_dataset_full[[1]]
  names(thinned_data) <- c('lon','lat')
  write.csv(thinned_data,
            file.path('inputs',species,'data_thinning/data_thinned.csv'),
            row.names = FALSE)
  return(no.records)
}

# Use state code (stateID) to get state name
get_statename <- function(stateID){
  state_codes <- c(AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California", CO = "Colorado", CT = "Connecticut", DE = "Delaware", FL = "Florida", GA = "Georgia", HI = "Hawaii", ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa", KS = "Kansas", KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",  MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi", MO = "Missouri",  MT = "Montana", NE = "Nebraska", NV = "Nevada", NH = "New Hampshire", NJ = "New Jersey", NM = "New Mexico", NY = "New York", NC = "North Carolina", ND = "North Dakota", OH = "Ohio", OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina", SD = "South Dakota", TN = "Tennessee", TX = "Texas",  UT = "Utah", VT = "Vermont", VA = "Virginia", WA = "Washington", WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming")
  state_name <- state_codes[stateID]
  return(state_name)
}

# select historical environment variables based on VIF (multi-collinearity)
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

get_bio_names <- function(species,bios.hist){
  var_names <- names(bios.hist)
  bios <- c('bio1','bio2','bio3','bio4','bio5','bio6','bio7','bio8','bio9','bio10',
            'bio11','bio12','bio13','bio14','bio15','bio16','bio17','bio18','bio19')
  bio_idx <- replicate(19, 0)
  for (i in 1:19){
    if (is.element(bios[i],var_names)){bio_idx[i] <- 1}else(bio_idx[i] <- 0)
  }
  bio_idx <- c(species,bio_idx)
  return(bio_idx)
}

# select environment variables based on selected historical variable names
sel_ev <- function(sel.evnames){
  data.dir <- paste0("~/Desktop/state_species/chelsa_climate_data/", timeperiod, "_usa/", model,"/",ssp)
  fns<- paste0(sel.evnames,".tif")
  climate.dir <- paste0(data.dir,'/',fns)
  bios <- raster::stack(climate.dir)
  return(bios)
}

# function to get parameters for BIOMOD_FormatingData
get_PA_paras <- function(model.group,no.records){
  #Group A: 'MaxEnt','BIOCLIM'/'SRE','GLM','GAM','GBM',
  if (model.group == 'GroupA'){
    PAstrategy = 'random'
    PArep = 20
    if (no.records > 10000){
      PAno = no.records
    }else{PAno = 10000}
    }
  #Group B: 'MARS'
  else if(model.group == 'GroupB'){
    PAstrategy = 'random'
    PArep = 20
    PAno = no.records}
  #Group C: 'CTA','FDA', 'ANN', 'RF'
  else if(model.group == 'GroupC'){
    if(no.records<=200){
      PAstrategy = 'disk'
    }else{
      PAstrategy = 'sre'}
    PArep = 20
    PAno = no.records}
  paras = c(PAstrategy,PArep,PAno)
  return(paras)
}

# Generate pseudo-absence based on the PA parameters
get_PA_data <- function(model.group,no.records,myResp,myRespXY,bios.hist,sp){
  PA.paras <- get_PA_paras(model.group,no.records)
  if (PA.paras[1] == 'disk'){
    PA.data <- BIOMOD_FormatingData(
      resp.var = myResp,# presence data (all 1's)
      resp.xy = myRespXY,# coordinates of presences
      expl.var = bios.hist,# RasterStack
      resp.name = sp,# name of species
      PA.strategy = PA.paras[1], 
      PA.dist.min = 220000,
      PA.dist.max = NULL,
      PA.nb.rep = as.numeric(PA.paras[2]), 
      PA.nb.absences = as.numeric(PA.paras[3])
    )} else {
      PA.data <- BIOMOD_FormatingData(
        resp.var = myResp,# presence data (all 1's)
        resp.xy = myRespXY,# coordinates of presences
        expl.var = bios.hist,# RasterStack
        resp.name = sp,# name of species
        PA.strategy = PA.paras[1], 
        PA.nb.rep = as.numeric(PA.paras[2]), 
        PA.nb.absences = as.numeric(PA.paras[3])
      )}
  return(PA.data)
}

## function to get PA dataset
get_PAtab <- function(bfd){
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species,
    bfd@PA.table
  )
}

# generate pseudo absence data for different models (which have 20 replicates)
get_pre_pa <- function(model.group,no.records,reps,myResp,myRespXY,bios.hist,species){
  PA.paras <- get_PA_paras(model.group,no.records)
  PA.data <- get_PA_data(model.group,no.records,myResp,myRespXY,bios.hist,species)
  csv.dir <- file.path(out_folder,species,'pre_pa_csv')
  if(!exists(csv.dir)) dir.create(csv.dir)
  output.dir <- file.path(csv.dir,model.group)
  if(!exists(output.dir)) dir.create(output.dir)
  
  # generate presence data
  pres.data <- myRespXY
  names(pres.data) = c('x', 'y') 
  pres.data$status <- 1
  # generate pseudo absence data (1 or 20 replicates) 
  for (i in 1:reps){
    pa.data.table <- get_PAtab(PA.data) %>% 
      filter(is.na(status))
    pa.data <- filter(pa.data.table,pa.data.table[i+3] == TRUE)
    pa.data <- pa.data[,c("x", "y","status")]
    pa.data$status <- 0
    # generate presence - pseudo absence data
    PA <- rbind(pres.data,pa.data)
    names(PA) <- c('x','y','Species')
    # save presence - pseudo absence data
    file.dir <- paste0(output.dir,"/pre_pa_",model.group,i,".csv")
    write.csv(PA,file.dir, row.names = TRUE)
  }
}

# plot occurrence map in Unite States
plot_occ_us <- function(species){
  occs <- read.csv(paste0('outputs/',species,'/data_thinning/data_thinned.csv'))
  occ_transformed <- usmap_transform(occs,
                                     input_names = c("lon","lat"),
                                     output_names = c("x","y"))
  occ_transformed <- filter(occ_transformed,lon < -66 & lon >-170 & lat > -15 & lat < 72)
  p<- plot_usmap() +
    geom_point(data = occ_transformed, aes(x = x, y = y),
               color = "red", alpha = 0.25) +
    theme(legend.position = "right") + 
    theme(panel.background = element_rect(colour = "black")) + 
    labs(title = paste0("GBIF occurrence data in Unite States"),
         subtitle = paste0("Species: ",species)) +
    theme(legend.position = "right")
  return(p)
}