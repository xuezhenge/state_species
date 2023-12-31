#setwd("/home/hhager/scratch/state_species")
#species_list <- list.files('outputs')
#for (species in species_list){
#  nullmodel.dir <- file.path('outputs',species,'nullmodel')
#  unlink(nullmodel.dir,recursive = TRUE)
#}

library(argparse)

# create parser object
parser <- ArgumentParser()
# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("--speciesid", default='1', type="integer",
                    help="input the species id, e.g., 1")

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
species.id <- args$speciesid

setwd("/Users/xuezhenge/Desktop/state_species")
#setwd("/Volumes/XG_data/state_species")
species_list <- as.data.frame(list.files('outputs'))
write.csv(species_list,'all_species_list.csv')


rm.dir <- file.path('outputs/BiomodOutputs',speciesV2,'.zip')
unlink(rm.dir,recursive = TRUE)
