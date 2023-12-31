#phylogenetic tree
# Load necessary library
library(ape)
library(ggtree)

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

df <- read.csv('inputs/flowers_tree.csv')
# Assuming df is your dataframe and it has columns 'Species', 'Genus', 'Family'
# Ensure that these columns are character strings
df$Species <- as.factor(df$Species)
df$Genus <- as.factor(df$Genus)
df$Family <- as.factor(df$Family)
df$Order <- as.factor(df$Order)
df$Clade <- as.factor(df$Clade)

# Create the phylogenetic tree using the correct formula structure
tree <- as.phylo(~ Clade/Order/Family/Genus/Species, data = df)

# Plot the tree
plot(tree)

# Using ggtree for a more customizable circular tree
g <- ggtree(tree, layout = "circular") +
  geom_tiplab() +
  theme_tree2()
plot(g)
