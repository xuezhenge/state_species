library(ape)
library(ggtree)

setwd("/Volumes/UoG_xuezhen/state_species_Dec2023")
df <- read.csv("inputs/flowers_tree.csv")

df$Species <- as.factor(df$Species)
df$Genus <- as.factor(df$Genus)
df$Family <- as.factor(df$Family)

tree <- as.phylo(~ Family/Genus/Species, data = df)

# Plot the tree
plot(tree, type = 'fan')

# Create a ggtree object
g <- ggtree(tree, layout = "circular")

# Add tip labels with specified size
g <- g + geom_tiplab(size = 3)  # Adjusts the size of tip labels

# Suppose we have identified two groups with the following node numbers
group1_nodes <- c(5, 9)  # Replace with actual node IDs for group 1
group2_nodes <- c(15, 16,17,18)  # Replace with actual node IDs for group 2

# Add highlighted background for group 1
g <- g + geom_highlight(node = group1_nodes, fill = "blue", alpha = 0.5)

# Add highlighted background for group 2
g <- g + geom_highlight(node = group2_nodes, fill = "green", alpha = 0.5)

# Add other customizations and plot the tree
g <- g + theme_tree2()  # Clean theme
plot(g)

