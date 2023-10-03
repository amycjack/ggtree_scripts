# Load necessary libraries
library(ggtree)    # For tree visualization
library(ape)       # For phylogenetic data
library(tidytree)  # For working with tree data
library(treeio)    # For reading tree files
library(ggplot2)   # For plotting
library(tidyverse) # For data manipulation
library(scales)    # For scale adjustments
library(data.table) # For working with data tables
library(glue)      # For text formatting

# Read the phylogenetic tree data from a file
tree <- read.tree("descurainia_tree.tree")

# Create a basic tree plot with tip labels
ggtree(tree) + geom_tiplab()

# Round tree internal labels (support values) to 2 decimal places
q <- ggtree(tree)
bootstraps <- q$data
bootstraps <- bootstraps[!bootstraps$isTip,]  # Filter out tip labels
bootstraps$label <- as.numeric(bootstraps$label)
bootstraps$label <- signif(bootstraps$label, digits = 2)
bootstraps <- bootstraps[bootstraps$label > 60,]  # Filter by a threshold

# Read annotation data from a CSV file
data <- read.csv("tree_anno.csv", header = TRUE)

# Add formatted columns to the annotation data
data <- dplyr::mutate(data, 
                      lab = glue("italic({species})~italic({genus})"),
                      color = c("#E495A5"),
                      name = glue("<i style='color:{color}'>{genus} **{species}**</i>")
)

# Group annotation data by clade and calculate the most recent common ancestor (MRCA) node
clades.df <- data %>%
  group_by(clade) %>%
  summarise(node = MRCA(tree, data$label[data$clade == clade])) %>%
  distinct() %>%
  ungroup()

# Define alternating gray colors
gray_colors <- c("white", "gray")
clades.df$color_group <- factor(clades.df$node %% 2, levels = 0:1)

# Plot the phylogenetic tree with annotations
ggtree(tree, ladderize = TRUE, size = 0.5) %<+% data +
  geom_tiplab(aes(label = lab), parse = T, hjust = -0.05, align = TRUE, linesize = 0.5) +
  geom_tippoint(aes(colour = Origin, shape = Origin), size = 4, show.legend = TRUE) +
  geom_text2(data = bootstraps, aes(label = label), hjust = -0.3, size = 2.5) +
  geom_treescale(x = 0.02, y = 0.01, offset = 0.1, offset.label = -0.2, label = "substitution rate") +
  geom_highlight(data = clades.df, 
                 aes(node = node, fill = color_group, group = clade),
                 alpha = 0.1,
                 align = "right",
                 extend = 0.005,
                 show.legend = FALSE) +
  geom_cladelab(data = clades.df,
                mapping = aes(node = node, label = clade),
                fontsize = 2,
                align = TRUE,
                offset = 0.005,
                offset.text = 0.00005,
                show.legend = FALSE) +
  scale_fill_manual(values = gray_colors)  # Set the fill colors for alternating clades
