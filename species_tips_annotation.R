library(ggtree)
library(ape)
library(tidytree)
library(treeio)
library(ggplot2)
library(tidyverse)
library(scales)
library(dplyr)
library(glue)

# script to generate tree with clean sample names and internal nodes

#read in tree and tip label data
tree <- read.beast("FigTree.tre")

# round tree internal labels (i.e., in my case, support values) to 2 dps
q <- ggtree(tree)
d <- q$data
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)
d$label <- signif(d$label, digits = 2)
d <- d[d$label > 0.2,]

# test tree to gain node # info 
ggtree(tree) + geom_text(aes(label=node), hjust=-.3,)

#read in annotation data containing sample name, genus, species, and tribe names
data <- read.csv("tree_anno.csv", header = TRUE)

# add column to annotation data to italicise, bold and glue certain columns
data2 <- dplyr::mutate(data, 
                    lab = glue("italic({genus})~italic({species})~~~bold({tribe})"),
                    color = c("#E495A5"),
                    name = glue("<i style='color:{color}'>{genus} **{species}**</i> {tribe}")
) 

#plot tree
tree2 <- ggtree(tree, ladderize=TRUE, size=0.5) %<+% data2 +
  geom_tiplab(aes(label=lab), parse=T, hjust=-0.05, align=TRUE) + #replace tips with our modified data table
  geom_text2(data=d, aes(label=label), hjust=-.2, size=3) + #add modified labels to internal nodews
  theme_tree2(legend.position='right') +
  geom_point2(aes(subset=(node==3)), shape=21, size=5, fill='darkgreen') + #highligh tip label with circle  
  geom_range("reltime_0.95_CI", color='red', size=2, alpha=.5)  +
  labs(x="AGE") + theme_tree2() + scale_x_continuous(expand=c(0, 0.1)) + xlab("Time") 

revts(tree2)

#save
ggsave(paste0("tree2", format(Sys.time(), "%d%m%Y"), ".pdf"),
       dpi = 320,
       width = 9,
       height =  4.1)

