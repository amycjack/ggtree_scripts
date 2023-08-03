library(ggtree)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(glue)

# Read the tree and round internal labels (support values) to 2 decimal places
tree <- read.beast("FigTree.tre")
tree$data <- tree$data %>% 
  filter(!isTip) %>%
  mutate(label = signif(as.numeric(label), digits = 2)) %>%
  filter(label > 0.2)

# Read annotation data
data <- read.csv("tree_anno.csv", header = TRUE)

# Create a modified data table with formatted labels
data2 <- data %>%
  mutate(
    lab = glue("italic({genus})~italic({species})~~~bold({tribe})"),
    color = "#E495A5",
    name = glue("<i style='color:{color}'>{genus} **{species}**</i> {tribe}")
  )

# Plot the tree with modified labels and annotations
tree2 <- ggtree(tree, ladderize = TRUE, size = 0.5) +
  geom_tiplab(aes(label = lab), parse = TRUE, hjust = -0.05, align = TRUE) +
  geom_text2(data = tree$data, aes(label = label), hjust = -.2, size = 3) +
  theme_tree2(legend.position = 'right') +
  geom_point2(data = tree$data, aes(subset = (node == 3)), shape = 21, size = 5, fill = 'darkgreen') +
  geom_range("reltime_0.95_CI", color = 'red', size = 2, alpha = .5) +
  labs(x = "AGE") +
  theme_tree2() +
  scale_x_continuous(expand = c(0, 0.1)) +
  xlab("Time")

# Save the plot to a PDF file
ggsave(paste0("tree2", format(Sys.time(), "%d%m%Y"), ".pdf"),
       dpi = 320,
       width = 9,
       height = 4.1)
