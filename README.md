# sd3

## Objectif

Ce package implémente plusieurs fonctions ayant pour but de faciliter la mise en forme de données sous la forme attendue par `networkD3` ou `sankeyD3`.

## Installation

L'installation du package peut se faire simplément en exécutant la commande suivante depuis une console R :

```r
# install.packages("remotes")
remotes::install_github("thoera/sd3")
```

## Usage

```r
library("sd3")
library("sankeyD3")

str(data_1)

# mise en forme des données
paths <- from_aggregated_paths(data = data_1, path = "parcours",
                               count = "comptage", sep = " - ")

# construction des nodes et links
nodes <- create_nodes(data = paths)
links <- create_links(data = paths, nodes = nodes)

# mise en forme des noms pour des questions esthétiques
links$source <- gsub("_[0-9]+", "", links$source)
links$target <- gsub("_[0-9]+", "", links$target)
nodes$node_id <- gsub("_[0-9]+", "", nodes$node)

# choix des couleurs
colors <- choose_colors(nodes = sort(unique(nodes$node_id)),
                        colors = c("#50BE87", "#4BB4E6"))

data_sankey <- list(nodes = nodes, links = links)

sankeyD3::sankeyNetwork(
  Links = data_sankey$links,
  Nodes = data_sankey$nodes,
  Source = "source_index",
  Target = "target_index",
  Value = "count",
  NodeID = "node_id",
  numberFormat = "",
  LinkGroup = "target",
  fontSize = 14,
  nodeWidth = 26,
  dragX = TRUE,
  dragY = TRUE,
  showNodeValues = FALSE,
  nodeCornerRadius = 5,
  linkOpacity = 0.3,
  nodeStrokeWidth = 0,
  zoom = FALSE,
  colourScale = sankeyD3::JS(colors)
)
```
