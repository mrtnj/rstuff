
## Playing with breeding structure graphs (Simianer & al 2020)

library(ggraph)
library(jsonlite)
library(tidygraph)


json <- fromJSON("~/Downloads/Basic Breeding Program (Simianer et al. 2020).json")

edges <- json$Edges
colnames(edges)[3] <- "type"

graph <- as_tbl_graph(edges[, 1:3],
                      directed = TRUE)


plot_basic <- ggraph(graph, layout = "tree", root = "males") +
    geom_node_label(aes(label = name),
                    label.padding = unit(4, "mm")) +
    geom_edge_arc(aes(colour = type),
                  arrow = arrow(length = unit(4, "mm")),
                  start_cap = circle(16, "mm"),
                  end_cap = circle(16, "mm"))
