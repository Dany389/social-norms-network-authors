#script author network

library(igraph)
library(tidyverse)
library(visNetwork)
library(dplyr)

rm(list=ls())

setwd("../")

# import network data
node_attributes <- readxl::read_xlsx("authors.xlsx") %>% distinct(id, .keep_all = T)
nodes <- read.csv("nodes.csv") %>% merge.data.frame(node_attributes)
edges <- read.csv("edges.csv")
AuthorGraph = graph_from_data_frame(edges, vertices = nodes, directed = F)

# Create edge weight
E(AuthorGraph)$weight <- 1 # any nmbr
AuthorGraph <- igraph::simplify(AuthorGraph, remove.multiple = T, remove.loops = F, edge.attr.comb = list(weight="sum"))

# plot the graph
plot_data=toVisNetworkData(AuthorGraph)

#visIgraph(AuthorGraph, physics = FALSE, smooth = TRUE)
#plot(AuthorGraph, vertex.label.color="black", vertex.size=2.5,
#     vertex.label.cex=0.5, vertex.label.dist=2, vertex.color="blue" ) 
#To differentiate between method elicitation, doesn't work
#vertex.color=c( "red", "blue")[1+(V(AuthorGraph)$Method_elicitation=="KW")] ) 
#plot_net <- visNetwork(nodes, edges) %>% 
  #visIgraphLayout(layout = "layout_with_fr") %>% 
#  visOptions(highlightNearest = list(enabled = T, hover = T), 
#             nodesIdSelection = T) 

visNetwork(plot_data$nodes, plot_data$edges) %>% visIgraphLayout() %>% visNodes(size= 10)

#for a cumulative graph, not really useful
#plot(x=0:max(degree(AuthorGraph, mode="all")),
    # y=1-degree_distribution(AuthorGraph, cumulative=T, mode="all"),
    # pch=19, cex=1.2, col="blue", xlab = "Degree", ylab="Cumulative Frequency")




# Network analyses

#histo degree

hist(degree(AuthorGraph), breaks=0:50, main="Histogram")

#analyse descriptive
degree(AuthorGraph, mode="all")
plot(y=degree_distribution(AuthorGraph, cumulative=T, mode="all"), x=0:26)
centr_degree(AuthorGraph, mode="all", normalized = T)

closeness(AuthorGraph, mode="all", weights=NA)
centr_clo(AuthorGraph, mode="all", normalized = T)

eigen_centrality(AuthorGraph)
centr_eigen(AuthorGraph, directed=F, normalized=T)

betweenness(AuthorGraph, directed=F, weights=NA)
edge_betweenness(AuthorGraph, directed=F, weights=NA)
centr_betw(AuthorGraph, directed=F, normalized=T)

diameter(AuthorGraph, directed=F, weights=NA)
get_diameter(AuthorGraph)

transitivity(AuthorGraph, type="local")
transitivity(AuthorGraph, type="global")

edge_density(AuthorGraph)

assortativity_degree(AuthorGraph, directed=F)

#Pour avoir la clique
cliques(AuthorGraph)
sapply(cliques(AuthorGraph), length)
largest.cliques(AuthorGraph)
vcol <- rep("blue", vcount(AuthorGraph))
vcol[unlist(largest.cliques(AuthorGraph))] <- "gold"
plot(AuthorGraph, vertex.label=V(AuthorGraph)$name, vertex.color=vcol, vertex.size=3.5, vertex.label.cex=0.5)

#Community
cluster_edge_betweenness(AuthorGraph)
dendPlot(cluster_edge_betweenness(AuthorGraph))

plot(cluster_edge_betweenness(AuthorGraph), AuthorGraph, 
     vertex.label.color="black", vertex.size=2.5, vertex.label.cex=0.5,
     vertex.label.dist=2, vertex.color="blue" )
# OU ?
plot(cluster_label_prop(AuthorGraph), AuthorGraph)
plot(cluster_label_prop(AuthorGraph), AuthorGraph, vertex.label.color="black", 
     vertex.size=2.5, vertex.label.cex=0.5, vertex.label.dist=2, vertex.color="blue" )

#Louvain Comunity Detection
cluster <- cluster_louvain(AuthorGraph)
cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$id <- rownames(cluster_df)

nodes <- merge(x = nodes, y = cluster_df, by = "id", all.x = TRUE)
colnames(nodes)[3] <- "group"
visNetwork(nodes, edges)

## importation data auteurs
authors_data <- readxl::read_excel("authors.xlsx", sheet = "Sheet1") %>%
  distinct(ID, .keep_all = T) %>% subset.data.frame(select = c("ID", "last_name", "first_name", "affil_now", "loc_now", "citation_nbr", "h_index", "research_interests", "gender", "eco_psycho" ))





