#script author network

library(igraph)
library(tidyverse)
library(visNetwork)
library(dplyr)

rm(list=ls())

setwd("../")

# import network data -----
node_attributes <- readxl::read_xlsx("authors.xlsx") %>% distinct(id, .keep_all = T)
nodes <- read.csv("nodes.csv") %>% merge.data.frame(node_attributes, all.x = T) %>% 
  mutate(domain = ifelse(eco_psycho %in% c("eco","environmental law & eco","law & eco"),"Eco", ifelse(eco_psycho %in% c("science","Science (ethics, biology)","computer science", "biology; maths","health","engineer","Maths/physics","medecine","neuroscience"), "Other", eco_psycho))) %>%
  mutate(location = ifelse(loc_now %in% c("France","Germany","Switzerland","UK", "Italy","Czech Republic","Denmark","Austria", "Republic of Moldova","Austria; Denmark", "Belgium","Finland", "Ireland","Italy;UK","Norway","Poland","Slovakia","Sweden"),"Europe", 
                           ifelse(loc_now %in% c("US", "Canada"), "North America", 
                                  ifelse(loc_now %in% c("Australia", "Tanzania"), "Pacific",
                                         ifelse(loc_now %in% c("China","India","Indonesia","Pakistan","Philippines","Russia","South Korea","UAE","Vietnam"),"Asia", ifelse(loc_now %in% c("Argentina", "Colombia"), "South America", "Other"))))))

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

net_domain <- visNetwork(plot_data$nodes%>% 
             mutate(value=h_index*80, 
                    group=domain) %>% mutate(value=replace(value, is.na(value), 1)), 
           plot_data$edges%>%mutate(width=plot_data$edges$weight*2)) %>% 
  visGroups(groupname = "Eco", color = "red") %>%
  visGroups(groupname = "psycho", color = "gray") %>%
  visGroups(groupname = "Philosophy", color = "green") %>%
  visGroups(groupname = "political science", color = "yellow") %>%
  visGroups(groupname = "Other", color = "blue") %>%
  visGroups(groupname = "Sociology", color = "pink") %>%
  visIgraphLayout() %>%visLegend(width = 0.1, position = "right", main = "Group") %>% visOptions(highlightNearest = list(enabled = T, hover = T), 
                                                                                                 nodesIdSelection = T) 

  
htmlwidgets::saveWidget(file="net_domain.html", widget = net_domain)

net_geo <- visNetwork(plot_data$nodes%>% 
             mutate(value=h_index*80, 
                    group=location) %>% mutate(value=replace(value, is.na(value), 1)), 
           plot_data$edges%>%mutate(width=plot_data$edges$weight*2)) %>% 
  visGroups(groupname = "Europe", color = "red") %>%
  visGroups(groupname = "US", color = "gray") %>%
  visIgraphLayout() %>%visLegend(width = 0.1, position = "right", main = "Group")

  #for a cumulative graph, not really useful
#plot(x=0:max(degree(AuthorGraph, mode="all")),
    # y=1-degree_distribution(AuthorGraph, cumulative=T, mode="all"),
    # pch=19, cex=1.2, col="blue", xlab = "Degree", ylab="Cumulative Frequency")




# Network analyses ---------------

#histo degree

hist(degree(AuthorGraph), breaks=0:30, main="Histogram")

#analyse descriptive
centr_degree(AuthorGraph, mode="all", normalized = T)
closeness(AuthorGraph, mode="all", weights=NA)
centr_clo(AuthorGraph, mode="all", normalized = T)

eigen_centrality(AuthorGraph)
eigen=centr_eigen(AuthorGraph, directed=F, normalized=T)

#betweenness(AuthorGraph, directed=F, weights=NA)
#edge_betweenness(AuthorGraph, directed=F, weights=NA)
#centr_betw(AuthorGraph, directed=F, normalized=T)

#diameter(AuthorGraph, directed=F, weights=NA)
#get_diameter(AuthorGraph)

#transitivity(AuthorGraph, type="local")
transitivity(AuthorGraph, type="global")

edge_density(AuthorGraph)

# are popular nodes (with many degrees) connected with each other?
assortativity_degree(AuthorGraph, directed=F) # yes but not too much

# are authors of the same h_index connected among themselves?
assortativity(AuthorGraph, V(AuthorGraph)$h_index%>%replace_na(0), directed=F) # yes but not too much

#Pour avoir la clique
cliques(AuthorGraph)
sapply(cliques(AuthorGraph), length)
largest.cliques(AuthorGraph)
vcol <- rep("blue", vcount(AuthorGraph))
vcol[unlist(largest.cliques(AuthorGraph))] <- "gold"
plot(AuthorGraph, vertex.label=V(AuthorGraph)$name, vertex.color=vcol, vertex.size=3.5, vertex.label.cex=0.5)

#Community
x=cluster_edge_betweenness(AuthorGraph)
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







# Network evolution ------------

## 2013 ----
rm(list = ls())
node_attributes <- readxl::read_xlsx("authors.xlsx") %>% distinct(id, .keep_all = T)
nodes <- read.csv("nodes_2013.csv") %>% merge.data.frame(node_attributes, all.x = T)%>% 
  mutate(domain = ifelse(eco_psycho %in% c("eco","environmental law & eco","law & eco"),"Eco", ifelse(eco_psycho %in% c("science","Science (ethics, biology)","computer science", "biology; maths","health","engineer","Maths/physics","medecine","neuroscience"), "Other", eco_psycho)))

edges <- read.csv("edges_2013.csv")
AuthorGraph = graph_from_data_frame(edges, vertices = nodes, directed = F)

# Create edge weight
E(AuthorGraph)$weight <- 1 # any nmbr
AuthorGraph <- igraph::simplify(AuthorGraph, remove.multiple = T, remove.loops = F, edge.attr.comb = list(weight="sum"))

# plot the graph
plot_data=toVisNetworkData(AuthorGraph)

plot_2013 <- visNetwork(plot_data$nodes%>% filter(!is.na(domain)) %>%
             mutate(value=h_index*60, 
                    group=domain) %>% mutate(value=replace(value, is.na(value), 1)), 
           plot_data$edges%>%mutate(width=plot_data$edges$weight*2)) %>% visIgraphLayout() %>%
  visGroups(groupname = "Eco", color = "red") %>%
  visGroups(groupname = "psycho", color = "gray") %>%
  visGroups(groupname = "Philosophy", color = "green") %>%
  visGroups(groupname = "political science", color = "yellow") %>%
  visGroups(groupname = "Other", color = "blue") %>%
  visGroups(groupname = "Sociology", color = "pink") %>%
  visIgraphLayout() %>%visLegend(width = 0.1, position = "right", main = "Group") %>% visOptions(highlightNearest = list(enabled = T, hover = T), 
                                                                                                 nodesIdSelection = T) 

htmlwidgets::saveWidget(file="net_2013.html", widget = plot_2013)

## 2015 ----

node_attributes <- readxl::read_xlsx("authors.xlsx") %>% distinct(id, .keep_all = T)
nodes <- read.csv("nodes_2015.csv") %>% merge.data.frame(node_attributes, all.x = T)%>%  
  mutate(domain = ifelse(eco_psycho %in% c("eco","environmental law & eco","law & eco"),"Eco", ifelse(eco_psycho %in% c("science","Science (ethics, biology)","computer science", "biology; maths","health","engineer","Maths/physics","medecine","neuroscience"), "Other", eco_psycho)))

edges <- read.csv("edges_2015.csv")
AuthorGraph = graph_from_data_frame(edges, vertices = nodes, directed = F)

# Create edge weight
E(AuthorGraph)$weight <- 1 # any nmbr
AuthorGraph <- igraph::simplify(AuthorGraph, remove.multiple = T, remove.loops = F, edge.attr.comb = list(weight="sum"))

# plot the graph
plot_data=toVisNetworkData(AuthorGraph)

plot_2015 <- visNetwork(plot_data$nodes%>% filter(!is.na(domain)) %>%
                          mutate(value=h_index*60, 
                                 group=domain) %>% mutate(value=replace(value, is.na(value), 1)), 
                        plot_data$edges%>%mutate(width=plot_data$edges$weight*2)) %>% visIgraphLayout() %>%
  visGroups(groupname = "Eco", color = "red") %>%
  visGroups(groupname = "psycho", color = "gray") %>%
  visGroups(groupname = "Philosophy", color = "green") %>%
  visGroups(groupname = "political science", color = "yellow") %>%
  visGroups(groupname = "Other", color = "blue") %>%
  visGroups(groupname = "Sociology", color = "pink") %>%
  visIgraphLayout() %>%visLegend(width = 0.1, position = "right", main = "Group")%>% visOptions(highlightNearest = list(enabled = T, hover = T), 
                                                                                                nodesIdSelection = T) 


htmlwidgets::saveWidget(file="net_2015.html", widget = plot_2015)

## 2018 ----

node_attributes <- readxl::read_xlsx("authors.xlsx") %>% distinct(id, .keep_all = T)
nodes <- read.csv("nodes_2018.csv") %>% merge.data.frame(node_attributes, all.x = T)%>%  
  mutate(domain = ifelse(eco_psycho %in% c("eco","environmental law & eco","law & eco"),"Eco", ifelse(eco_psycho %in% c("science","Science (ethics, biology)","computer science", "biology; maths","health","engineer","Maths/physics","medecine","neuroscience"), "Other", eco_psycho)))

edges <- read.csv("edges_2018.csv")
AuthorGraph = graph_from_data_frame(edges, vertices = nodes, directed = F)

# Create edge weight
E(AuthorGraph)$weight <- 1 # any nmbr
AuthorGraph <- igraph::simplify(AuthorGraph, remove.multiple = T, remove.loops = F, edge.attr.comb = list(weight="sum"))

# plot the graph
plot_data=toVisNetworkData(AuthorGraph)

plot_2018 <- visNetwork(plot_data$nodes%>% filter(!is.na(domain)) %>%
                          mutate(value=h_index*60, 
                                 group=domain) %>% mutate(value=replace(value, is.na(value), 1)), 
                        plot_data$edges%>%mutate(width=plot_data$edges$weight*2)) %>% visIgraphLayout()%>%
  visGroups(groupname = "Eco", color = "red") %>%
  visGroups(groupname = "psycho", color = "gray") %>%
  visGroups(groupname = "Philosophy", color = "green") %>%
  visGroups(groupname = "political science", color = "yellow") %>%
  visGroups(groupname = "Other", color = "blue") %>%
  visGroups(groupname = "Sociology", color = "pink") %>%
  visIgraphLayout() %>%visLegend(width = 0.1, position = "right", main = "Group") %>% visOptions(highlightNearest = list(enabled = T, hover = T), 
                                                                                                 nodesIdSelection = T) 


htmlwidgets::saveWidget(file="net_2018.html", widget = plot_2018)

## 2020 ----
#rm(list = ls())
node_attributes <- readxl::read_xlsx("authors.xlsx") %>% distinct(id, .keep_all = T)
nodes <- read.csv("nodes_2020.csv") %>% merge.data.frame(node_attributes, all.x = T)%>%  
  mutate(domain = ifelse(eco_psycho %in% c("eco","environmental law & eco","law & eco"),"Eco", ifelse(eco_psycho %in% c("science","Science (ethics, biology)","computer science", "biology; maths","health","engineer","Maths/physics","medecine","neuroscience"), "Other", eco_psycho)))

edges <- read.csv("edges_2020.csv")
AuthorGraph = graph_from_data_frame(edges, vertices = nodes, directed = F)

# Create edge weight
E(AuthorGraph)$weight <- 1 # any nmbr
AuthorGraph <- igraph::simplify(AuthorGraph, remove.multiple = T, remove.loops = F, edge.attr.comb = list(weight="sum"))

# plot the graph
plot_data=toVisNetworkData(AuthorGraph)

plot_2020 <- visNetwork(plot_data$nodes%>% filter(!is.na(domain)) %>%
                          mutate(value=h_index*60, 
                                 group=domain) %>% mutate(value=replace(value, is.na(value), 1)), 
                        plot_data$edges%>%mutate(width=plot_data$edges$weight*2)) %>% visIgraphLayout() %>%
  visGroups(groupname = "Eco", color = "red") %>%
  visGroups(groupname = "psycho", color = "gray") %>%
  visGroups(groupname = "Philosophy", color = "green") %>%
  visGroups(groupname = "political science", color = "yellow") %>%
  visGroups(groupname = "Other", color = "blue") %>%
  visGroups(groupname = "Sociology", color = "pink") %>%
  visIgraphLayout() %>%visLegend(width = 0.1, position = "right", main = "Group")%>% visOptions(highlightNearest = list(enabled = T, hover = T), 
                                                                                                nodesIdSelection = T) 


htmlwidgets::saveWidget(file="net_2020.html", widget = plot_2020)