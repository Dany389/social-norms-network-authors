#script author network

library(igraph)
library(tidyverse)
library(visNetwork)
library(dplyr)

rm(list=ls())

setwd("../")

# Import data ----
## filter for method elicitation (only KW - BX - Both in the lab)
authors_net <- readxl::read_excel("Social Norms meta.xlsx", sheet = "ALL") %>%
  #subset.data.frame(subset = Method_elicitation =="KW" | 
  #                   Method_elicitation =="Bicchieri" | 
  #                  Method_elicitation =="Both") %>%
  distinct(PaperID, .keep_all = T) %>% subset.data.frame(select = c("PaperID", "Authors", "Year", "Outlet", "Published", "Game_type", "Method_elicitation")) %>%
  mutate(New_authors = str_replace_all(Authors, ";", "--"))

authors_n <- authors_net[grep("--", authors_net$New_authors), ] # take only co-authored papers
authors_n$numberoauthors <- (1+str_count(authors_n$New_authors, "--")) # compute number of coauthors

authors_solo <- authors_net[-grep( distinct(PaperID, .keep_all = T)) %>% subset.data.frame(select = c("--", authors_net$New_authors)),]

# Create nodes and edges ----
# data wrangling from https://stackoverflow.com/questions/57487704/how-to-split-a-string-of-author-names-by-comma-into-a-data-frame-and-generate-an
SplitAuthors <- sapply(authors_n$New_authors, strsplit, split = "--", fixed = TRUE) # list of characters, each list is a paper
AuthorCombinations <- sapply(SplitAuthors,function(x){combn(unlist(x),m = 2)}) # compute all poss. combinats. among co-authors
AuthorEdges <- rapply(AuthorCombinations,unlist) # transform matrix into list
names(AuthorEdges) <- NULL
AuthorEdges <- trimws(AuthorEdges)
AuthorGraph <- graph(AuthorEdges, directed = FALSE) # igraph - we input the edge list (taken two by two)

### PROBLEM HERE =readxl::read_xlsx("authors.xlsx") %>% distinct(id, .keep_all = T)
nodes <- read.csv("nodes.csv") %>% merge.data.frame(suca)
edges <- read.csv("edges.csv")
AuthorGraph = graph_from_data_frame(edges, vertices = nodes, directed = F)

####################################################
# method KW
l_method <- list()
for(i in 1:length(authors_n$Method_elicitation)){
  l_method[[i]] <- data.frame(id=SplitAuthors[[i]], method=rep(authors_n$Method_elicitation[i],authors_n$numberoauthors[i]))
}
l_method_sum <- do.call(rbind, l_method) %>% group_by(id) %>% summarise(n_KW=sum(method=="KW")+sum(method=="Both"),n_BX=sum(method=="Bicchieri")+sum(method=="Both")) %>% mutate(d=(n_KW-n_BX)/(n_KW+n_BX))

Nodes <- data.frame(id=as_ids(V(AuthorGraph)), label=as_ids(V(AuthorGraph))) %>%
  merge.data.frame(l_method_sum) %>% 
  mutate(color = ifelse(d < -.5, "blue",ifelse(d < -.01, "grey", ifelse(d < .5, "green", "red")))) %>% 
  mutate(title = paste0("<p><b>", id,"</b><br>","Bicchieri = ", n_BX, "<br> Krupka = ", n_KW, "</p>"))


Edges <- data.frame(matrix(AuthorEdges, ncol=2, byrow = T))
colnames(Edges) <- c("from", "to")



plot_net <- visNetwork(nodes, edges) %>% 
  #visIgraphLayout(layout = "layout_with_fr") %>% 
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T) 

visNetwork(nodes, edges) %>% visIgraphLayout() %>% visNodes(size= 10)

visIgraph(AuthorGraph, physics = FALSE, smooth = TRUE)

plot(AuthorGraph, vertex.label.color="black", vertex.size=2.5,
     vertex.label.cex=0.5, vertex.label.dist=2, vertex.color="blue" ) 

#To differentiate between method elicitation, doesn't work
#vertex.color=c( "red", "blue")[1+(V(AuthorGraph)$Method_elicitation=="KW")] ) 

#for a cumulative graph, not really useful
#plot(x=0:max(degree(AuthorGraph, mode="all")),
    # y=1-degree_distribution(AuthorGraph, cumulative=T, mode="all"),
    # pch=19, cex=1.2, col="blue", xlab = "Degree", ylab="Cumulative Frequency")

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





