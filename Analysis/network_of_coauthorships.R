# script for network analysis

library(igraph)
library(tidyverse)
library(visNetwork)

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

authors_solo <- authors_net[-grep("--", authors_net$New_authors), ]

# Create nodes and edges ----
# data wrangling from https://stackoverflow.com/questions/57487704/how-to-split-a-string-of-author-names-by-comma-into-a-data-frame-and-generate-an
SplitAuthors <- sapply(authors_n$New_authors, strsplit, split = "--", fixed = TRUE) # list of characters, each list is a paper
AuthorCombinations <- sapply(SplitAuthors,function(x){combn(unlist(x),m = 2)}) # compute all poss. combinats. among co-authors
AuthorEdges <- rapply(AuthorCombinations,unlist) # transform matrix into list
names(AuthorEdges) <- NULL
AuthorEdges <- trimws(AuthorEdges)
AuthorGraph <- graph(AuthorEdges, directed = FALSE) # igraph - we input the edge list (taken two by two)



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

plot_net <- visNetwork(Nodes, Edges) %>% 
  #visIgraphLayout(layout = "layout_with_fr") %>% 
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T) 
