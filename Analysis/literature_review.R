# Import data from social_norms.xls
rm(list=ls())

library(tidyverse)
library(wordcloud)
library(tm)
library(RWeka)

setwd("../")

# read data 
master <- readxl::read_xlsx(path = "Social Norms meta.xlsx", sheet = "ALL")

authors_attributes <- readxl::read_xlsx("authors.xlsx") %>% distinct(id, .keep_all = T) %>% strsplit(Id_paper_affil, split = ":")

master_methods <- readxl::read_excel("Social Norms meta.xlsx", sheet = "ALL") %>%
  subset.data.frame(subset = Method_elicitation =="KW" | 
                     Method_elicitation =="Bicchieri" | 
                    Method_elicitation =="Both") %>%
  distinct(PaperID, .keep_all = T) %>% subset.data.frame(select = c("PaperID", "Authors", "Year", "Outlet", "Published", "Game_type", "Method_elicitation"))

# text analysis -------------
ords <- master %>% group_by(PaperID) %>% arrange(PaperID) %>%filter(row_number()==1)%>% ungroup() %>% select(Keywords) %>% sapply(str_split, pattern=";") %>% unlist()

docs <- VCorpus(VectorSource(ords))
#toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
#docs <- tm_map(docs, removeNumbers)
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c('"')) 
# Remove punctuations
#docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
#docs <- tm_map(docs, stripWhitespace)
#newBigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
dtm <- TermDocumentMatrix(docs, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))
title("Alternative")


## Trend of methods ------------
table=master_methods %>% group_by(Year, Method_elicitation)%>%count()%>%ungroup()%>%group_by(Method_elicitation)%>%mutate(cum_sep_len = cumsum(n)) %>% mutate(cum_sep_len=replace(cum_sep_len, Method_elicitation=="KW"&Year<2013, NA))
ggplot(data=table, aes(x=Year, cum_sep_len, color=Method_elicitation))+geom_line() + theme_classic()

## 
do.call(rbind, l_method) %>% group_by(id) %>% 
  summarise(n_KW=sum(method=="KW")+sum(method=="Both"),n_BX=sum(method=="Bicchieri")+sum(method=="Both")) %>% 
  mutate(d=(n_KW-n_BX)/(n_KW+n_BX))