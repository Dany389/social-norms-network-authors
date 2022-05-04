# Import data from social_norms.xls
rm(list=ls())

library(tidyverse)
library(wordcloud)
library(tm)
library(RWeka)

setwd("../")

# read data 
master <- readxl::read_xlsx(path = "Social Norms meta.xlsx", sheet = "ALL")

# word cloud analysis

keywords <- master %>% group_by(PaperID) %>% arrange(PaperID) %>%filter(row_number()==1)%>% ungroup() %>% select(Keywords) %>% sapply(str_split, pattern=";") %>% unlist()

docs <- VCorpus(VectorSource(keywords))
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
