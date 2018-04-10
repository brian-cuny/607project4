library(tidyverse)
library(modelr)
library(tidytext)
library(zoo)
library(scales)
library(ggraph)
library(igraph)
library(topicmodels)
library(httr)
library(RCurl)
library(XML)
library(magrittr)

Website.Parse <- function(type){
  list.files(path=paste0('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607project4\\SW\\', type, '\\'), pattern="\\d+") %>%
  map_dfr(~getURL(paste0('file:///C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607project4\\SW\\', type, '\\', .)) %>% 
            htmlParse() %>%
            xpathSApply('/html//body', xmlValue) %>%
            strsplit(split='\\n') %>%
            unlist() %>%
            as.tibble() %>%
            add_column(type=type, .before=1)
        )
}


data <- c('Bands', 'BioMedical', 'Goats', 'Sheep') %>%
  map_df(~Website.Parse(.)) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words)

tfidf <- data %>%
  count(type, word, sort=TRUE) %>%
  bind_tf_idf(word, type, n)

tfidf %<>%
  arrange(type, tf_idf) %>%
  mutate(order = row_number()) %>%
  group_by(type) %>%
  top_n(10, tf_idf)

ggplot(tfidf, aes(order, tf_idf, fill=type)) +
  geom_bar(show.legend=FALSE, stat='identity') +
  facet_wrap(~type, scales='free') +
  coord_flip() +
  theme(axis.text.x=element_text(angle=-30, vjust=1, hjust=0)) +
  scale_x_continuous(
    breaks = tfidf$order,
    labels = tfidf$word
  )





