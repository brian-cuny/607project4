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
library(tm)
library(SnowballC)

Website.Parse <- function(type){
  list.files(path=paste0('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607project4\\SW\\', type, '\\'), pattern="\\d+") %>%
  map_dfr(~getURL(paste0('file:///C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607project4\\SW\\', type, '\\', .x)) %>% 
            htmlParse() %>%
            xpathSApply('/html//body', xmlValue) %>%
            strsplit(split='\\n') %>%
            unlist() %>%
            as.tibble() %>%
            add_column(type=paste(type, .x, sep='_'), .before=1)
        )
}


data <- c('Bands', 'BioMedical', 'Goats', 'Sheep') %>%
  map_df(~Website.Parse(.)) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words)


# td_idf by topic ---------------------------------------------------------
custom.stopwords <- data_frame(word=c('div', 'p', 'h'))

tfidf <- data %>%
  separate(type, c('type', 'document'), sep='_') %>%
  filter(!str_detect(word, '(\\d|\\.)+')) %>%
  mutate(word = wordStem(word)) %>%
  anti_join(custom.stopwords) %>%
  count(type, word) %>%
  bind_tf_idf(word, type, n) %>%
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
# td_idf by topic ---------------------------------------------------------







word_counts <- data %>%
  count(type, word, sort=TRUE) %>%
  ungroup()

chapters_dtm <- word_counts %>%
  cast_dtm(type, word, n)

chapters_lda <- LDA(chapters_dtm, k=4, control=list(seed=1234))

topics <- tidy(chapters_lda, matrix='gamma')

topics %>%
  separate(document, c('type', 'document'), sep='_') %>%
  mutate(type = reorder(type, gamma*topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~type)















