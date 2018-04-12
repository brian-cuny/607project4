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
library(caret)
set.seed(1234)


Website.Parse <- function(folder, type){
  path <- paste0('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607project4\\', folder, '\\', type, '\\')
  list.files(path=path, pattern="\\d+") %>%
  map_dfr(~getURL(paste0('file:///', path, .x)) %>% 
            htmlParse() %>%
            xpathSApply('/html//body', xmlValue) %>%
            strsplit(split='\\n') %>%
            unlist() %>%
            as.tibble() %>%
            add_column(type=type, .before=1) %>%
            add_column(id=paste(type, .x, sep='_'), .before=1)
        )
}

custom.stopwords <- data_frame(word=c('div', 'p', 'h', 'img', 'function'))

data <- c('Bands', 'BioMedical', 'Goats', 'Sheep') %>%
  map_df(~Website.Parse('Train', .)) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, '(\\d|\\.|[^A-Za-z])+')) %>%
  filter(stringi::stri_enc_mark(word) == 'ASCII') %>%
  mutate(word = wordStem(word)) %>%
  anti_join(custom.stopwords)

# td_idf by topic ---------------------------------------------------------
data.tfidf <- data %>%
  count(type, word) %>%
  bind_tf_idf(word, type, n) %>%
  arrange(type, tf_idf) %>%
  mutate(order = row_number()) %>%
  group_by(type) %>%
  top_n(10, tf_idf)

ggplot(data.tfidf, aes(order, tf_idf, fill=type)) +
  geom_bar(show.legend=FALSE, stat='identity') +
  facet_wrap(~type, scales='free') +
  coord_flip() +
  theme(axis.text.x=element_text(angle=-30, vjust=1, hjust=0)) +
  scale_x_continuous(
    breaks = data.tfidf$order,
    labels = data.tfidf$word
  )
# td_idf by topic ---------------------------------------------------------

# sentiment analysis ------------------------------------------------------

afinn.data <- data %>%
  inner_join(get_sentiments('afinn')) %>%
  count(type, word, score) %>%
  mutate(total = n/sum(n))

afinn.frequent.data <- afinn.data %>%
  group_by(type) %>%
  top_n(5, n) %>%
  arrange(type, desc(n))

afinn.vline.data <- afinn.data %>%
  group_by(type) %>%
  summarise(avg = mean(n*score))

ggplot(afinn.data, aes(score, total, fill=factor(score))) +
  geom_bar(stat='identity') +
  geom_vline(data=afinn.vline.data, aes(xintercept=avg)) +
  facet_wrap(~type, ncol=1) +
  scale_x_continuous(limits=c(-5,5), breaks=seq(-5,5,1)) +
  scale_y_continuous(limits=c(0,.15), breaks=seq(0,.15,.05), expand=c(0,0), labels=percent) +
  scale_fill_brewer(palette='RdYlGn') + 
  labs(x='Sentiment Score',
       y='Frequency',
       title='Use of Strong Words by Topic') +
  theme_bw() + 
  theme(legend.position='none',
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        strip.background=element_rect(fill='grey70'))

# sentiment analysis ------------------------------------------------------


# bigrams -----------------------------------------------------------------

bigram.data <- c('BioMedical') %>%
  map_df(~Website.Parse('Train', .)) %>%
  unnest_tokens(word, value, token='ngrams', n=2) %>%
  separate(word, c('word1', 'word2'), sep=' ') %>%
  anti_join(stop_words, by=c('word1'='word')) %>%
  anti_join(stop_words, by=c('word2'='word')) %>%
  anti_join(custom.stopwords, by=c('word1'='word')) %>%
  anti_join(custom.stopwords, by=c('word2'='word')) %>%
  filter(!str_detect(word1, '(\\d|\\.|[^A-Za-z])+')) %>%
  filter(!str_detect(word2, '(\\d|\\.|[^A-Za-z])+')) %>%
  filter(stringi::stri_enc_mark(word1) == 'ASCII') %>%
  filter(stringi::stri_enc_mark(word2) == 'ASCII')

afinn.biomedical.frequent.data <- afinn.frequent.data %>%
  filter(type == 'BioMedical')

bigram.data %>% 
  count(word1, word2, sort=TRUE) %>%
  filter(word1 %in% afinn.biomedical.frequent.data$word | word2 %in% afinn.biomedical.frequent.data$word,
         n >= 2) %>%
  mutate(word1 = wordStem(word1)) %>%
  mutate(word2 = wordStem(word2)) %>%
  graph_from_data_frame() %>%
  ggraph(layout='fr') +
  geom_edge_link(aes(edge_alpha=n), show.legend=FALSE, arrow=arrow(type='closed', length=unit(.15, 'inches'))) + 
  geom_node_point(color='lightblue', size=5) +
  geom_node_text(aes(label=name), vjust=1, hjust=1) +
  theme_void() +
  labs(title='Frequently Used BioMedical Word Pairings')


# bigrams -----------------------------------------------------------------





# random forest -----------------------------------------------------------

test.numbers <- sample(1:323, 32, replace=FALSE)

ml.data <- data %>%
  count(type, id, word) %>%
  cast_dtm(id, word, n) %>%
  removeSparseTerms(sparse=0.99) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(type = str_extract(row.names(.), '[^_]+'))

train.data <- ml.data %>%
  filter(!(row_number() %in% test.numbers))

test.data <- ml.data %>%
  filter(row_number() %in% test.numbers)

train.results <- train(type~., data=train.data,
           method='rf',
           trControl=trainControl(method='cv',
                                  number = 2
                                  ),
           verbose=FALSE)

test.results <- predict(train.results, newdata=test.data)

present.results <- data_frame(original=test.data$type, prediction=test.results)

table(present.results$original, present.results$prediction)



