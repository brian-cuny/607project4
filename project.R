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



