library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud)


# initial number of words per source
tidy_unigram %>% group_by(source) %>% count()

td <- tidy_unigram

# remove numbers
td <- filter(td, !str_detect(word, "^[0-9]*$"))

td_with_stop_words <- td

# remove stop words
# perhaps not good for the prediction algo
data(stop_words)
td_with_stop_words <- anti_join(td, stop_words, by = 'word')

# final number of words per source
td %>% group_by(source) %>% count()
td_with_stop_words %>% group_by(source) %>% count()

# blogs and twitter had higher % of words removed

# view most used words
td %>% group_by(source) %>% count(word, sort = TRUE) 

# plot most seen words

# plot#1 - most used words in all docs without stop-words
td %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  top_n(25) %>%
  ggplot(aes(word, n)) +
    geom_bar(stat = "identity") +
    labs(title = 'Most used words in all documents (without stop-words)') +
    xlab(NULL) +
    coord_flip()
    
# plot#1b - most used words in all docs with stop-words
td_with_stop_words %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  top_n(25) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  labs(title = 'Most used words in all documents (with stop-words)') +
  xlab(NULL) +
  coord_flip()

# plot#1c - wordcloud
set.seed(256)
td %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# plot #2 - most used words by documents without stop-words
td %>%
  count(source, word, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  #mutate_if(is.factor, as.character) %>%
  ggplot(aes(word, n)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~source, scales = 'free_y') +
    labs(title = 'Most used words by document type (without stop-words)', x = NULL) +
    coord_flip()

# plot #3 - most used words by documents with stop-words
td_with_stop_words %>%
  count(source, word, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  #mutate_if(is.factor, as.character) %>%
  ggplot(aes(word, n)) +
    geom_bar(stat = "identity") +
    facet_wrap(~source, scales = 'free_y') +
    labs(title = 'Most used words by document type (including stop-words)', x = NULL) +
    coord_flip()

# plot#4a - most used 2-grams in all docs
tidy_bigram %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  top_n(25) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  labs(title = 'Most used 2-grams in all documents') +
  xlab(NULL) +
  coord_flip()

# plot#4b - wordcloud
set.seed(256)
tidy_bigram %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# plot#5a - most used 3-grams in all docs
tidy_trigram %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  top_n(25) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  labs(title = 'Most used 3-grams in all documents') +
  xlab(NULL) +
  coord_flip()

# plot#5b - wordcloud
set.seed(256)
tidy_trigram %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
