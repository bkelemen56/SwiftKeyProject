# ---------------------------------------------------------------------
# model #2.0 - learn from trigrams and bigrams
#
# 1) extract all bigrams and trigrams from the training text
# 2.1) from each trigram, extract the bigram and the following word
# 2.2) from each bigram, extract the unigram and the following word
# 3.1) calculate the frequency of each word following the same bigram
# 3.2) calculate the frequency of each word folliwing the same unigram
# 4.1) prediction: read two words, find the most frequent word afterwards
#    from bigrams. 
# 4.2) if none found search the second word read in the unigrams and find
#    the most frequent word afterwards.
# 5) if none found, can't predict unseen trigrams
# ---------------------------------------------------------------------

# set to FALSE to use the process whole dataset
use_small_data <- TRUE      # TRUE will use a small sub-set of the data (for development)
train_pct = .01           # percentage of each file to use as training

(library(tidyverse))
library(stringr)

library(quanteda)
library(readtext)

# ---------------------------------------------------------------------
# Load documents
# ---------------------------------------------------------------------
cat('loading documents\n')

path = '../raw-data/final/en_US'
file_types <- c('blogs', 'news', 'twitter')
files <- c('en_US.blogs.small.txt', 'en_US.news.small.txt', 'en_US.twitter.small.txt')
names(files) <- file_types

# read the text files into a data frame (source, text)

# only read small for now
raw_text <- readtext('data/*small*', docvarsfrom  = 'filenames', dvsep = '\\.', verbosity = 0)
text_corpus <- corpus(raw_text)
# docvar2 will be the source

# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------
cat('\ncomputing bigrams and trigrams\n')

# clean the data
clean_text <- text_corpus
# TODO: add here additional cleaning stuff...
# TODO: remove 'RT' that are re-twittes

# process bigrams
dfm_bigram <- dfm(clean_text, ngrams = 2, removePunct = TRUE)

# compute trigrams
dfm_trigram <- dfm(clean_text, ngrams = 3, removePunct = TRUE)

# split trigram into first 2 words and following word
ss <- str_split(featnames(dfm_trigram), '_', n = 3)
bigram <- sapply(ss, function(x) paste(x[[1]], x[[2]]))
word <- sapply(ss, function(x) x[[3]])

df_model1 <- tibble(bigram = bigram, word = word, n = colSums(dfm_trigram)) %>%
  group_by(bigram) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

# split bigram into first word and following word
ss <- str_split(featnames(dfm_bigram), '_', n = 2)
unigram <- sapply(ss, function(x) x[[1]])
word <- sapply(ss, function(x) x[[2]])

df_model2 <- tibble(unigram = unigram, word = word, n = colSums(dfm_bigram)) %>%
  group_by(unigram) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

cat('size of trigram model = ',object.size(df_model1), '\n')
cat('size of bigram model = ',object.size(df_model2), '\n')

cat('\ntraining accuracy:\n')

# predictions on all trigrams in training
df_predic_train1 <- df_model1 %>%
  group_by(bigram) %>%
  arrange(desc(freq)) %>%
  mutate(n = NULL, freq = NULL) %>%
  slice(1)

# predictions on all bigrams in training
df_predic_train2 <- df_model2 %>%
  group_by(unigram) %>%
  arrange(desc(freq)) %>%
  mutate(n = NULL, freq = NULL) %>%
  slice(1)

df_accuracy <- df_model1 %>%
  left_join(df_predic_train1, by = 'bigram') %>%
  mutate(tp = (word.x == word.y) * n, word = word.x,
         unigram = str_split(bigram, ' ', n = 2)[[1]][[2]]) %>%
  select(bigram, unigram, word, n, freq, tp)

df_accuracy1 <- df_accuracy %>% filter(tp != 0)

df_accuracy2 <- df_accuracy %>% 
  filter(tp == 0) %>%
  left_join(df_predic_train2, by = 'unigram') %>%
  mutate(tp = (word.x == word.y) * n, word = word.x) %>%
  select(bigram, unigram, word, n, freq, tp)

df_accuracy <- rbind(df_accuracy1, df_accuracy2)

cat('training accuracy = ', sum(df_accuracy$tp) / sum(df_accuracy$n))

cat('\nprediction:\n')

predict_word <- function(s) {
  df <- subset(df_model1, bigram == s)
  if (nrow(df) > 0) {
    word <- (df %>% arrange(desc(freq)))[1:1, ]$word
  } else {
    s <- str_split(s, ' ', n = 2)[[1]][[2]]  # use the second word read
    df <- subset(df_model2, unigram == s)
    if (nrow(df) > 0) {
      word <- (df %>% arrange(desc(freq)))[1:1, ]$word
    } else {
      word <- NULL
    }
  }
  word
}

print_predict_word <- function(word) {
  if (!is.null(word)) {
    cat('predicted word: ', word)
  } else {
    cat("can't predict - no match")
  }
}

repeat {
  s <- readline(prompt = "enter two words: ")
  if (s == '') {
    break
  } else {
    word <- predict_word(s)
    print_predict_word(word)
  }
}

  
cat('\nend program')
