# ---------------------------------------------------------------------
# model #2 - learn from trigrams and bigrams
#
# the interactive process reads a sentince and predicts each word.
# a list of words predicted are displayed.
#
# 1) extract all bigrams and trigrams from the training text
# 2.1) from each trigram, extract the bigram and the following word
# 2.2) from each bigram, extract the unigram and the following word
# 3.1) calculate the frequency of each word following the same bigram
# 3.2) calculate the frequency of each word folliwing the same unigram
# 4.1) prediccazrtion: read two words, find the most frequent word afterwards
#    from bigrams. 
# 4.2) if none found search the second word read in the unigrams and find
#    the most frequent word afterwards.
# 5) if none found, can't predict unseen trigrams
# ---------------------------------------------------------------------

library(tidyverse)
library(stringr)

library(quanteda)
library(readtext)

# controls training documents to use
use_small_data <- TRUE     # TRUE will use a small sub-set of the data (for development)
use_pct_data = .01         # percentage of each file to use as training

do_accuracy_calc <- FALSE  # this takes some time, so may not want always 

# ---------------------------------------------------------------------
# Load documents
# ---------------------------------------------------------------------

cat('loading documents [small=', use_small_data, ', pct=', use_pct_data, ']\n')

path = 'data'
# file_types <- c('blogs', 'news', 'twitter')
# files <- c('en_US.blogs.small.txt', 'en_US.news.small.txt', 'en_US.twitter.small.txt')
# names(files) <- file_types

# only read small for now
pattern <- ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_'))
raw_text <- readtext(paste0(path, '/*train\\.', pattern, '*'), docvarsfrom  = 'filenames', dvsep = '\\.', verbosity = 0)
text_corpus <- corpus(raw_text)
# docvar2 will be the source

# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------
cat('\ncomputing bigrams and trigrams\n')

# clean the data
clean_corpus <- text_corpus
# TODO: add here additional cleaning stuff...
# TODO: remove 'RT' that are re-twittes

# process bigrams
cat('  bigrams\n')
dfm_bigram <- dfm(clean_corpus, ngrams = 2, removePunct = TRUE)

# compute trigrams
cat('  trigrams\n')
dfm_trigram <- dfm(clean_corpus, ngrams = 3, removePunct = TRUE)

cat('  trigram model\n')
# split trigram into first 2 words and following word
ss <- str_split(featnames(dfm_trigram), '_', n = 3)
bigram <- sapply(ss, function(x) paste(x[[1]], x[[2]]))
word <- sapply(ss, function(x) x[[3]])

df_model1 <- tibble(bigram = bigram, word = word, n = colSums(dfm_trigram)) %>%
  group_by(bigram) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

cat('  bigram model\n')
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

if (do_accuracy_calc) {
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
}

clean_text <- function(s) {
  tok <- quanteda::tokenize(s, what = "word", removePunct = TRUE, removeNumbers = TRUE, 
                            removeSeparators = TRUE, removeHyphens = TRUE, removeTwitter = TRUE,
                            removeURL = TRUE)
  paste(tok[[1]], collapse = ' ')
}

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

predict_top_words <- function(s, n_words = 5) {
  
  pretty_fmt <- function(df) {
    str <- summarise(df, word = paste(paste0(word, '(', freq, '%)'), collapse = ', '))
    str <- str_replace(str, ', $', '')
    str
  }
  
  # start with empty set of words
  words <- data_frame(word=character(), freq=double())
  
  # check trigrams
  df <- subset(df_model1, bigram == s)
  if (nrow(df) > 0) {
    n_words_to_get <- min(n_words - length(words), nrow(df))
    df <- df %>% 
      arrange(desc(freq)) %>% 
      slice(1:n_words_to_get) %>% 
      transmute(word = word, freq = round(freq*100, 2))
    words <- rbind(words, df)
  }
  
  # check bigrams if more words are needed
  if (length(words) < n_words) {
    s <- str_split(s, ' ', n = 2)[[1]][[2]]  # use the second word read
    df <- subset(df_model2, unigram == s)
    if (nrow(df) > 0) {
      n_words_to_get <- min(n_words - length(words), nrow(df))
      df <- df %>% 
        arrange(desc(freq)) %>% 
        slice(1:n_words_to_get) %>% 
        transmute(word = word, freq = round(freq*100, 1))
      words <- rbind(words, df)
    }
  }
  
  pretty_fmt(words)
}

# accepts a complete sentence
predict_sentence <- function(s) {
  s <- char_tolower(s)
  tok <- quanteda::tokenize(s)[[1]]
  
  s <- paste(tok[1], tok[2])
  result <- s
  m <- 3
  while (m <= (length(tok)+1)) {
    word <- predict_top_words(s, 5)

    if (m > length(tok)) {
      # final prediction
      next_word <- paste0('\n[', word, ']')
    } else if (is.null(word) || word != tok[m]) {
      # incorrect prediction
      next_word <- paste0('\n{', tok[m], ', !', ifelse(is.null(word), 'NULL', word), '}')
    } else {
      # correct prediction
      next_word <- paste0('\n', word)
    }
    
    # debug
    # cat('s=', s, '\n')
    # cat('predic=', next_word, '\n\n')

    result <- paste(result, next_word)
    
    s <- paste(tok[m-1], tok[m])
    m <- m+1 
  }
  
  result
}


print_predict <- function(word) {
  if (!is.null(word)) {
    cat('prediction: ', word)
  } else {
    cat("can't predict - no match")
  }
}

# main loop
repeat {
  s <- readline(prompt = "enter two or more words: ")
  if (s == '') {
    break
  } else {
    # clean s
    s <- clean_text(s)
    prediction <- predict_sentence(s)
    print_predict(prediction)
  }
}

  
cat('\nend program')
