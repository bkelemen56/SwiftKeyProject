# ---------------------------------------------------------------------
# model #3 - learn from trigrams and bigrams using katz backoff process
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

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

# controls training documents to use
use_small_data <- FALSE     # TRUE will use a small sub-set of the data (for development)
use_pct_data = .25         # percentage of each file to use as training

do_accuracy_calc <- FALSE  # this takes some time, so may not want always 

# discount factor for katz backoff model
discount_factor <- 0.5
use_unigram_model <- FALSE  # TRUE will also use the unigram in the backoff

# key for cache file
model_id <- paste0('model-3.', 
                   ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_')))

# ---------------------------------------------------------------------
# load documents
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
# clean data
# TODO: remove bad words
# TODO: add start/end sentince
# ---------------------------------------------------------------------

# clean the data
clean_corpus <- text_corpus
# TODO: add here additional cleaning stuff...
# TODO: remove 'RT' that are re-twittes

# ---------------------------------------------------------------------
# compute models
# ---------------------------------------------------------------------

cat('\ncomputing models\n')

load_from_cache <- function(model_id, model_type) {
  filename <- paste0('cache/', model_id, '.', model_type, '.cache')
  if (file.exists(filename)) {
    model <- read_rds(filename)
  } else {
    model <- NULL
  }
  model
}

save_to_cache <- function(model, model_id, model_type) {
  filename <- paste0('cache/', model_id, '.', model_type, '.cache')
  write_rds(model, filename)
}

# ---------------------------------------------------------------------
# unigram model:
#
# this model will produce an ordered (desc) table of words with (word, 
# count, frequency) in the corpus
# ---------------------------------------------------------------------
if (use_unigram_model) {
  cat('  unigram model\n')
  
  df_model.1 <- load_from_cache(model_id, '1-gram')
  if (is.null(df_model.1)) {
    
    dfm_unigram <- dfm(clean_corpus, ngrams = 1, removePunct = TRUE)
    df_model.1 <- tibble(word = featnames(dfm_unigram), n = colSums(dfm_unigram), freq = n / sum(dfm_unigram))  %>%
      arrange(desc(freq))
    
    save_to_cache(df_model.1, model_id, '1-gram')
  }

} else {
  df_model.1 <- NULL
}

# ---------------------------------------------------------------------
# bigram model:
#
# this model will produce an ordered (desc) table of bigrams with (root, 
# word, count, frequency) in the corpus. root will be the first word
# of the bigram
# ---------------------------------------------------------------------
cat('  bigram model\n')

df_model.2 <- load_from_cache(model_id, '2-gram')
if (is.null(df_model.2)) {
  
  dfm_bigram <- dfm(clean_corpus, ngrams = 2, removePunct = TRUE)
  
  # split bigram into first word and following word
  ss <- str_split(featnames(dfm_bigram), '_', n = 2)
  root <- sapply(ss, function(x) x[[1]])
  word <- sapply(ss, function(x) x[[2]])
  
  df_model.2 <- tibble(root = root, word = word, n = colSums(dfm_bigram)) %>%
    group_by(root) %>%
    mutate(freq = n / sum(n)) %>%
    ungroup()
  
  save_to_cache(df_model.2, model_id, '2-gram')
}

# ---------------------------------------------------------------------
# trigram model:
#
# this model will produce an ordered (desc) table of trigrams with 
# (root, word, count, frequency) in the corpus. root will be the first 
# two words of the trigram
# ---------------------------------------------------------------------
cat('  trigram model\n')

df_model.3 <- load_from_cache(model_id, '3-gram')
if (is.null(df_model.3)) {
  
  dfm_trigram <- dfm(clean_corpus, ngrams = 3, removePunct = TRUE)

  # split trigram into first 2 words and following word
  ss <- str_split(featnames(dfm_trigram), '_', n = 3)
  root <- sapply(ss, function(x) paste(x[[1]], x[[2]]))
  word <- sapply(ss, function(x) x[[3]])
  
  df_model.3 <- tibble(root = root, word = word, n = colSums(dfm_trigram)) %>%
    group_by(root) %>%
    mutate(freq = n / sum(n)) %>%
    ungroup()
  
  save_to_cache(df_model.3, model_id, '3-gram')
}

# now combine all models
models <- list(model.1 = df_model.1, model.2 = df_model.2, model.3 = df_model.3)

# print memory size of each model
cat('size of unigram model = ', object.size(df_model.1), 'bytes\n')
cat('size of bigram model = ', object.size(df_model.2), 'bytes\n')
cat('size of trigram model = ', object.size(df_model.3), 'bytes\n')

# ---------------------------------------------------------------------
# accuracy calculation
# ---------------------------------------------------------------------

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

# ---------------------------------------------------------------------
# prediction function
# ---------------------------------------------------------------------

predict_word <- function(models, s, discount_factor) {
  
  predict_word_recursive <- function(level, s, prev_words, min_prev_freq, alpha) {
    df_words <- data_frame(word = character(), freq = double())
    df_model <- models[[level]]
    
    if (level == 1) {
      if (use_unigram_model) {
        # need special case to avoid pocessing all the unigram model
        
        # filer out all words that would not help given their frequency and have not been seen
        df_words <- df_model %>%
          subset(n > ((min_prev_freq * sum(df_model$n) / alpha) + discount_factor) & !(word %in% prev_words)) %>%
          mutate(new_n = ifelse(n == 0, 0, n - discount_factor), 
                 new_freq = alpha * new_n / sum(n)) %>%
          select(word, freq = new_freq)
      }
    } else {
      # levels 2, 3, ...
      
      # get all predictions at this level
      df_words <- df_model %>%
        subset(root == s & !(word %in% prev_words)) %>%
        mutate(new_n = ifelse(n == 0, 0, n - discount_factor), 
               new_freq = alpha * new_n / sum(n)) %>%
        filter(new_freq > min_prev_freq) %>%
        select(word, freq = new_freq)
        
      if (nrow(df_words) > 0) {
        alpha <- 1 - sum(df_words$freq)
        prev_words = c(prev_words, df_words$word)
        min_prev_freq = min(df_words$freq)
      } else {
        alpha <- 1
      }
    
      # go down one level
      s <- sub(s, pattern = "^[[:alpha:]]*( |$)", replacement = "")  # remove first word
      df <- predict_word_recursive(level-1, s, prev_words = prev_words, min_prev_freq = min_prev_freq, alpha)
      
      df_words <- rbind(df_words, df)
    }
      
    df_words
  }
  
  # do predictions starting with trigrams (level 3)
  df_words <- predict_word_recursive(3, s, prev_words = c(), min_prev_freq = 0, alpha = 1) %>%
    arrange(desc(freq)) %>%
    slice(1:10)
  
  pretty_fmt(df_words)
}

# predicts the top 5 words for 's' and returns a string
# of words and frequency as %
predict_top_words <- function(s, n_words = 5) {
  
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

# ---------------------------------------------------------------------
# utility functions for interactive section
# ---------------------------------------------------------------------

clean_text <- function(s) {
  tok <- quanteda::tokenize(s, what = "word", removePunct = TRUE, removeNumbers = TRUE, 
                            removeSeparators = TRUE, removeHyphens = TRUE, removeTwitter = TRUE,
                            removeURL = TRUE)
  paste(tok[[1]], collapse = ' ')
}

pretty_fmt <- function(df) {
  str <- summarise(df, word = paste(paste0(word, '(', round(freq, 1), '%)'), collapse = ', '))
  str <- str_replace(str, ', $', '')
  str
}

# accepts a complete sentence
predict_sentence <- function(s) {
  s <- char_tolower(s)
  tok <- quanteda::tokenize(s)[[1]]
  
  s <- paste(tok[1], tok[2])
  m <- 3
  result <- s
  while (m <= (length(tok)+1)) {
    word <- predict_word(models, s, discount_factor)
    #word <- predict_top_words(s, 5)

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

# ---------------------------------------------------------------------
# main interactive loop
# ---------------------------------------------------------------------

cat('\nprediction:\n')

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
