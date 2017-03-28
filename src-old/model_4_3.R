# ---------------------------------------------------------------------
# model #4.1 - learn from 4-grams, trigrams and bigrams using katz backoff process
#
# model data storage has been optmized: n-grams stored as hashed values,
# words indexed, data.table for faster retrieval. All models stored in one
# file "model-4-1.xxxxx.cache"
#
# the interactive process reads a sentince and predicts each word.
# a list of words predicted are displayed.
#
# ---------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(FeatureHashing)

source("src/calc_models_4_3.R")

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

model_id <- "model-4-3"
debug_model_calc <- FALSE

# controls training documents to use
use_small_data <- TRUE     # TRUE will use a small sub-set of the data (for development)
use_pct_data = .01           # percentage of each file to use as training

do_accuracy_calc <- TRUE

# discount factor for katz backoff model
discount_factor <- 0.5
use_unigram_model <- FALSE # TRUE will also use the unigram in the backoff
max_model_level <- 4       # start with this level for backoff

# ---------------------------------------------------------------------
# load models
# ---------------------------------------------------------------------

cat(paste0('loading models from cache [', model_id, ']\n'))
if (debug_model_calc) {
  models <- NULL
} else {
  models <- load_from_cache(
    model_id_fname_prefix(model_id, use_small_data, use_pct_data))
}

if (is.null(models)) {
  cat('models not found, calculating\n')
  models <- 
    load_documents(use_small_data, use_pct_data) %>% 
    clean_data() %>%
    calc_and_save_models(model_id, use_small_data, use_pct_data, debug_model_calc = debug_model_calc)
}

# ---------------------------------------------------------------------
# accuracy calculation
# ---------------------------------------------------------------------

if (do_accuracy_calc) {
  cat("accuracy calculation\n")
  
  accuracy_train <- model_accuracy(models, "train")
  accuracy_test <- model_accuracy(models, "test")
}

# ---------------------------------------------------------------------
# prediction function
# ---------------------------------------------------------------------

predict_word <- function(models, s, discount_factor) {
  
  predict_word_recursive <- function(level, s, prev_id_words, min_prev_freq, alpha) {
    dt_words <- data.table(id_word = integer(), freq = double())
    
    if (level == 1) {
      if (use_unigram_model) {
        # need special case to avoid pocessing all the unigram model
        dt_model <- models[[1]]
        
        # filer out all words that would not help given their frequency and have not been seen
        
        # dt_words <- dt_model %>%
        #   subset(n > ((min_prev_freq * sum(df_model$n) / alpha) + discount_factor) & !(word %in% prev_words)) %>%
        #   mutate(new_n = ifelse(n == 0, 0, n - discount_factor), 
        #          new_freq = alpha * new_n / sum(n)) %>%
        #   select(word, freq = new_freq)
        
        dt_words <- 
          dt_model[n > ((min_prev_freq * sum(dt_model$n) / alpha) + discount_factor) & 
                     !(id_word %in% prev_id_words), 
                   .(id_word, 
                     freq = alpha * (n - discount_factor) / sum(n))
                  ]
        
      }
    } else {
      # levels 2, 3, ...
      dt_model <- models[[2]]
      
      # get all predictions at this level

      # dt_words <- df_model %>%
      #   subset(root == s & !(word %in% prev_words)) %>%
      #   mutate(new_n = ifelse(n == 0, 0, n - discount_factor), 
      #          new_freq = alpha * new_n / sum(n)) %>%
      #   filter(new_freq > min_prev_freq) %>%
      #   select(word, freq = new_freq)
      
      hash_s <- hashed.value(s)
      dt_words <- dt_model[hash_root == hash_s & 
                             !(id_word %in% prev_id_words) & 
                             n_gram == level &
                             n > ((min_prev_freq * sum(dt_model$n) / alpha) + discount_factor), 
                           .(id_word, 
                             freq = alpha * (n - discount_factor) / sum(n))
                          ]

      if (nrow(dt_words) > 0) {
        alpha <- alpha * (1 - sum(dt_words$freq))
        prev_id_words = c(prev_id_words, dt_words$id_word)
        min_prev_freq = min(dt_words$freq)
      } 
    
      # go down one level
      s <- sub(s, pattern = "^[[:alpha:]]*( |$)", replacement = "")  # remove first word
      dt <- predict_word_recursive(level - 1, 
                                   s, 
                                   prev_id_words = prev_id_words, 
                                   min_prev_freq = min_prev_freq, 
                                   alpha = alpha)
      
      dt_words <- rbind(dt_words, dt)
    }
      
    dt_words
  }
  
  # do predictions starting with max level n-grams
  dt_words <- predict_word_recursive(max_model_level, 
                                     s, 
                                     prev_id_words = c(), 
                                     min_prev_freq = 0, 
                                     alpha = 1) [order(-freq)] [,head(.SD, 10)]

  pretty_fmt(models[[1]], dt_words)
}

# predicts the top 5 words for 's' and returns a string
# of words and frequency as %
# predict_top_words <- function(s, n_words = 5) {
#   
#   # start with empty set of words
#   words <- data_frame(word=character(), freq=double())
#   
#   # check trigrams
#   df <- subset(df_model1, bigram == s)
#   if (nrow(df) > 0) {
#     n_words_to_get <- min(n_words - length(words), nrow(df))
#     df <- df %>% 
#       arrange(desc(freq)) %>% 
#       slice(1:n_words_to_get) %>% 
#       transmute(word = word, freq = round(freq*100, 2))
#     words <- rbind(words, df)
#   }
#   
#   # check bigrams if more words are needed
#   if (length(words) < n_words) {
#     s <- str_split(s, ' ', n = 2)[[1]][[2]]  # use the second word read
#     df <- subset(df_model2, unigram == s)
#     if (nrow(df) > 0) {
#       n_words_to_get <- min(n_words - length(words), nrow(df))
#       df <- df %>% 
#         arrange(desc(freq)) %>% 
#         slice(1:n_words_to_get) %>% 
#         transmute(word = word, freq = round(freq*100, 1))
#       words <- rbind(words, df)
#     }
#   }
#   
#   pretty_fmt(words)
# }

# ---------------------------------------------------------------------
# utility functions for interactive section
# ---------------------------------------------------------------------

clean_text <- function(s) {
  tok <- quanteda::tokenize(s, what = "word", removePunct = TRUE, removeNumbers = TRUE, 
                            removeSeparators = TRUE, removeHyphens = TRUE, removeTwitter = TRUE,
                            removeURL = TRUE)
  paste(tok[[1]], collapse = ' ')
}

pretty_fmt <- function(model.1, dt) {
  dt2<- copy(dt)
  dt2[model.1, word := i.word, on = "id_word"]     # get words from id's
  
  str <- summarise(dt2, word = paste(paste0(word, '(', round(freq, 5), '%)'), collapse = ', '))
  str <- str_replace(str, ', $', '')
  str
}

# accepts a complete sentence
predict_sentence <- function(s) {
  s <- char_tolower(s)
  tok <- quanteda::tokenize(s)[[1]]
  
  s <- paste(tok[1], tok[2], tok[3])
  m <- 4
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
    
    result <- paste(result, next_word)
    
    s <- paste(tok[m-2], tok[m-1], tok[m])
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
# debug new model 4.1
# ---------------------------------------------------------------------

# just debug model 4.1 vs. 3.1
debug_model_4_1 <- function() {
  model.4 <- models
  model.4.1 <- models[[1]]
  model.4.n <- models[[2]] 
  
  model.3.1 <- load_from_cache(model_id_fname_prefix('model-3', use_small_data, use_pct_data), '1-gram')
  model.3.2 <- load_from_cache(model_id_fname_prefix('model-3', use_small_data, use_pct_data), '2-gram')
  model.3.3 <- load_from_cache(model_id_fname_prefix('model-3', use_small_data, use_pct_data), '3-gram')
  model.3.4 <- load_from_cache(model_id_fname_prefix('model-3', use_small_data, use_pct_data), '4-gram')
  
  model.3.1
}

# ---------------------------------------------------------------------
# main interactive loop
# ---------------------------------------------------------------------

if (debug_model_calc) {
  debug_model_4_1()
} else {
  cat('\nprediction:\n')
  
  repeat {
    cat('\n')
    s <- readline(prompt = "enter three or more words: ")
    if (s == '') {
      break
    } else {
      # clean s
      s <- clean_text(s)
      prediction <- predict_sentence(s)
      print_predict(prediction)
    }
  }
}

cat('\nend program')
