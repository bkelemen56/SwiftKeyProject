# ---------------------------------------------------------------------
# model #4.4 - learn from 4-grams, trigrams and bigrams using katz backoff process
# use feature hashing and word encoding to reduce size of model
# use of data.table for faster processing.
#
# the n-gram model is pruned to heep max n_prune words for each root to reduce
# size without loosing precision.
#
# incremental processing. split train file into "n_lines_inc" increments,
# process each one and combine.
# 
# script to compute all n-gram models and save to cache
# ---------------------------------------------------------------------

library(tidyverse)
library(stringr)

library(tokenizers)

library(quanteda)
library(readtext)

library(data.table)
library(FeatureHashing)

source("~/R Workspace/data science class/10- capstone/SwiftKeyProject/src/bad_words.R")

# ---------------------------------------------------------------------
# load documents
# ---------------------------------------------------------------------

# # load documents using readtext
# load_documents <- function(use_small_data = TRUE, use_pct_data = 0.01) {
#   cat('loading documents [small= ', use_small_data, ', pct=', use_pct_data, ']\n')
#   
#   path = 'data'
#   pattern <- ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_'))
# 
#   raw_text <- readtext(paste0(path, '/all.train\\.', pattern, '*'), docvarsfrom  = 'filenames', dvsep = '\\.', verbosity = 0)
#   #raw_text <- str_split(raw_text$text, "\n")
#   
#   #text_corpus <- corpus(raw_text)
#   # # docvar2 will be the source
#   
#   # rm(raw_text)
#   # gc()
# 
#   #text_corpus
#   raw_text
# }

# load documents using readtext from a supplied filename
load_documents0 <- function(filename) {
  cat('loading document ', filename, '\n')
  readtext(filename, docvarsfrom  = 'filenames', dvsep = '\\.', verbosity = 0)
}

# # load documents using read_lines
# load_documents2 <- function(use_small_data = TRUE, use_pct_data = 0.01) {
#   cat('loading documents [small= ', use_small_data, ', pct=', use_pct_data, ']\n')
#   
#   path = 'data'
#   pattern <- ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_'))
#   
#   raw_text <- data_frame(file = dir(path, pattern = paste0('*train\\.', pattern, '*'), full.names = TRUE)) %>%
#     mutate(source = gsub('^(.+)\\.(.+)\\.(.+)\\.txt', '\\1', basename(file))) %>%
#     mutate(text = map(file, function(f) { read_lines(f, progress = interactive()) })) %>%
#     unnest(text) %>%
#     select(source, text) 
#   
#   raw_text
# }

# ---------------------------------------------------------------------
# clean data
# ---------------------------------------------------------------------

my_tokenize_words <- function(text, verbose = TRUE) {
  quanteda::tokenize(text, what = "word", ngrams = 1,
                     removePunct = TRUE, removeNumbers = TRUE, removeSymbols = TRUE,
                     removeTwitter = TRUE, removeURL = TRUE, 
                     simplify = TRUE, verbose = verbose)
}

# escape special characters in regex below
quotemeta <- function(string) str_replace_all(string, "(\\W)", "\\\\\\1")

clean_text <- function(text, to_lower = FALSE, tokenize_string = FALSE) {
  # some regex extracted from https://github.com/trinker/qdapRegex/blob/master/inst/dictionary_maintenance.R
  regex_list <- list(
    rm_underscores = "_+ *_*",
    rm_twitter_url = "(https?://t\\.co[^ ]*)|(t\\.co[^ ]*)",
    rm_url         = "(ftp|http|https)\\S+\\s*",
    rm_bad_words   = paste0("(^|\\s)", 
                            "(", paste(quotemeta(bad_words), collapse = "|"), ")",
                            "((.+?(?=\\s))|$)"),
    rm_RT          = "RT"
  )
  
  regex <- paste(sapply(regex_list, function(x) { paste0("(", x, ")")}), collapse = "|")
  
  s <- text
  if (to_lower) {
    s <- str_to_lower(s)
  }
  
  s <- trimws(str_replace_all(s, regex, " "))
  
  if (tokenize_string) {
    s <- my_tokenize_words(s, verbose = FALSE)
  }
  
  s
}

clean_data <- function(text_corpus) {
  cat('cleaning data\n')

  clean_text <- (text_corpus[[1]]) %>%
    paste(collapse = "\n") %>%
    tokenize_sentences(lowercase = TRUE, strip_punctuation = FALSE, simplify = TRUE) %>%
    clean_text()
  
  clean_text
}

# ---------------------------------------------------------------------
# model cache functions
# ---------------------------------------------------------------------

# loads a model from the cache folder
load_model_from_cache <- function(model_fname) {
  cat(paste0('loading model from cache [', model_fname, ']\n'))

  filename <- paste0('~/R Workspace/data science class/10- capstone/SwiftKeyProject/cache/', model_fname)
  if (file.exists(filename)) {
    model <- read_rds(filename)
  } else {
    cat(paste0("Filename ", filename, " does not exist\n"))
    model <- NULL
  }
  
  # assure all keys and indices are set
  model1 <- model[[1]]
  if (!("word" %in% key(model1))) setkey(model1, word)
  if (!("id_word" %in% indices(model1))) setindex(model1, id_word)
  
  model2 <- model[[2]]
  if (!("hash_root" %in% key(model2))) setkey(model2, hash_root)
  if (!("hash_root__n_gram" %in% indices(model2))) setindex(model2, hash_root, n_gram)
  
  cat(paste0('  model size = ', round(object.size(model) / 1024 / 2014, 1), ' MB\n'))
  
  model
}

# saved the model to the cache folder
save_to_cache <- function(model, model_fname) {
  filename <- paste0('~/R Workspace/data science class/10- capstone/SwiftKeyProject/cache/', model_fname)
  write_rds(model, filename)
}

save_model_to_text_files <- function(model, model_fname) {
  cat(paste0('saving model into two text files: ', model_fname, '[-1|-2]\n'))
  filename <- paste0('~/R Workspace/data science class/10- capstone/SwiftKeyProject/cache/', model_fname)
  write_csv(model[[1]], paste0(filename, "-1"))
  write_csv(model[[2]], paste0(filename, "-2"))
}

# loads a model from the cache folder
load_model_from_text_files <- function(model_fname) {
  cat(paste0('loading model from text files: ', model_fname, '[-1|-2]\n'))
  
  filename <- paste0('~/R Workspace/data science class/10- capstone/SwiftKeyProject/cache/', model_fname)
  filename1 <- paste0(filename, "-1")
  filename2 <- paste0(filename, "-2")
  
  if (file.exists(filename1) && file.exists(filename2)) {
    model1 <- data.table(read_csv(filename1))
    model2 <- data.table(read_csv(filename2))
    model <- list(model1 = model1, model2 = model2)
  } else {
    cat(paste0("One or both filenames ", filename1, " or ", filename2, " donn't exist\n"))
    return(NULL)
  }
  
  # assure all keys and indices are set
  model1 <- model[[1]]
  if (!("word" %in% key(model1))) setkey(model1, word)
  if (!("id_word" %in% indices(model1))) setindex(model1, id_word)
  
  model2 <- model[[2]]
  if (!("hash_root" %in% key(model2))) setkey(model2, hash_root)
  if (!("hash_root__n_gram" %in% indices(model2))) setindex(model2, hash_root, n_gram)
  
  cat(paste0('  model size = ', round(object.size(model) / 1024 / 2014, 1), ' MB\n'))
  
  model
}

# ---------------------------------------------------------------------
# compute and save models
#
# model_id            prefix for files to cache (e.g. "model-3")
# do_mem_size_calc    calcualte and print size of each model?
# ---------------------------------------------------------------------

# # returns the prefix for model filename without the n-gran part
# model_id_fname_prefix <- function(model_id, use_small_data = TRUE, use_pct_data = 0.01) {
#   paste0(model_id, ".",
#          ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_')))
# }

# calculates model
calc_model <- function(clean_corpus, 
                       model_fname, 
                       n_prune = 10,
                       do_mem_size_calc = FALSE,
                       debug_model_calc = FALSE) {
  
  cat('computing model [', model_fname, ']\n')
  
  # ---------------------------------------------------------------------
  # unigram model:
  #
  # this model will produce a data.table with (word, count, frequency and
  # id_word) from the corpus. the data.table will use word as the key
  # and have an index on id_word
  # ---------------------------------------------------------------------
  cat('  unigram model\n')
  
  cat('    tokenize\n')
  word_tokens <- my_tokenize_words(clean_corpus)

  cat('    create data.table\n')
  # prepare dt1 with all unique words, their counts and id's
  dt.1 <- data.table(word = as_vector(word_tokens))
  
  rm(word_tokens)
  gc()
  
  cat('    counting words and calc freq\n')
  dt.1 <- dt.1[, list(n = .N), by = word]       # word count 
  dt.1[, freq := n / sum(n)]                # word frequency
  dt.1[, id_word := as.integer(seq(1, nrow(dt.1)))]             # id for each word
  
  setkey(dt.1, word)                               # clustered index on word
  setindex(dt.1, id_word)                           # index in id_word
  
  # ---------------------------------------------------------------------
  # n-gram models (n = 2:4)
  #
  # this model will produce a data.table of bigrams, trigrams and 4-grams
  # with (hash_root, id_word, count, freq) from the corpus. root will be 
  # n-gram without the last word.
  #
  # hashing is done via hash.value from FeatureHashing
  # ---------------------------------------------------------------------
  cat('  n-gram models (n=2:4)\n')
  
  cat('    tokenize sentences\n')
  # break the text by sentences
  sentence <- clean_corpus   # we now have sentences here!
  # sentence <- 
  #   quanteda::tokenize(clean_corpus, what = "sentence", 
  #                      removePunct = TRUE, removeSymbols = TRUE, removeNumbers = TRUE, 
  #                      removeTwitter = TRUE, removeURL = TRUE, 
  #                      simplify = TRUE, verbose = TRUE) 
  
  cat('    tokenize n-grams\n')
  
  # create ngrams from each sentence, n=2:4
  ngram_tokens <- 
    quanteda::tokenize(sentence, what = "word", ngrams = 2:4,
                       removePunct = TRUE, removeNumbers = TRUE, removeSymbols = TRUE,
                       removeTwitter = TRUE, removeURL = TRUE, 
                       simplify = TRUE, verbose = TRUE)
  
  cat('    create data.table\n')
  # create data.table for fast processing
  dt.n <- data.table(ngram_tok = ngram_tokens)      # get all ngrams, n=2:4
  
  rm(ngram_tokens)
  gc()
  
  cat('    counting n-grams\n')
  dt.n <- dt.n[, list(n = .N), by = ngram_tok]      # ngram count
  
  cat('    splitting n-grams\n')
  dt.n[, ngram := str_split(ngram_tok, "_")]        # separate ngram into root and word
  dt.n[, root := sapply(ngram, function(x) {paste(x[1:(length(x) - 1)], collapse = " ")})]
  dt.n[, word := sapply(ngram, function(x) {x[length(x)]})]
  
  dt.n[, n_gram := sapply(ngram, length)]           # calculate 'n' from 'n'-gram
  
  cat('    hashing root\n')
  # add hash values and word ids
  dt.n[, hash_root := hashed.value(root)]           # hash the root
  
  cat('    adding id_word\n')
  setindex(dt.n, word)
  dt.n[dt.1, id_word := i.id_word, on = "word"]     # get id's for words
  dt.n[is.na(id_word)]                              # remove words not mapped (ex. in URLs)
  
  cat('    calc freq\n')
  # calculate frequencies for each word/root
  dt.n[, freq := n / sum(n), by = hash_root]        # calc frequences x root
  
  # create compact model
  if (!debug_model_calc) {
    dt.n[, ngram_tok := NULL]                         # drop non-needed variables
    dt.n[, ngram := NULL]
    dt.n[, root := NULL]
    dt.n[, word := NULL]
  }
  
  # model 4.2: finally prune the model: keep top n_prune words for each root
  cat('    prune\n')
  setindex(dt.n, hash_root)
  
  # based on http://stackoverflow.com/questions/34753050/data-table-select-first-n-rows-within-group
  v1 <- dt.n[order(-freq), .I[1:n_prune], by = hash_root]$V1
  dt.n <- dt.n[na.omit(v1)]
  rm(v1)
  
  # old code (slow):
  # dt.n <- dt.n[order(-freq), head(.SD, n_prune), by = hash_root]
  
  # setindex(dt.n, NULL)
  setkey(dt.n, hash_root)
  setindex(dt.n, hash_root, n_gram)                 # final indices
  
  if (do_mem_size_calc) {
    # print memory size of each model
    cat('size of unigram model = ', object.size(dt.1), 'bytes\n')
    cat('size of n-gram models = ', object.size(dt.n), 'bytes\n')
  }
  
  # ---------------------------------------------------------------------
  # now combine all models
  # ---------------------------------------------------------------------
  model <- list(model.1 = dt.1, model.n = dt.n)
  
  model
}


# # calculates and saves model to cache (this is very expensive)
# calc_and_save_model <- function(clean_corpus, 
#                                 model_fname, 
#                                 n_prune = 10,
#                                 do_mem_size_calc = FALSE,
#                                 debug_model_calc = FALSE) {
#   
#   model <- calc_models(clean_corpus, 
#                        model_fname, 
#                        n_prune,
#                        do_mem_size_calc, 
#                        debug_model_calc)
#   
#   cat('    saving to cache\n')
#   save_to_cache(model, model_fname)
#   
#   model
# }

# ---------------------------------------------------------------------
# accuracy calculations
# ---------------------------------------------------------------------

model_accuracy <- function(model, fname, n_prune = 5, discount_factor = 0.5, use_unigram = FALSE, max_model_level = 4,
                           n_lines_to_process = -1) {
  
  # load test file and clean
  clean_text <- load_documents0(fname) %>% 
    clean_data()
  if (n_lines_to_process > 0) {
    clean_text <- clean_text[1:n_lines_to_process]
  }
  
  cat("predicting")
  
  n_words_to_predict <- 0
  n_correct_predictions <- 0
  n_incorrect_predictions <- 0
  n_null_predictions <- 0
  
  n_line <- 0
  for (clean_text_line in clean_text) {
    #cat(paste0(clean_text_line, "\n"))
    # debug
    #if (n_line == 100) { break }
    
    if (n_line %% 1000 == 0) { cat(paste0("\n", n_line)) }
    if (n_line %% 10 == 0) { cat(".") }
    n_line <- n_line + 1
    
    #cat("\n")
    #cat(paste0("New line" , clean_text_line, "\n"))
    
    clean_text_split <- my_tokenize_words(clean_text_line, verbose = FALSE)
    
    m <- length(clean_text_split)
    
    # debug
    #print(clean_text_split)
    #print(m)
    
    # iterate over every 3-gram
    for (i in c(1:m)) {
      if (i < 3) {
        #cat(paste("skipping first 2 words of sentence:", clean_text_split[i], "\n"))
      } else if (i == m) {
        #cat("skipping last word in sentence.\n")
      } else {
        n_words_to_predict <- n_words_to_predict + 1
        text_to_predict <- paste(clean_text_split[i - 2], clean_text_split[i - 1], clean_text_split[i])
        expected_word <- clean_text_split[i + 1]
          
        #print(text_to_predict)
        #print(paste("True next value:", expected_word))

        predicted_words <- predict_words(model, 
                                         text_to_predict, 
                                         discount_factor = discount_factor, 
                                         use_unigram = use_unigram,
                                         max_model_level = max_model_level)
        
        #print("Predictions:")
        #print(predicted_words)
        
        if (!is.null(predicted_words)) {
          if (nrow(predicted_words[word == expected_word]) != 0) {
            #print("  > correct prediction")
            n_correct_predictions <- n_correct_predictions + 1
          } else {
            #print("  > incorrect prediction")
            n_incorrect_predictions <- n_incorrect_predictions + 1
          }
        } else {
          #print("  > NULL prediction")
          n_null_predictions <- n_null_predictions + 1
        }
      }
    }
  }
  
  cat("end loop")
  
  cat("\n")
    
  list(n_words_to_predict = n_words_to_predict,
       n_correct_predictions = n_correct_predictions,
       n_incorrect_predictions = n_incorrect_predictions,
       n_null_predictions = n_null_predictions,
       accuracy = (n_correct_predictions / n_words_to_predict)
  )
}

# ---------------------------------------------------------------------
# combine models
# ---------------------------------------------------------------------

combine_models <- function(model1, model2, n_prune = 5) {
  
  # ---- recalculate id_word --------------
  
  model.1a <- model1[[1]]
  model.1b <- model2[[1]]
  
  model.na <- model1[[2]]
  model.nb <- model2[[2]]
  
  m <- model.1a[, max(id_word)]                               # model.1b ids should start at m+1
  model.1b[, new_id_word := id_word + m]                      # reallocate word ids
  model.1b[model.1a, new_id_word := i.id_word, on = "word"]   # update id's for dup words in both models
  model.nb[model.1b, new_id_word := i.new_id_word, on = "id_word"]    # get new id's
  
  model.1b[, id_word := new_id_word]                          # now update id_word and drop new...
  model.1b[, new_id_word := NULL]
  
  model.nb[, id_word := new_id_word]
  model.nb[, new_id_word := NULL]
  
  # ---- combine unigram models --------------
  
  cat('    combining unigram models\n')
  dt.1 <- rbind(model.1a, model.1b)
  
  # re-calculate count and frequencies
  dt.1 <- unique(dt.1[, list(n = sum(n), freq = 0, id_word), by = word])  # new word count 
  dt.1[, freq := n / sum(n)]                                  # new word frequency

  setindex(dt.1, word)                                        # set indices
  setindex(dt.1, id_word)
  
  cat(paste0('      size(model.1a) = ', object.size(model.1a), '\n'))
  cat(paste0('      size(model.1b) = ', object.size(model.1b), '\n'))
  cat(paste0('      size(dt.1)     = ', object.size(dt.1), '\n'))
  
  # --- combine n-gram models ---------------

  cat('    combining n-gram models\n')
  dt.n <- rbind(model.na, model.nb)

  cat('    re-calc count of n-grams\n')
  dt.n.tmp <- unique(dt.n[, list(n = sum(n), n_gram, freq = 0), by = c("hash_root", "id_word")])    # ngram count
  setcolorder(dt.n.tmp, c("hash_root", "n", "n_gram", "id_word", "freq"))
  dt.n <- dt.n.tmp
  
  setindex(dt.n, hash_root)
  
  cat('    calc freq\n')
  dt.n[, freq := n / sum(n), by = hash_root]                  # calc frequences x root
  
  # model 4.2: prune the model: keep top n_prune words for each root
  cat('    prune\n')
  
  # based on http://stackoverflow.com/questions/34753050/data-table-select-first-n-rows-within-group
  v1 <- dt.n[order(-freq), .I[1:n_prune], by = hash_root]$V1
  dt.n <- dt.n[na.omit(v1)]
  rm(v1)
  
  # cat('      benchmarking:\n')
  # print(system.time({
  #   v1 <- dt.n[order(-freq), .I[1:n_prune], by = hash_root]$V1
  #   dt1 <- dt.n[na.omit(v1)]
  #   rm(v1)
  # }))
  # print(system.time(dt2 <- dt.n[order(-freq), head(.SD, n_prune), by = hash_root]))
  # 
  # print(paste("Identical? ", identical(dt1, dt2)))
  # stop('aborted')
  # 
  # old code (slow)
  # dt.n <- dt.n[order(-freq), head(.SD, n_prune), by = hash_root]

  # setindex(dt.n, NULL)
  setindex(dt.n, hash_root, n_gram)                           # final indices
  
  cat(paste0('      size(model.na) = ', object.size(model.na), '\n'))
  cat(paste0('      size(model.nb) = ', object.size(model.nb), '\n'))
  cat(paste0('      size(dt.n)     = ', object.size(dt.n), '\n'))
  
  # combine the new (combined) model
  model <- list(model.1 = dt.1, model.n = dt.n)
  
  model
}

# ---------------------------------------------------------------------
# prediction function
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# prediction function
#
# models                 models to use
# s                      text to predict
# discount factor        for katz backoff model
# use_unigram_model      should we also use the unigram in the backoff?
# max_model_level        start with this level for backoff
#
# returns                data.table of predicted words
# ---------------------------------------------------------------------

predict_words <- function(models, s, discount_factor = 0.5, use_unigram_model = FALSE, max_model_level = 4) {
  
  predict_words_recursive <- function(level, s, prev_id_words, min_prev_freq, alpha) {
    dt_words <- data.table(id_word = integer(), freq = double())
    
    if (level == 1) {
      if (use_unigram_model) {
        # need special case to avoid pocessing all the unigram model
        dt_model <- models[[1]]
        
        # filer out all words that would not help given their frequency and have not been seen
        dt_words <- 
          dt_model[n > ((min_prev_freq * sum(dt_model$n) / alpha) + discount_factor) & 
                     !(id_word %in% prev_id_words), 
                   .(id_word, 
                     freq = alpha * (n - discount_factor) / sum(n))
                   ]
      }
    } else {
      #cat(paste('prediction for = ', s, ' [level', level, ']\n'))
      
      # levels 2, 3, ...
      dt_model <- models[[2]]
      
      # get all predictions at this level
      hash_s <- hashed.value(s)
      dt_words <- dt_model[hash_root == hash_s][ 
                             !(id_word %in% prev_id_words) & 
                             n_gram == level &
                             n > ((min_prev_freq * sum(dt_model$n) / alpha) + discount_factor), 
                           .(id_word, 
                             freq = alpha * (n - discount_factor) / sum(n))
                           ]
      
      #print(dt_words)
      
      if (nrow(dt_words) > 0) {
        alpha <- alpha * (1 - sum(dt_words$freq))
        prev_id_words = c(prev_id_words, dt_words$id_word)
        min_prev_freq = min(dt_words$freq)
      } 
      
      # go down one level
      s <- sub(s, pattern = "^[[:alpha:]]*( |$)", replacement = "")  # remove first word
      dt <- predict_words_recursive(level - 1, 
                                   s, 
                                   prev_id_words = prev_id_words, 
                                   min_prev_freq = min_prev_freq, 
                                   alpha = alpha)
      
      dt_words <- rbind(dt_words, dt)
    }
    
    dt_words
  }
  
  #cat('predicting:', s, '\n')
  
  # do predictions starting with max level n-grams
  dt_words <- predict_words_recursive(max_model_level, 
                                     s, 
                                     prev_id_words = c(), 
                                     min_prev_freq = 0, 
                                     alpha = 1) [order(-freq)] [,head(.SD, 10)]
  
  
  # m1 <- model[[1]]
  # dtw <- copy(dt_words)
  # dtw[m1, word := i.word, on = "id_word"]
  # dtw <- dtw[, .(word, freq)]   
  
  # get words from id's
  #m1 <- model[[1]]
  
  dt_words <- dt_words[model[[1]], word := i.word, on = "id_word"][, .(word, freq)]   
  if (nrow(dt_words) > 0) {
    setkey(dt_words, word)
  }
  
  dt_words[order(-freq)]
}

pretty_fmt_prediction <- function(dt_words) {
  if (nrow(dt_words) == 0) {
    str <- ""
  } else {
    str <- summarise(dt_words, word = paste(paste0(word, '(', round(freq, 5), '%)'), collapse = ', '))
    str <- str_replace(str, ', $', '')
  }
  str
}
