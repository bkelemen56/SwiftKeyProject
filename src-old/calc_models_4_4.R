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

source("src/bad_words.R")

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

clean_data <- function(text_corpus) {
  
  # escape special characters in regex below
  quotemeta <- function(string) str_replace_all(string, "(\\W)", "\\\\\\1")
  
  # some regex extracted from https://github.com/trinker/qdapRegex/blob/master/inst/dictionary_maintenance.R
  remove_multi_regex <- function(x) {
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
    
    str_replace_all(x, regex, " ")
  }

  cat('cleaning data\n')

  clean_text <- (text_corpus[[1]]) %>%
    paste(collapse = "\n") %>%
    tokenize_sentences(lowercase = TRUE, strip_punctuation = FALSE, simplify = TRUE) %>%
    remove_multi_regex()
  
  clean_text
}

# ---------------------------------------------------------------------
# model cache functions
# ---------------------------------------------------------------------

# loads a model from the cache folder
load_from_cache <- function(model_fname) {
  filename <- paste0('cache/', model_fname)
  if (file.exists(filename)) {
    model <- read_rds(filename)
  } else {
    cat(paste0("Filename ", filename, " does not exist\n"))
    model <- NULL
  }
  model
}

# saved the model to the cache folder
save_to_cache <- function(model, model_fname) {
  filename <- paste0('cache/', model_fname)
  write_rds(model, filename)
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
  word_tokens <- quanteda::tokenize(clean_corpus, what = "word", ngrams = 1,
                                    removePunct = TRUE, removeNumbers = TRUE, removeSymbols = TRUE,
                                    removeTwitter = TRUE, removeURL = TRUE, 
                                    simplify = TRUE, verbose = TRUE) 
  
  cat('    create data.table\n')
  # prepare dt1 with all unique words, their counts and id's
  dt.1 <- data.table(word = as_vector(word_tokens))
  
  rm(word_tokens)
  gc()
  
  cat('    counting words and calc freq\n')
  dt.1 <- dt.1[, list(n = .N), by = word]       # word count 
  dt.1[, freq := n / sum(n)]                # word frequency
  dt.1[, id_word := as.integer(seq(1, nrow(dt.1)))]             # id for each word
  setindex(dt.1, word)                              # index on word
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

model_accuracy <- function(models, dataset) {
  cat("Model accuracy not implemented yet.\n")
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