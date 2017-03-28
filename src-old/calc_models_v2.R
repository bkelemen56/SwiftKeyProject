# ---------------------------------------------------------------------
# model #3.1 - learn from 4-grams, trigrams and bigrams using katz backoff process
#
# script to compute all n-gram models and save to cache
# ---------------------------------------------------------------------

library(tidyverse)
library(stringr)

library(quanteda)
library(readtext)

# ---------------------------------------------------------------------
# load documents
# ---------------------------------------------------------------------

# load documents using readtext
load_documents1 <- function(use_small_data = TRUE, use_pct_data = 0.01) {
  cat('loading documents [small= ', use_small_data, ', pct=', use_pct_data, ']\n')
  
  path = 'data'
  pattern <- ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_'))

  raw_text <- readtext(paste0(path, '/*train\\.', pattern, '*'), docvarsfrom  = 'filenames', dvsep = '\\.', verbosity = 0)
  text_corpus <- corpus(raw_text)
  # # docvar2 will be the source
  # 
  rm(raw_text)
  gc()

  text_corpus
  
}

# load documents using read_lines
load_documents2 <- function(use_small_data = TRUE, use_pct_data = 0.01) {
  cat('loading documents [small= ', use_small_data, ', pct=', use_pct_data, ']\n')
  
  path = 'data'
  pattern <- ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_'))
  
  raw_text <- data_frame(file = dir(path, pattern = paste0('*train\\.', pattern, '*'), full.names = TRUE)) %>%
    mutate(source = gsub('^(.+)\\.(.+)\\.(.+)\\.txt', '\\1', basename(file))) %>%
    mutate(text = map(file, function(f) { read_lines(f, progress = interactive()) })) %>%
    unnest(text) %>%
    select(source, text)

  raw_text
}

# ---------------------------------------------------------------------
# clean data
# TODO: remove bad words
# TODO: add start/end sentince
# ---------------------------------------------------------------------

clean_data <- function(text_corpus) {
  # clean the data
  clean_corpus <- text_corpus
  # TODO: add here additional cleaning stuff...
  # TODO: remove 'RT' that are re-twittes
  
  clean_corpus
}

# ---------------------------------------------------------------------
# compute models
#
# model_id            prefix for files to cache (e.g. "model-3")
# do_mem_size_calc    calcualte and print size of each model?
# ---------------------------------------------------------------------

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

calc_and_save_models <- function(clean_corpus, model_id, do_mem_size_calc = FALSE) {

  cat('\ncomputing models ', model_id, '\n')
  
  # key for cache file (leave model 3 to reuse cache)
  model_id <- paste0('model-3.', 
                     ifelse(use_small_data, 'small', format(use_pct_data, decimal.mark = '_')))
  
  # ---------------------------------------------------------------------
  # unigram model:
  #
  # this model will produce an ordered (desc) table of words with (word, 
  # count, frequency) in the corpus
  # ---------------------------------------------------------------------
  cat('  unigram model\n')
  
  dfm_unigram <- dfm(clean_corpus, ngrams = 1, removePunct = TRUE)
  df_model.1 <- tibble(word = featnames(dfm_unigram), n = colSums(dfm_unigram), freq = n / sum(dfm_unigram))  %>%
    arrange(desc(freq))
  
  save_to_cache(df_model.1, model_id, '1-gram')

  rm(dfm_unigram)
  gc()

  # ---------------------------------------------------------------------
  # bigram model:
  #
  # this model will produce an ordered (desc) table of bigrams with (root, 
  # word, count, frequency) in the corpus. root will be the first word
  # of the bigram
  # ---------------------------------------------------------------------
  cat('  bigram model\n')
  
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
  
  rm(dfm_bigram)
  gc()
  
  # ---------------------------------------------------------------------
  # trigram model:
  #
  # this model will produce an ordered (desc) table of trigrams with 
  # (root, word, count, frequency) in the corpus. root will be the first 
  # two words of the trigram
  # ---------------------------------------------------------------------
  cat('  trigram model\n')
  
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

  rm(dfm_trigram)
  gc()
  
  # ---------------------------------------------------------------------
  # 4-gram model:
  #
  # this model will produce an ordered (desc) table of 4-grams with 
  # (root, word, count, frequency) in the corpus. root will be the first 
  # three words of the 4-gram
  # ---------------------------------------------------------------------
  cat('  4-gram model\n')
  
  dfm_4gram <- dfm(clean_corpus, ngrams = 4, removePunct = TRUE)
  
  # split 4gram into first 3 words and following word
  ss <- str_split(featnames(dfm_4gram), '_', n = 4)
  root <- sapply(ss, function(x) paste(x[[1]], x[[2]], x[[3]]))
  word <- sapply(ss, function(x) x[[4]])
  
  df_model.4 <- tibble(root = root, word = word, n = colSums(dfm_4gram)) %>%
    group_by(root) %>%
    mutate(freq = n / sum(n)) %>%
    ungroup()
  
  save_to_cache(df_model.4, model_id, '4-gram')
  
  rm(dfm_4gram)
  gc()
  
  if (do_mem_size_calc) {
    # print memory size of each model
    cat('size of unigram model = ', object.size(df_model.1), 'bytes\n')
    cat('size of bigram model = ', object.size(df_model.2), 'bytes\n')
    cat('size of trigram model = ', object.size(df_model.3), 'bytes\n')
    cat('size of 4-gram model = ', object.size(df_model.4), 'bytes\n')
  }
  
  # ---------------------------------------------------------------------
  # now combine all models
  # ---------------------------------------------------------------------
  models <- list(model.1 = df_model.1, model.2 = df_model.2, model.3 = df_model.3, model.4 = df_model.4)
  
  models
}
