# ---------------------------------------------------------------------
# model #5.0 - Final model. 
#
# Utility functions used to load documents, clean text, calculate models, 
# combine (sub) models, calculate accuracy and predict words.
#
# model learn from 4-grams, trigrams and bigrams using stupid katz backoff 
# algorithm using partial n-gram hashing and word encoding to reduce size 
# of the model. used data.table for faster processing.
#
# the n-gram model is pruned to heep max n_prune words for each root to reduce
# size without loosing precision.
#
# allows incremental processing. split train file into "n_lines_inc" increments,
# process each one and combine.
# ---------------------------------------------------------------------

source("R/bad_words.R")

library(tokenizers)

library(quanteda)
library(readtext)

library(FeatureHashing)

# ---------------------------------------------------------------------
# load documents
# ---------------------------------------------------------------------

#' load document text files.
#'
#' \code{load_documents} loads the contents of text file(s) using 
#'   readtext::readtext function.
#' 
#' @param filename complete filename to read.
#' @return a data.frame with the text read from each file as well as
#'   meta-data gathered from the filename.
#' @examples
#'   load_documents("all.train-001.txt")
#'   
load_documents <- function(filename) {
  flog.trace("start: load_documents")
  
  filename <- paste0(PATH_DATA, filename)
  flog.info(paste("loading document", filename))

  documents <- readtext(filename, docvarsfrom  = 'filenames', dvsep = '\\.', verbosity = 0)
  
  flog.trace("end: load_documents")
  documents
}

# ---------------------------------------------------------------------
# clean data
# ---------------------------------------------------------------------

#' tokenize text into 1-grams (words).
#' 
#' \code{my_tokenize_words} tokenizes character vector using quanteda::tokenize 
#' into 1-grams (words) punctuation, numbers, symbols, twitter handles and URLs.
#'
#' @param text character vector. Text to tokenize.
#' @param verbose logical. Defaults to FALSE
#' @return a character vector with all the tokens (1-grams).
#'   the chaacter vector is the result of calling:
#'   quanteda::tokenize(text, what = "word", ngrams = 1,
#'     removePunct = TRUE, removeNumbers = TRUE, removeSymbols = TRUE,
#'     removeTwitter = TRUE, removeURL = TRUE, 
#'     simplify = TRUE, verbose = verbose)
#' @examples
#'  my_tokenize_words("This is a line of text", verbose = TRUE)
#'  
my_tokenize_words <- function(text, verbose = FALSE) {
  flog.trace("start: my_tokenize_words")
  
  tok <- quanteda::tokenize(text, what = "word", ngrams = 1,
                            removePunct = TRUE, removeNumbers = TRUE, removeSymbols = TRUE,
                            removeTwitter = TRUE, removeURL = TRUE, 
                            simplify = TRUE, verbose = verbose)

  flog.trace("end: my_tokenize_words")
  tok
}

#' escapes special characters in regex.
#'
#' @param str regex to process.
#' @return A regex with the special characters escaped.
#' @examples
#'   quotemeta("i'm")
#'   quotemeta(c("i'm", "you're", "(abc)"))
quotemeta <- function(str) str_replace_all(str, "(\\W)", "\\\\\\1")

#' cleans text 
#' 
#' \code{clean_text} cleans the text by removing underscores, 
#'   twitter URLs, twitter handles, URLs, bad words 
#'   \url{https://gist.github.com/jamiew/1112488}. optionally
#'   can convert to lowercase.
#'
#' @param text text to clean.
#' @param to_lower convert \code{text} to lowercase? defaults to FALSE.
#' @param tokenize_string tokenize text into 1-grams? defaults to FALSE.
#' @return the cleaned text
#' @examples
#'   clean_text("Click https://www.google.com to search a topic.", to_lower = TRUE)
#'   clean_text("Click https://www.google.com to search a topic.", tokenize_string = TRUE)
#'   
clean_text <- function(text, to_lower = FALSE, tokenize_string = FALSE) {
  flog.trace("start: clean_text")
  
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
  
  flog.trace("end: clean_text")
  s
}

#' clean documents text
#' 
#' \code{clean_documents} cleans the text documents loaded by 
#'   \code{load_documents} by separating into sentences, 
#'   converting to lowercase, removing punctuation, and finally
#'   calling \code{clean_text} to clean the text (removing underscores, 
#'   twitter URLs, twitter handles, URLs, bad words 
#'   \url{https://gist.github.com/jamiew/1112488}). 
#'
#' @param documents documents to clean.
#' @return the cleaned documents text
#' @examples
#'   doc <- load_documents("data/all.train-001.txt")
#'   clean_documents(doc)
#'   
clean_documents <- function(documents) {
  flog.trace("start: clean_documents")
  flog.debug("cleaning documents")

  clean_documents <- (documents[[1]]) %>%
    paste(collapse = "\n") %>%
    tokenize_sentences(lowercase = TRUE, strip_punctuation = FALSE, simplify = TRUE) %>%
    clean_text()
  
  flog.trace("end: clean_documents")
  clean_documents
}

# ---------------------------------------------------------------------
# model cache functions
# ---------------------------------------------------------------------

#' loads a model from an rds file.
#' 
#' \code{load_model_from_cache} reads an rds file containing the
#'   a model that was previously calculated and saved. After the
#'   model is loaded, the data.table key and indices are created
#'   (if needed) to optimize model query.
#'
#' @param model_fname model filename (without path) to load
#' @return the model
#' @examples
#'   m <- load_model_from_cache("model-5-0.001-f.cache")
#'   
load_model_from_cache <- function(model_fname) {
  flog.trace("start: load_model_from_cache")

  filename <- paste0(PATH_CACHE, model_fname)
  flog.debug(paste('loading model from cache', filename))
  
  if (file.exists(filename)) {
    model <- read_rds(filename)
  } else {
    flog.error(paste("model", filename, "does't exist"))
    model <- NULL
  }
  
  # assure all keys and indices are set
  model1 <- model[[1]]
  if (!("word" %in% key(model1))) setkey(model1, word)
  if (!("id_word" %in% indices(model1))) setindex(model1, id_word)
  
  model2 <- model[[2]]
  if (!("hash_root" %in% key(model2))) setkey(model2, hash_root)
  if (!("hash_root__n_gram" %in% indices(model2))) setindex(model2, hash_root, n_gram)
  
  flog.debug(paste0('  model size = ', format(object.size(model), units = "Mb")))
  
  flog.trace("end: load_model_from_cache")
  model
}

#' saves a model to an rds file.
#' 
#' \code{save_model_to_cache} writes an rds file containing the
#'   the model to the cache folder
#'
#' @param model model to save
#' @param model_fname model filename (without path) to create
#' @return nothing
#' @examples
#'   save_model_to_cache(model, "model-5-0.001-f.cache")
#'   
save_model_to_cache <- function(model, model_fname) {
  flog.trace("start: save_model_to_cache")

  filename <- paste0(PATH_CACHE, model_fname)
  flog.debug(paste('saving model to cache', filename))

  write_rds(model, filename)
  
  flog.trace("end: save_model_to_cache")
}

#' saves a model as two text files.
#' 
#' \code{save_model_to_cache_as_text_files} writes two text files to
#'   the cache folder containing the the model. Files are appended
#'   "-1" and "-2" respectively to their filenames.
#'
#' @param model model to save
#' @param model_fname model filename (without path) to use as
#'   base filename. Filenames are appended "-1" and "-2" 
#'   respectively to their filenames.
#' @return nothing
#' @examples
#'   save_model_to_cache_as_text_files(model, "model-5-0.001-f.cache")
#'   
save_model_to_cache_as_text_files <- function(model, model_fname) {
  flog.trace("start: save_model_to_cache_as_text_files")
  
  filename <- paste0(PATH_CACHE, model_fname)
  flog.debug(paste0("saving model to cache as two text files ", filename, "[-1|-2]"))
  
  write_csv(model[[1]], paste0(filename, "-1"))
  write_csv(model[[2]], paste0(filename, "-2"))

  flog.trace("end: save_model_to_cache_as_text_files")
}

#' loads a model from two text files.
#' 
#' \code{load_model_from_cache_from_text_files} reads two text files
#'   containing a model that was previously calculated and saved via
#'   \code{save_model_to_cache_as_text_files}. After the
#'   model is loaded, the data.table key and indices are created
#'   (if needed) to optimize model query.
#'
#' @param model_fname model filename (without path) to load
#' @return the model
#' @examples
#'   m <- load_model_from_cache("model-5-0.001-f.cache")
#'   
load_model_from_cache_from_text_files <- function(model_fname) {
  flog.trace("start: load_model_from_cache_from_text_files")
  
  filename <- paste0(PATH_CACHE, model_fname)
  filename1 <- paste0(filename, "-1")
  filename2 <- paste0(filename, "-2")

  flog.debug(paste0("loading model from cache from text files ", filename, "[-1|-2]"))
  
  if (file.exists(filename1) && file.exists(filename2)) {
    model1 <- data.table(read_csv(filename1))
    model2 <- data.table(read_csv(filename2))
    model <- list(model1 = model1, model2 = model2)
  } else {
    flog.error(paste("One or both filenames", filename1, "or", filename2, "does't exist"))
    return(NULL)
  }
  
  # assure all keys and indices are set
  model1 <- model[[1]]
  if (!("word" %in% key(model1))) setkey(model1, word)
  if (!("id_word" %in% indices(model1))) setindex(model1, id_word)
  
  model2 <- model[[2]]
  if (!("hash_root" %in% key(model2))) setkey(model2, hash_root)
  if (!("hash_root__n_gram" %in% indices(model2))) setindex(model2, hash_root, n_gram)
  
  flog.debug(paste0('  model size = ', format(object.size(model), units = "Mb")))
  
  flog.trace("end: load_model_from_cache_from_text_files")
  model
}

# ---------------------------------------------------------------------
# calculate model
# ---------------------------------------------------------------------

#' calculates the model.
#' 
#' \code{calc_model} calculates the model data structures from the
#'   text of clean documents. The model is actually four n-gram models: 
#'   [1..4]-gram models. 
#'   
#' models are stored as data.tables to optimize 
#'   their processing. For each [2..4]-grams, each n-gram is separated 
#'   into the "root" and the last word.
#'   
#' the words are represented as integers and the roots as hashed values. 
#'   this reduces the memory required to store the model in memory. 
#'
#' @param clean_documents documents previously loaded and cleaned
#' @param n_prune number of words to retain for each "root".
#' @param do_mem_size_calc calculate and log the size of the model 
#'   objects? only log in INFO or lower logging levels.
#' @param debug_model_calc leave extra variables in the model for
#'   debugging (increases the memory needed to store the model)
#'   
#' @return the model
#' 
#' @examples
#'   doc <- load_documents("data/all.train-001.txt")
#'   clean_doc <- clean_documents(doc)
#'   model <- calc_model(clean_doc)
#'   
calc_model <- function(clean_documents, 
                       n_prune = 10,
                       do_mem_size_calc = FALSE,
                       debug_model_calc = FALSE) {
  
  flog.trace("start: calc_model")
  flog.info("computing model")
  
  # ---------------------------------------------------------------------
  # unigram model:
  #
  # this model will produce a data.table with (word, count, probability and
  # id_word) from the corpus. the data.table will use word as the key
  # and have an index on id_word
  # ---------------------------------------------------------------------
  flog.info("  unigram model")
  
  flog.debug('    tokenize')
  word_tokens <- my_tokenize_words(clean_documents)

  flog.debug('    create data.table')
  # prepare dt1 with all unique words, their counts and id's
  dt.1 <- data.table(word = as_vector(word_tokens))
  
  rm(word_tokens)
  gc()
  
  flog.debug('    counting words and calc probabilities')
  dt.1 <- dt.1[, list(n = .N), by = word]           # word frequency 
  dt.1[, prob := n / sum(n)]                        # word probability
  dt.1[, id_word := as.integer(seq(1, nrow(dt.1)))] # id for each word
  
  setkey(dt.1, word)                                # clustered index on word
  setindex(dt.1, id_word)                           # index in id_word
  
  # ---------------------------------------------------------------------
  # n-gram models (n = 2:4)
  #
  # this model will produce a data.table of bigrams, trigrams and 4-grams
  # with (hash_root, id_word, count, prob) from the corpus. root will be 
  # n-gram without the last word.
  #
  # hashing is done via hash.value from FeatureHashing
  # ---------------------------------------------------------------------
  flog.info('  n-gram models (n=2:4)')
  
  # flog.debug('    tokenize sentences')
  # break the text by sentences
  sentence <- clean_documents                       # we now have sentences here!

  flog.debug('    tokenize n-grams')
  
  # create ngrams from each sentence, n=2:4
  ngram_tokens <- 
    quanteda::tokenize(sentence, what = "word", ngrams = 2:4,
                       removePunct = TRUE, removeNumbers = TRUE, removeSymbols = TRUE,
                       removeTwitter = TRUE, removeURL = TRUE, 
                       simplify = TRUE, verbose = FALSE)
  
  flog.debug('    create data.table')
  # create data.table for fast processing
  dt.n <- data.table(ngram_tok = ngram_tokens)      # get all ngrams, n=2:4
  
  rm(ngram_tokens)
  gc()
  
  flog.debug('    calculating n-grams frequencies')
  dt.n <- dt.n[, list(n = .N), by = ngram_tok]      # ngram frequency
  
  flog.debug('    splitting n-grams into root and word')
  dt.n[, ngram := str_split(ngram_tok, "_")]        # separate ngram into root and word
  dt.n[, root := sapply(ngram, function(x) {paste(x[1:(length(x) - 1)], collapse = " ")})]
  dt.n[, word := sapply(ngram, function(x) {x[length(x)]})]
  
  dt.n[, n_gram := sapply(ngram, length)]           # calculate 'n' from 'n'-gram
  
  flog.debug('    hashing root')
  dt.n[, hash_root := hashed.value(root)]           # hash the root
  
  flog.debug('    adding id_word')
  setindex(dt.n, word)
  dt.n[dt.1, id_word := i.id_word, on = "word"]     # get id's for words
  dt.n[is.na(id_word)]                              # remove words not mapped (ex. in URLs)
  
  flog.debug('    calc probabilities')
  # calculate probabilities for each word/root
  dt.n[, prob := n / sum(n), by = hash_root]        # calc probabilities having seen root
  
  # create compact model
  if (!debug_model_calc) {
    dt.n[, ngram_tok := NULL]                       # drop non-needed variables
    dt.n[, ngram := NULL]
    dt.n[, root := NULL]
    dt.n[, word := NULL]
  }
  
  # model 4.2: finally prune the model: keep top n_prune words for each root
  flog.debug('    prune')
  setindex(dt.n, hash_root)
  
  # based on http://stackoverflow.com/questions/34753050/data-table-select-first-n-rows-within-group
  v1 <- dt.n[order(-prob), .I[1:n_prune], by = hash_root]$V1
  dt.n <- dt.n[na.omit(v1)]
  rm(v1)
  
  # old code (slow):
  # dt.n <- dt.n[order(-prob), head(.SD, n_prune), by = hash_root]
  
  # setindex(dt.n, NULL)
  setkey(dt.n, hash_root)
  setindex(dt.n, hash_root, n_gram)                 # final indices
  
  if (do_mem_size_calc) {
    # print memory size of each model
    flog.info(paste('size of unigram model =', format(object.size(dt.1), units = "Mb")))
    flog.info(paste('size of n-gram models =', format(object.size(dt.n), units = "Mb")))
  }
  
  # ---------------------------------------------------------------------
  # now combine all models
  # ---------------------------------------------------------------------
  model <- list(model.1 = dt.1, model.n = dt.n)
  
  flog.trace("end: calc_model")
  model
}

# ---------------------------------------------------------------------
# accuracy calculations
# ---------------------------------------------------------------------

#' calculates the model accuracy.
#' 
#' \code{model_accuracy} calculates the model accuract by loadi
#'   testing document and predicting all words starting from the first
#'   3-gram until the last of each sentence.
#'
#' @param model model to evaluate
#' @param test_fname filename (including path) of the test file to 
#'   evaluate the model
#' @param discount_factor stupid katz backoff discount factor. 
#'   defaults to 0.5
#' @param use_unigram use unigrams in the model evalation? 
#'   defaults to FALSE.
#' @param max_model_level starting "n" in n-gram to start preducting
#'   each word. default if 4 to start at 4-gram level.
#' @param n_lines_to_process number of lines from test_fname to
#'   use for the accuracy calculation. default if -1 to process
#'   all lines from the file.
#'   
#' @return a list with
#'   - n_words_to_predict number or words predicted
#'   - n_correct_predictions number of correct predictions (word is in 
#'       top 5 predicted words)
#'   - n_incorrect_predictions number of incorrect predictions (word 
#'       wasn't in top 5 predicted words)
#'   - n_null_predictions number of NULL predictions (prediction
#'       algorithm can't predict). this only happenes if \code{use_unigram}
#'       if FALSE and no other n-gram root is found
#'   - accuracy accuracy of the model calculated as 
#'       \code{(n_correct_predictions / n_words_to_predict)}
#' 
#' @examples
#'   accuracy <- 
#'     load_documents("data/all.train-001.txt") %>%
#'     clean_documents() %>%
#'     calc_model() %>%
#'     model_accuracy()
#'   
model_accuracy <- function(model, 
                           test_fname, 
                           discount_factor = 0.5, 
                           use_unigram = FALSE, 
                           max_model_level = 4,
                           n_lines_to_process = -1) {
  
  flog.trace("start: model_accuracy")
  
  # load test file and clean
  clean_text <- load_documents(test_fname) %>% 
    clean_documents()
  if (n_lines_to_process > 0) {
    flog.debug(paste("only calculating accuracy over", n_lines_to_process, "lines"))
    clean_text <- clean_text[1:n_lines_to_process]
  }
  
  n_words_to_predict <- 0
  n_correct_predictions <- 0
  n_incorrect_predictions <- 0
  n_null_predictions <- 0
  n_line <- 0
  
  flog.info(paste("predicting over", test_fname))
  for (clean_text_line in clean_text) {
    if (n_line %% 100 == 0) { flog.info(paste0("  processing lines ", (n_line+1), ":", (n_line+100))) }
    #if (n_line %% 10 == 0) { flog.debug(paste0("             line # ", n_line)) }
    n_line <- n_line + 1
    
    #flog.debug(paste("New line:" , clean_text_line))
    
    clean_text_split <- my_tokenize_words(clean_text_line, verbose = FALSE)
    m <- length(clean_text_split)
    
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
          
        predicted_words <- predict_words(model, 
                                         text_to_predict, 
                                         discount_factor = discount_factor, 
                                         use_unigram = use_unigram,
                                         max_model_level = max_model_level)
        
        if (!is.null(predicted_words)) {
          if (nrow(predicted_words[word == expected_word]) != 0) {
            n_correct_predictions <- n_correct_predictions + 1
          } else {
            n_incorrect_predictions <- n_incorrect_predictions + 1
          }
        } else {
          n_null_predictions <- n_null_predictions + 1
        }
      }
    }
  }
  flog.debug("end loop")
  
  acc <- list(n_words_to_predict = n_words_to_predict,
              n_correct_predictions = n_correct_predictions,
              n_incorrect_predictions = n_incorrect_predictions,
              n_null_predictions = n_null_predictions, 
              accuracy = (n_correct_predictions / n_words_to_predict))
         
  log_accuracy("accuracy:", acc)        

  flog.trace("end: model_accuracy")
  acc               
}

# helper function to log accuracy results
log_accuracy <- function(msg, acc) {
  flog.info(msg)        
  flog.info(paste("  n_words_to_predict =", acc$n_words_to_predict))
  flog.info(paste("  n_correct_predictions =", acc$n_correct_predictions))
  flog.info(paste("  n_incorrect_predictions =", acc$n_incorrect_predictions))
  flog.info(paste("  n_null_predictions =", acc$n_null_predictions))
  flog.info(paste("  accuracy =", round(acc$accuracy, 4)))
}

# helper function to reduce two accuracy lists
reduce_accuracy <- function(acc1, acc2) {
  acc <- list(n_words_to_predict = acc1$n_words_to_predict + acc2$n_words_to_predict,
              n_correct_predictions = acc1$n_correct_predictions + acc2$n_correct_predictions,
              n_incorrect_predictions = acc1$n_incorrect_predictions + acc2$n_incorrect_predictions,
              n_null_predictions = acc1$n_null_predictions + acc2$n_null_predictions, 
              accuracy = (acc1$n_correct_predictions + acc2$n_correct_predictions) / 
                (acc1$n_words_to_predict + acc2$n_words_to_predict))
  acc
}

# ---------------------------------------------------------------------
# combine models
# ---------------------------------------------------------------------

#' calculates the model accuracy.
#' 
#' \code{combine_models} combines two models together. this function is 
#'   used to incrementally build a larger model by building smaller 
#'   models first and then combining. see combine_models_*.R.
#'
#' @param model1 first model to combine
#' @param model2 second model to combine
#' @param n_prune number of words to retain for each "root". 
#'   defaults to 5
#'   
#' @return a combined model
#' 
#' @examples
#'   model1 <- load_model_from_cache("model-4-4.001-a.cache")
#'   model2 <- load_model_from_cache("model-4-4.001-b.cache")
#'   model <- combine_models(model1, model2)
#'   
combine_models <- function(model1, model2, n_prune = 5) {
  flog.trace("start: combine_models")
  flog.info("combining models")
  
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
  
  flog.info("  combining unigram models")
  dt.1 <- rbind(model.1a, model.1b)
  
  # re-calculate count and probabilities
  dt.1 <- unique(dt.1[, list(n = sum(n), prob = 0, id_word), by = word])  # new word count 
  dt.1[, prob := n / sum(n)]                                  # new word probability

  setindex(dt.1, word)                                        # set indices
  setindex(dt.1, id_word)
  
  flog.debug(paste0('    size(model.1a) = ', format(object.size(model.1a), units = "Mb")))
  flog.debug(paste0('    size(model.1b) = ', format(object.size(model.1b), units = "Mb")))  
  flog.debug(paste0('    size(dt.1)     = ', format(object.size(dt.1), units = "Mb")))  
  
  # --- combine n-gram models ---------------

  flog.info("  combining n-gram models")
  dt.n <- rbind(model.na, model.nb)

  flog.debug('    re-calc count of n-grams')
  dt.n.tmp <- unique(dt.n[, list(n = sum(n), n_gram, prob = 0), by = c("hash_root", "id_word")])    # ngram count
  setcolorder(dt.n.tmp, c("hash_root", "n", "n_gram", "id_word", "prob"))
  dt.n <- dt.n.tmp
  
  setindex(dt.n, hash_root)
  
  flog.debug("    calc probabilities")
  dt.n[, prob := n / sum(n), by = hash_root]                  # calc probabilities x root
  
  # model 4.2: prune the model: keep top n_prune words for each root
  flog.debug("    prune")
  
  # based on http://stackoverflow.com/questions/34753050/data-table-select-first-n-rows-within-group
  v1 <- dt.n[order(-prob), .I[1:n_prune], by = hash_root]$V1
  dt.n <- dt.n[na.omit(v1)]
  rm(v1)
  
  # setindex(dt.n, NULL)
  setindex(dt.n, hash_root, n_gram)                           # final indices
  
  flog.debug(paste0("   size(model.na) = ", format(object.size(model.na), units = "Mb")))
  flog.debug(paste0("   size(model.nb) = ", format(object.size(model.nb), units = "Mb")))
  flog.debug(paste0("   size(dt.n)     = ", format(object.size(dt.n), units = "Mb")))
  
  # combine the new (combined) model
  model <- list(model.1 = dt.1, model.n = dt.n)
  
  flog.trace("end: combine_models")
  model
}

# ---------------------------------------------------------------------
# prediction function
# ---------------------------------------------------------------------

#' preduct next word
#' 
#' \code{predict_words} combines two models together. this function is 
#'   used to incrementally build a larger model by building smaller 
#'   models first and then combining. see combine_models_*.R.
#'
#' @param model model to use in prediction
#' @param s tri-gram to use for prediction. this text should have already
#'   been cleaned.
#' @param discount_factor stupid katz backoff discount factor. 
#'   defaults to 0.5
#' @param use_unigram use unigrams in the model evalation? 
#'   defaults to FALSE.
#' @param max_model_level starting "n" in n-gram to start preducting
#'   each word. default if 4 to start at 4-gram level.
#'   
#' @return a data.table with the predicted words
#' 
#' @examples
#'   words <- predict_words(model, "this is my"
#'   
predict_words <- function(model, 
                          s, 
                          discount_factor = 0.5, 
                          use_unigram_model = FALSE, 
                          max_model_level = 4) {
  
  predict_words_recursive <- function(level, s, prev_id_words, min_prev_prob, alpha) {
    dt_words <- data.table(id_word = integer(), prob = double())
    
    if (level == 1) {
      if (use_unigram_model) {
        # need special case to avoid pocessing all the unigram model
        dt_model <- model[[1]]
        
        # filer out all words that would not help given their probability and have not been seen yet
        dt_words <- 
          dt_model[n > ((min_prev_prob * sum(dt_model$n) / alpha) + discount_factor) & 
                     !(id_word %in% prev_id_words), 
                   .(id_word, 
                     prob = alpha * (n - discount_factor) / sum(n))
                   ]
      }
    } else {
      #cat(paste('prediction for = ', s, ' [level', level, ']\n'))
      
      # levels 2, 3, ...
      dt_model <- model[[2]]
      
      # get all predictions at this level
      hash_s <- hashed.value(s)
      dt_words <- dt_model[hash_root == hash_s][ 
                             !(id_word %in% prev_id_words) & 
                             n_gram == level &
                             n > ((min_prev_prob * sum(dt_model$n) / alpha) + discount_factor), 
                           .(id_word, 
                             prob = alpha * (n - discount_factor) / sum(n))
                           ]
      
      #print(dt_words)
      
      if (nrow(dt_words) > 0) {
        alpha <- alpha * (1 - sum(dt_words$prob))
        prev_id_words = c(prev_id_words, dt_words$id_word)
        min_prev_prob = min(dt_words$prob)
      } 
      
      # go down one level
      s <- sub(s, pattern = "^[[:alpha:]]*( |$)", replacement = "")  # remove first word
      dt <- predict_words_recursive(level - 1, 
                                   s, 
                                   prev_id_words = prev_id_words, 
                                   min_prev_prob = min_prev_prob, 
                                   alpha = alpha)
      
      dt_words <- rbind(dt_words, dt)
    }
    
    dt_words
  }
  
  flog.trace("start: predict_words")
  flog.trace(paste0("predicting next word for '", s, "'"))
  
  # do predictions starting with max level n-grams
  dt_words <- predict_words_recursive(max_model_level, 
                                     s, 
                                     prev_id_words = c(), 
                                     min_prev_prob = 0, 
                                     alpha = 1) [order(-prob)] [,head(.SD, 10)]
  
  
  dt_words <- dt_words[model[[1]], word := i.word, on = "id_word"][, .(word, prob)]   
  if (nrow(dt_words) > 0) {
    setkey(dt_words, word)
  }
  
  flog.trace(paste0("  predicted words are ", paste(dt_words, collapse = ", ")))
  
  flog.trace("end: predict_words")
  dt_words[order(-prob)]
}

pretty_fmt_prediction <- function(dt_words) {
  if (nrow(dt_words) == 0) {
    str <- ""
  } else {
    str <- summarise(dt_words, word = paste(paste0(word, '(', round(prob, 5), '%)'), collapse = ', '))
    str <- str_replace(str, ', $', '')
  }
  str
}
