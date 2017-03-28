#----------------------------------------------------------------------------
#
# R script to tidy the text data for processing
#
# Input files: data/text_data.rds
# Output file: data/tidy_text_data.rds
#
#----------------------------------------------------------------------------

source('src/globals.R')

library(tidyverse)
library(tidytext)
library(SnowballC)

# clean the data
clean_text <- raw_text
# add here additional cleaning stuff...

# tidy the data
tidy_unigram <-
  clean_text %>%
  unnest_tokens(word, text)

# if we want to stem the words
tidy_unigram_stem <- tidy_unigram %>%
  mutate(word_stem = wordStem(word))

# saveRDS(tidy_unigram, paste0('data/tidy_text_data', ifelse(use_small_data, '_small', ''), '.rds'))

# process 2-grams
tidy_bigram <-
  clean_text %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)
# write_rds(tidy_bigram, paste0('data/tidy_text_data_2gram', ifelse(use_small_data, '_small', ''), '.rds'))

# process 3-grams
tidy_trigram <-
  clean_text %>%
  unnest_tokens(word, text, token = "ngrams", n = 3)
# write_rds(tidy_trigram, paste0('data/tidy_text_data_3gram', ifelse(use_small_data, '_small', ''), '.rds'))



