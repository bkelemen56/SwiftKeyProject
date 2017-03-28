library(tidyverse)
library(tidytext)
library(stringr)

# how frequent are stop words?
td <- tidy_unigram %>%
  count(word, sort = TRUE) %>%
  mutate(freq = n / sum(n), freq_accum = cumsum(freq)) %>%
  left_join(stop_words[stop_words$lexicon == 'snowball', ], by='word')

# freq of stop words in the corpus ~43%
sum(td$freq[td$lexicon == 'snowball'], na.rm = TRUE)
# freg of non-stop words ~57%
sum(td$freq[is.na(td$lexicon)], na.rm = TRUE)

# how many words to cover 50% of language
nrow(td[td$freq_accum <= .5, ])
nrow(td[td$freq_accum <= .9, ])