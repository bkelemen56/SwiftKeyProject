ts1 <- tokenize_sentences(text_corpus[[1]][1:1], lowercase = TRUE, strip_punctuation = FALSE, simplify = TRUE)

ts2 <- ts1
  tokenize_words(stopwords = c("this", "a", "and")) %>%
  lapply(function(x) paste(x, collapse = " ")) %>%
  as.character()
