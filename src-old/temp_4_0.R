source('src/calc_models_4_1.R')

use_small_data <- TRUE
use_pct_data <- .1

clean_corpus <- 
  load_documents(use_small_data, use_pct_data) %>%
  clean_data()

# 
models <- calc_and_save_models(clean_corpus, 
                               "model-4",
                               do_mem_size_calc = TRUE)

# get all unigrams (with duplicates)
word_tokens <- quanteda::tokenize(text, what = "word", ngrams = 1,
                                  removePunct = TRUE, removeNumbers = TRUE, removeSymbols = TRUE,
                                  removeTwitter = TRUE, removeURL = TRUE, 
                                  simplify = TRUE, verbose = TRUE)

# prepare dt1 with all unique words, their counts and id's
dt1 <- data.table(word = word_tokens)
dt1 <- dt1[, list(count = .N), by = word]
dt1[, freq := count / sum(count)]
dt1[, id_word := seq(1, nrow(dt1))]
setkey(dt1, word)
setindex(dt1, id_word)

rm(word_tokens)

# break the text by sentences
sentence <- 
  quanteda::tokenize(text, what = "sentence", 
                     removePunct = TRUE, removeSymbols = TRUE, removeNumbers = TRUE, 
                     removeTwitter = TRUE, removeURL = TRUE, 
                     simplify = TRUE, verbose = TRUE)

# create ngrams from each sentence, n=2:4
# TODO: ngrams are spilling from one sentence to another. need to stop at end of the sentence
ngram_tokens <- 
  quanteda::tokenize(sentence, what = "word", ngrams = 2:4,
                     removePunct = TRUE, removeNumbers = TRUE, removeSymbols = TRUE,
                     removeTwitter = TRUE, removeURL = TRUE, 
                     simplify = TRUE, verbose = TRUE)

# create data.table for fast processing
system.time({
  dt2 <- data.table(ngram_tok = ngram_tokens)       # get all ngrams, n=2:4
  
  dt2 <- dt2[, list(n = .N), by = ngram_tok]        # calculate count of each ngram and group by ngram
  
  dt2[, ngram := str_split(ngram_tok, "_")]         # separate words from ngram, then split into root and word
  dt2[, root := sapply(ngram, function(x) {paste(x[1:(length(x) - 1)], collapse = "_")})]
  dt2[, word := sapply(ngram, function(x) {x[length(x)]})]
  
  dt2[, n_gram := sapply(ngram, length)]            # calculate 'n' from 'n'-gram
  
  setindex(dt2, root)
  setindex(dt2, word)
})

# add hash values and word ids
system.time({
  dt2[, hash_root := hashed.value(root)]
  dt2[dt1, id_word := i.id_word, on = "word"]
})

# calculate frequencies for each word/root
system.time({
  dt2[, freq := n / sum(n), by = hash_root]
})


# can we drop column "ngram" ?

# create compact model
dt3 <- copy(dt2)
dt3[, ngram_tok := NULL]
dt3[, ngram := NULL]
dt3[, root := NULL]
dt3[, word := NULL]
dt3[, n := NULL]

setindex(dt3, hash_root, n_gram)






#-----------------------------------
#-- test with FeatureHashing
# library(FeatureHashing)
# 
# object.size(dfm1 <- dfm(text1, ngrams = 1, removePunct = TRUE))
# object.size(hm1 <- hashed.model.matrix(~ word, data = dt_unigram, hash.size = 2^12))
# 
# s1 <- 
#   object.size(dfm2 <- dfm(text1, ngrams = 2, removePunct = TRUE)) +
#   object.size(dfm3 <- dfm(text1, ngrams = 3, removePunct = TRUE)) +
#   object.size(dfm4 <- dfm(text1, ngrams = 4, removePunct = TRUE))
# s1
# 
# # reduces the size well!!! ~26% of the original size
# hash.size(dt2$root)
# 
# hm2 <- hashed.model.matrix(~ root, data = dt2, hash.size = 2^14, create.mapping = TRUE)  # has 2:4-grams
# s2 <- object.size(hm2)
# 
# hm2.2 <- hashed.model.matrix(~ root, data = dt2[N == 2], hash.size = 2^14)  # has 2:4-grams
# hm2.3 <- hashed.model.matrix(~ root, data = dt2[N == 3], hash.size = 2^14)  # has 2:4-grams
# hm2.4 <- hashed.model.matrix(~ root, data = dt2[N == 4], hash.size = 2^14)  # has 2:4-grams
# 
# s2.x <- object.size(hm2.2) + object.size(hm2.3) + object.size(hm2.4)
# 
# 
# # hm2 <- hashed.model.matrix(~ root, data = dt2[N == 4], hash.size = 2^14)
# # object.size(hm2)  # has 2:4-grams
# 
# s2/s1
# 
# 
# 
# 
# 
# 
# 
# 
# # split ngrams into words
# # system.time(dt2[, split1 := str_split(ngram, "_")])
# # 
# # separate_root_word <- function(ngram) {
# #   ss <- str_split(ngram, "_")
# #   root <- paste(ss[1:(length(ss) - 1)], collapse = "_")
# #   word <- ss[length(ss)] 
# #   return(as.character(root = root, word = word))
# # }
# # 
# # system.time(dt2[, c("root","word") := separate_root_word(ngram)])
# 
# 
# # DT[, c("new1","new2") := myfun(y,v), with = FALSE]
# 
# x <- dt2[, paste(split1[1:(length(split1)-1)], collapse = "_")]
# 
# system.time({
#   dt2[, root := paste(split1[1:(length(split1)-1)], collapse = "_") ]
#   dt2[, word := split1[length(split1)]]
# })
#                  
# 
# system.time(dt2[, split2 := sapply(str_split(ngram, "_"), 
#                                   function(x) { c(paste(x[c(1:(length(x)-1))], collapse = "_"), x[length(x)]) })])
# 
# sapply(str_split(s, "_"), function(x) { c(paste(x[c(1:(length(x)-1))], collapse = "_"), x[length(x)]) })
# 
# 
# # or should we encode here the splitted words?
# 
# # unique(dt$n)
# # dt[n == 5]
# 
# 
# 
# 
# 
# 
# 
# ####################################################################################
# # old code
# ####################################################################################
# 
# 
# 
# # get all n-grams, n=1:4
# tn <- quanteda::tokenize(text1, what = "word", ngram = 1:4, 
#                          removePunct = TRUE, 
#                          removeNumbers = TRUE,
#                          removeSymbols = TRUE,
#                          removeTwitter = TRUE,
#                          removeURL = TRUE, 
#                          simplify = TRUE, 
#                          verbose = TRUE)
# 
# # get all 1-grams. these can be used to encode the rest of the text
# t1 <- quanteda::tokenize(text1, what = "word", ngram = 1, 
#                          removePunct = TRUE, 
#                          removeNumbers = TRUE,
#                          removeSymbols = TRUE,
#                          removeTwitter = TRUE,
#                          removeURL = TRUE, 
#                          simplify = TRUE, 
#                          verbose = TRUE)
# 
# t2 <- quanteda::tokenize(text1, what = "word", ngram = 2, 
#                          removePunct = TRUE, 
#                          removeNumbers = TRUE,
#                          removeSymbols = TRUE,
#                          removeTwitter = TRUE,
#                          removeURL = TRUE, 
#                          simplify = TRUE, 
#                          verbose = TRUE)
# 
# t3 <- quanteda::tokenize(text1, what = "word", ngram = 3, 
#                          removePunct = TRUE, 
#                          removeNumbers = TRUE,
#                          removeSymbols = TRUE,
#                          removeTwitter = TRUE,
#                          removeURL = TRUE, 
#                          simplify = TRUE, 
#                          verbose = TRUE)
# 
# t4 <- quanteda::tokenize(text1, what = "word", ngram = 4, 
#                          removePunct = TRUE, 
#                          removeNumbers = TRUE,
#                          removeSymbols = TRUE,
#                          removeTwitter = TRUE,
#                          removeURL = TRUE, 
#                          simplify = TRUE, 
#                          verbose = TRUE)
# 
# t4_split <- str_split(t4, "_")
# 
# 
# s2 <- tokenize(
#   tokenize(text1$text, what = "sentence", tokenize(
#     tokenize(mydat$text, what = "sentence", simplify = TRUE),
#     ngrams = 2,
#     removePunct = TRUE,
#     simplify = TRUE)),
#   ngrams = 2,
#   removePunct = TRUE,
#   simplify = TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# tok <- tokens_ngrams(text1, n = 1)
# g2 <- tokens_ngrams(s, n = 2)
# 
# tok <- lapply(text$text, tokens)
# 
# sentences <- lapply(text$texts, tokenize.character, what= "sentence")
# 
# sent1 <- unlist(sent)
# 
# sent2 <- sapply(sent, map, paste)
#   
# tok_2gram <- lapply(tok, tokens_ngrams, n = 2)
# 
# s1 <- map_chr(tok_2gram, str_split, ' ')
# s2 <- map(s1, c)