# ---------------------------------------------------------------------
# model #5.0 - final model
# interactive prediction of next word on console
#
# the interactive process reads a sentince and predicts each word.
# a list of words predicted are displayed.
# ---------------------------------------------------------------------

source('R/globals.R')
source('R/model_util.R')

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

# parameters to predict word
use_unigram_model <- TRUE
discount_factor <- 0.5
max_model_level <- 4       # start with this level for backoff

# ---------------------------------------------------------------------
# functions
# ---------------------------------------------------------------------

#' predicts the next word for all 3-grams in a sentence
#' 
#' \code{predict_sentence} accepts a sentence and will iterate over
#'   all 3-grams predicting the next word and comparing to the
#'   actual next word. will also calculate the accuracy of the 
#'   predictions.
#'
#' @param model model to use
#' @param s sentence to use for predictions.
#' @return string with the results to display on console 
#' @examples
#'   \code{predict_sentence(model, "whay shoud I")} will 
#'   predict the next word.
#'   
predict_sentence <- function(model, s, ...) {
  flog.trace("start:predict_sentence")
  flog.debug(paste0("predicting words from sentence '", s, "'"))
  
  s <- char_tolower(s)
  tok <- my_tokenize_words(s)
  
  s <- paste(tok[1], tok[2], tok[3])
  m <- 4
  result <- s
  
  n_words_to_predict <- 0
  n_correct_predictions <- 0
  n_incorrect_predictions <- 0
  n_null_predictions <- 0
  
  while (m <= (length(tok)+1)) {
    flog.debug(paste0("predicting next word from '", s, "'"))
    predicted_words <- predict_words(model, s, ...)

    if (m <= length(tok)) n_words_to_predict <- n_words_to_predict + 1
      
    if (!is.null(predicted_words)) {
      
      if (m > length(tok)) {
        # final word to predict
        next_word <- paste0(pretty_fmt_prediction(predicted_words))
        
      } else if (nrow(predicted_words[word == tok[m]]) != 0) {
        # correct prediction
        n_correct_predictions <- n_correct_predictions + 1
        next_word <- paste0(tok[m], ' == ', pretty_fmt_prediction(predicted_words))
        
      } else {
        # incorrect prediction
        n_incorrect_predictions <- n_incorrect_predictions + 1
        next_word <- paste0(tok[m], ' != ', pretty_fmt_prediction(predicted_words))
      }
      
    } else {
      # can't prediction (no word found)
      n_null_predictions <- n_null_predictions + 1
      next_word <- paste0(tok[m], " : no prediction")
    }
    
    flog.debug(paste0("  prediction: ", next_word))
    
    result <- paste(result, "\n# ", next_word)
    
    s <- paste(tok[m-2], tok[m-1], tok[m])
    m <- m+1 
  }
  
  acc <- n_correct_predictions / n_words_to_predict
  flog.debug(paste0("accuracy = ", acc))
  cat(paste0("accuracy = ", acc, "\n"))

  flog.trace("end:predict_sentence")
  result
}

print_predict <- function(word) {
  if (!is.null(word)) {
    cat('prediction: ', word, '\n')
  } else {
    cat("can't predict - no match")
  }
}

# ---------------------------------------------------------------------
# main
# ---------------------------------------------------------------------

# initialize the logger and start logging...
init_logger(threshold = DEBUG, filename = "interactive_prediction", timestamp = TRUE, tee = FALSE)
flog.info("start: interactive_preddction")
cat('interactive prediction:\n\n')

# load model from cache
postfix <- readline(prompt = "which model to use [a-g] /a/? ")
if (postfix == "") postfix <- "a"
model_fname <- paste0(MODEL_ID, ".001-", postfix, ".cache")
model <- load_model_from_cache(model_fname)

flog.info(paste("using model", model_fname))
cat(paste("using model", model_fname, "\n"))

flog.info(paste0("use_unigram_model = ", use_unigram_model))
flog.info(paste0("discount_factor = ", discount_factor))
flog.info(paste0("max_model_level = ", max_model_level))

# predict interactively
cat('\nprediction:\n')
repeat {
  cat('\n')
  s <- readline(prompt = "enter three or more words: ")
  if (s == '') {
    break
  } else {
    # clean s
    s <- clean_text(s)
    prediction <- predict_sentence(model, 
                                   s, 
                                   use_unigram_model = use_unigram_model, 
                                   discount_factor = discount_factor, 
                                   max_model_level = max_model_level)
    print_predict(prediction)
  }
}

flog.info("end: interactive_preddction")
