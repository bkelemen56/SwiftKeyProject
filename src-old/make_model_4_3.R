debugSource('src/calc_models_4_3.R')

model_id <- "model-4-3"
use_small_data <- FALSE
use_pct_data <- .01

n_prune <- 10                # number of words to keep for same root
n_lines_part <- 10000        # number of lines to read per batch

pct <- c(.01, .02, .03)
#pct <- c(.01, .02, .03, .04, .05)

# process incrementally the training file in batches
process_models <- function(model_id, use_small_data, use_pct_data, n_prune = 10, n_lines_part = 500,
                           do_mem_size_calc = FALSE,
                           debug_model_calc = FALSE) {
  
  # read whole file and do initial cleaning
  text <- load_documents(use_small_data, use_pct_data) %>% 
    clean_data()
  
  text <- text[[1]]
  
  # iterate over chunks of text
  n <- 1
  model <- NULL
  while (n < length(text)) {
    text_part <- text[n:min(n + n_lines_part, length(text))]
    
    model_part <- paste0(model_id, " [", n, ":", min(n + n_lines_part, length(text)), "]")
    
    model.a <- calc_model(text_part, model_part, use_small_data, use_pct_data, do_mem_size_calc)
    
    if (n == 1) {
      model <- model.a
    } else {
      model <- combine_models(model, model.a)
      model.a <- NULL
    }
    
    n <- n + n_lines_part
  }
  
  # save the final combines model to cache
  save_to_cache(model, model_id_fname_prefix(model_id, use_small_data, use_pct_data))
}

# for data.table parallel processing
setDTthreads(7)

if (use_small_data) {
  model <- process_models(model_id, use_small_data, use_pct_data, 
                          n_prune, n_lines_part,
                          do_mem_size_calc = TRUE)
}


for (use_pct_data in pct) {
  cat(paste("processing pct =", use_pct_data, "\n"))
  cat("********************************\n\n")

  model <- process_models(model_id, use_small_data = FALSE, use_pct_data, 
                          n_prune, n_lines_part,
                          do_mem_size_calc = TRUE)
}
