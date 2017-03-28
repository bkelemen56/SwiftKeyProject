source('src/calc_models_4_1.R')

model_id <- "model-4-1"
use_small_data <- FALSE
use_pct_data <- .01

pct <- c(.05)
#pct <- c(.01, .02, .03, .04, .05)

# for data.table parallel processing
setDTthreads(7)

if (use_small_data) {
  models <- 
    load_documents(use_small_data, use_pct_data) %>% 
    clean_data() %>%
    calc_and_save_models(model_id, use_small_data, use_pct_data, do_mem_size_calc = TRUE)
}


for (use_pct_data in pct) {
  cat(paste("processing pct =", use_pct_data, "\n"))
  cat("********************************\n\n")

  models <- 
    load_documents(use_small_data = FALSE, use_pct_data) %>%
    clean_data() %>%
    calc_and_save_models("model-4", use_small_data = FALSE, use_pct_data, do_mem_size_calc = TRUE)
}
