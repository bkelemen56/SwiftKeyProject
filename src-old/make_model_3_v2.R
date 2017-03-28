source('src/calc_models_v2.R')

model_id <- "model-3"

use_small_data <- TRUE
use_pct_data <- .01

text <- load_documents(use_small_data, use_pct_data) 
# %>%
#   clean_data() %>%
#   calc_and_save_models(model_id, do_mem_size_calc = FALSE)




