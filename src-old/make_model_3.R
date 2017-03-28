source('src/calc_models.R')

model_id <- "model-3"

use_small_data <- FALSE
use_pct_data <- .75

load_documents(use_small_data, use_pct_data) %>%
  clean_data() %>%
  calc_and_save_models(model_id, do_mem_size_calc = FALSE)
