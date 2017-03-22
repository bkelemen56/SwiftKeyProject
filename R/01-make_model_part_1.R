debugSource('src/calc_models_4_4.R')

model_id    <- "model-4-4"
# use_small_data <- FALSE
debug       <- FALSE           # to only test loops, no file processing
do_parallel <- TRUE           # run in parallel (no console output) - only in production
n_prune     <- 4              # number of words to keep for same root

setDTthreads(7)                # for data.table parallel processing

# process incrementally a training (small) file
process_model <- function(model_fname, 
                           n_prune = 10, 
                           do_mem_size_calc = FALSE,
                           debug_model_calc = FALSE) {
  
  text <- load_documents0(model_fname) %>% 
    clean_data()
  
  #text <- text[[1]]
  
  model <- calc_model(text, model_fname, n_prune = n_prune, do_mem_size_calc = do_mem_size_calc)
}

format_fixed_width <- function(i, width) {
  s <- paste(i)
  while (str_length(s) < width) s <- paste0("0", s)
  s
}

process_file <- function(i) {
  cat(paste("processing file #", format_fixed_width(i, 3), "\n"))
  model <- process_model(paste0("data/all.train-", format_fixed_width(i, 3), ".txt"), 
                         n_prune, 
                         do_mem_size_calc = TRUE)
  save_to_cache(model, paste0(model_id, ".", format_fixed_width(i, 3), ".cache"))
  NULL
}

if (do_parallel) {
  cat("computing in parallel... (no console output)")
  library(doParallel)
  
  no_cores <- detectCores() - 1  
  cl <- makeCluster(no_cores, type = "FORK")  
  registerDoParallel(cl) # ?no_cores)  

  foreach(i = 1:80) %dopar% process_file(i)

  stopCluster(cl)
  
} else if (debug) {
  process_file(1)
  
} else {
  for (i in c(1:80)) process_file(i)
}
