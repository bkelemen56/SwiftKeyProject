debugSource('src/model_4_4_util.R')

model_id    <- "model-4-4"
model_fname <- paste0(model_id, ".001-a.cache")

# use_small_data <- FALSE
debug       <- TRUE           # to only test loops, no file processing
do_parallel <- FALSE           # run in parallel (no console output) - only in production
n_prune     <- 5              # number of words to keep for same root

setDTthreads(7)                # for data.table parallel processing

# format_fixed_width <- function(i, width) {
#   s <- paste(i)
#   while (str_length(s) < width) s <- paste0("0", s)
#   s
# }

calc_accuracy <- function(i, ...) {
  fname <- paste0("data/all.test-", str_pad(i, 3, "left", "0"), ".txt")
  cat(paste("calc accuracy on", fname, "\n"))
  accuracy <- model_accuracy(model, fname, n_prune = n_prune, ...)
  print(accuracy)
  accuracy
}

# load model from cache
model <- load_model_from_cache(model_fname)

if (do_parallel) {
  cat("computing in parallel... (no console output)")
  library(doParallel)
  
  no_cores <- detectCores() - 1  
  cl <- makeCluster(no_cores, type = "FORK")  
  registerDoParallel(cl) # ?no_cores)  

  foreach(i = 1:80) %dopar% calc_accuracy(i)

  stopCluster(cl)
  
} else if (debug) {
  calc_accuracy(1, 
                n_lines_to_process = 100,
                use_unigram = TRUE)
  
} else {
  for (i in c(1:80)) calc_accuracy(i)
}
