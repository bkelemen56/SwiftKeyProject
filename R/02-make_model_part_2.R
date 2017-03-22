debugSource('src/calc_models_4_4.R')

model_id    <- "model-4-4"
debug       <- FALSE           # to only test loops, no file processing
do_parallel <- TRUE           # run in parallel (no console output) - only in production
n_prune    <- 5                # number of words to keep for same root
start_on_loop <- 2             # for troubleshooting. valid values 1..7

setDTthreads(7)                # for data.table parallel processing

format_fixed_width <- function(i, width) {
  s <- paste(i)
  while (str_length(s) < width) s <- paste0("0", s)
  s
}

combine_files <- function(i, postfix) {
  j <- i * 2 - 1
  
  in_postfix_str <- ifelse(postfix == 1, "", paste0("-", letters[postfix - 1]))
  in_file1 <- paste0(model_id, ".", format_fixed_width(j, 3), in_postfix_str, ".cache")
  in_file2 <- paste0(model_id, ".", format_fixed_width(j + 1, 3), in_postfix_str, ".cache")
  out_file <- paste0(model_id, ".", format_fixed_width(i, 3), "-", letters[postfix], ".cache")
  
  cat(paste("combining model files ", in_file1, " and ", in_file2, " into ", out_file, "\n"))
  
  if (!debug) {
    model1 <- load_from_cache(in_file1)
    model2 <- load_from_cache(in_file2)
    
    model <- combine_models(model1, model2, n_prune = n_prune)
    save_to_cache(model, out_file)
  }
  
  NULL
}

copy_file <- function(i, postfix) {
  j <- i * 2 - 1
  
  in_postfix_str <- ifelse(postfix == 1, "", paste0("-", letters[postfix - 1]))
  in_file <- paste0("cache/", model_id, ".", format_fixed_width(j, 3), in_postfix_str, ".cache")
  out_file <- paste0("cache/", model_id, ".", format_fixed_width(i, 3), "-", letters[postfix], ".cache")
  
  cat(paste("copying model file ", in_file, " to ", out_file, "\n"))
  
  if (!debug) {
    file.copy(in_file, out_file, overwrite = TRUE)
  }
  
  NULL
}


# for data.table parallel processing
#setDTthreads(7)

if (do_parallel) {
  cat("computing in parallel... (no console output)\n")
  library(doParallel)
  
  no_cores <- detectCores() - 1  
  cl <- makeCluster(no_cores, type = "FORK")  
  registerDoParallel(cl) # ?no_cores)  
}

n <- 80 %/% 2^(start_on_loop - 1)
postfix <- start_on_loop            # counter for "a", "b", ...
loops <- c(start_on_loop:7)         # hard coded for 80 sub-files :-(

for (loop in loops) {
  cat(paste("loop #", loop, "\n"))
  even <- (n %/% 2 * 2 == n)

  next_n <- (n + 1) %/% 2
  n <- n %/% 2
  
  if (do_parallel) {
    foreach(i = 1:n) %dopar% combine_files(i, postfix)
  } else {
    for (i in c(1:n)) combine_files(i, postfix)
  }
  
  if (!even) copy_file(n + 1, postfix)
  
  n <- next_n
  postfix <- postfix + 1

  cat("\n")
}

if (do_parallel) {
  stopCluster(cl)
}

cat("\nend prgram\n")

