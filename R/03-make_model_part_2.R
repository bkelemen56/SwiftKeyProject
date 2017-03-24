# ---------------------------------------------------------------------
# model #5.0 - final model
# make model - part 2
#
# script to process the 80 separate models created in part 1. the
# separate models are combined iteratively into the larger and larger
# models until the final one is calculated.
# ---------------------------------------------------------------------

source('R/globals.R')
source('R/bad_words.R')
source('R/model_util.R')

# ---------------------------------------------------------------------
# parameters
# ---------------------------------------------------------------------

debug         <- FALSE         # to only test loops, no file processing
do_parallel   <- TRUE          # run in parallel (no console output) - only in production
n_prune       <- 5             # number of words to keep for same root
start_on_loop <- 1             # for troubleshooting. valid values 1..7

# ---------------------------------------------------------------------
# functions
# ---------------------------------------------------------------------

#' combines two model files into a new model file.
#' 
#' \code{combine_files} loads two pre-calculated model files and 
#'   combines them into one. the new model is saved to cache. each
#'   combine level assignes a new postfix character starting with "a"
#'   for the first combination level. outout files will be called 
#'   <MODEL_ID>-###-[a-g].txt.
#'
#' @param i models numbers (i * 2 - 1) and (i * 2) to be combined
#' @param postfix index into letters[.] to add to the combined model filename
#' @return NONE 
#' @examples
#'   \code{combine_files(2, "a")} will load models \code{model_XXX.003.txt}
#'   and \code{model_XXX.004.txt} from the PATH_CACHE folder and 
#'   create \code{model_XXX.002-a.cache} as the combined model.
#'   
combine_files <- function(i, postfix) {
  j <- i * 2 - 1
  
  in_postfix_str <- ifelse(postfix == 1, "", paste0("-", letters[postfix - 1]))
  in_file1 <- paste0(MODEL_ID, ".", str_pad(j,     3, side = "left", "0"), in_postfix_str,        ".cache")
  in_file2 <- paste0(MODEL_ID, ".", str_pad(j + 1, 3, side = "left", "0"), in_postfix_str,        ".cache")
  out_file <- paste0(MODEL_ID, ".", str_pad(i,     3, side = "left", "0"), "-", letters[postfix], ".cache")
  
  flog.info(paste("combining model files", in_file1, "and", in_file2, "into", out_file))
  
  # only physically combine model files if !debug
  if (!debug) {
    model1 <- load_model_from_cache(in_file1)
    model2 <- load_model_from_cache(in_file2)
    
    model <- combine_models(model1, model2, n_prune = n_prune)
    save_model_to_cache(model, out_file)
  }
  
  invisible()
}

#' copies a model file to the next level.
#' 
#' \code{copy_file} copies one pre-calculated model file into one. 
#'   the new model is saved to cache. this function is only used
#'   when there is an odd number of model files in the current level
#'   and the last model file needs to be copied to the next
#'   level. the new level assignes a new 
#'   postfix character starting with "a" for the first combination level. 
#'   outout files will be called <MODEL_ID>-###-[a-g].txt.
#'
#' @param i models number (i * 2 - 1) to be copied
#' @param postfix index into letters[.] to add to the copied model filename
#' @return NONE 
#' @examples
#'   \code{copy_file(3, "a")} will load model \code{model_XXX.005.txt}
#'   from the PATH_CACHE folder and copy to \code{model_XXX.003-a.cache}.
#'   
copy_file <- function(i, postfix) {
  j <- i * 2 - 1
  
  in_postfix_str <- ifelse(postfix == 1, "", paste0("-", letters[postfix - 1]))
  in_file  <- paste0(DIR_CACHE, MODEL_ID, ".", str_pad(j, 3, side = "left", "0"), in_postfix_str,        ".cache")
  out_file <- paste0(DIR_CACHE, MODEL_ID, ".", str_pad(i, 3, side = "left", "0"), "-", letters[postfix], ".cache")
  
  flog.info(paste("copying model file", in_file, "to", out_file))
  
  # only physically copy the model file if !debug
  if (!debug) {
    file.copy(in_file, out_file, overwrite = TRUE)
  }
  
  invisible()
}

# ---------------------------------------------------------------------
# main
# ---------------------------------------------------------------------

# initialize the logger and start logging...
init_logger(threshold = DEBUG, filename = "make_model_part_2", timestamp = TRUE, tee = TRUE)
flog.info("start: make_model_part_2")

if (do_parallel) {
  flog.info("start: parallel computing")
  library(doParallel)
  
  no_cores <- detectCores() - 1  
  cl <- makeCluster(no_cores, type = "FORK")  
  registerDoParallel(cl) 
  
  flog.info(paste("computing with", no_cores, "cores"))
}

n <- 80 %/% 2^(start_on_loop - 1)   # number of levels to compute
postfix <- start_on_loop            # counter for "a", "b", ...
loops <- c(start_on_loop:7)         # hard coded for 80 sub-files :-(

# loop over each level
for (loop in loops) {
  flog.info("")
  flog.info(paste("combining files in loop #", loop))
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
}

if (do_parallel) {
  stopCluster(cl)
  flog.info("end: parallel computing")
}

flog.info("end: make_model_part_2")

