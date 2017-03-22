#----------------------------------------------------------------------------
#
# Text file utility functions
#
#----------------------------------------------------------------------------

library(stringr)
library(R.utils)
#library(microbenchmark)

source('util/general_util.R')

# buffer size when reading from files
CHUNCK_SIZE <- 500000

#----------------------------------------------------------------------------
#
# Generic template for a file process.
#
# First opens the file, executes the 'func' and closes the file.
#
#----------------------------------------------------------------------------
file_process <- function(file_name, open_mode = 'r', func) {
  return(tryCatch({
    f <- file(file_name, open = open_mode)
    func(f)
  }, warning = function(e)
    return(warning_message(e)),
  error = function(e)
    return(error_message(e)),
  finally = {
    close(f)
  }))
}

#----------------------------------------------------------------------------
#
# Returns the number of lines in a file
#
#----------------------------------------------------------------------------
nlines <- function(file_name) {
  file_process(file_name, "rb", function(f) {
    n <- 0L
    while (length(chunk <- readBin(f, "raw", CHUNCK_SIZE)) > 0) {
      n <- n + sum(chunk == as.raw(10L))
    }
    n
  })
}

#----------------------------------------------------------------------------
#
# Returns the number of lines in a file via OS cmds:
# wc -l <filename> | awk '{print $1}'
#
#----------------------------------------------------------------------------
nlines_OS <- function(file_name) {
  as.integer(system2(
    "wc",
    args = c("-l", file_name, " | awk '{print $1}'"),
    stdout = TRUE
  ))
}

#----------------------------------------------------------------------------
#
# Returns the max length of all lines in a file 
#
#----------------------------------------------------------------------------
max_len_lines <- function(file_name) {
  file_process(file_name, "r", function(f) {
    max_len <- 0
    while (length(lines <- readLines(f, CHUNCK_SIZE, skipNul = TRUE)) > 0) {
      max_len <- max(max_len, max(str_length(lines)))
    }
    max_len
  })
}

#----------------------------------------------------------------------------
#
# Returns the max length of all lines in a file via OS commands:
# cat <file_name> | awk '{print length}' | sort -nr | head -1
#
#----------------------------------------------------------------------------
max_len_lines_OS <- function(file_name) {
  as.integer(system2(
    "cat",
    args = c(file_name, " | awk '{print length}' | sort -nr | head -1"),
    stdout = TRUE
  ))
}


#----------------------------------------------------------------------------
#
# Loads a file into memory. Caution with large files
#
#----------------------------------------------------------------------------
load_file <- function(file_name) {
  file_process(file_name, "r", function(f) {
    readLines(f, skipNul = TRUE)
  })
}

#----------------------------------------------------------------------------
#
# Loads various files into memory and returns a list of their content. 
# Caution with large files
#
#----------------------------------------------------------------------------
load_files <- function(file_names, path = '') {
  lapply(
    file_names, 
    function(x) {
      file_process(paste0(path, '/', x), "r",function(f) { readLines(f, skipNul = TRUE) })
    }
  )
}

#----------------------------------------------------------------------------

test <- FALSE
if (test) {
  nlines(text_file_twitter)
}
