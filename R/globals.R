# ---------------------------------------------------------------------
# model #5.0 - Final model. 
# global libraries, configurations and variables
#
# ---------------------------------------------------------------------

#library(devtools)
#install_github("zatonovo/futile.logger")
library(futile.logger)

library(tidyverse)
library(stringr)

library(tokenizers)

library(quanteda)
library(readtext)

library(data.table)
library(FeatureHashing)

# ---------------------------------------------------------------------
# global variables
# ---------------------------------------------------------------------

MODEL_ID <- "model-5-0"

PATH_ROOT  <- '~/R Workspace/data science class/10- capstone/pnw/'
PATH_CACHE <- paste0(PATH_ROOT, 'cache/')
PATH_DATA  <- paste0(PATH_ROOT, 'data/')
PATH_LOG   <- paste0(PATH_ROOT, 'logs/')

# ---------------------------------------------------------------------
# configure logging system
# ---------------------------------------------------------------------

# slightly modified version from
# https://github.com/zatonovo/futile.logger/blob/master/R/layout.R
my_layout.simple.parallel <- function(level, msg, ...) {
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS4")
  the.pid  <- Sys.getpid()
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) ifelse(is.null(x), 'NULL', x))
    msg <- do.call(sprintf, c(msg, parsed))
  }
  sprintf("%s | %s | %s | %s\n", str_pad(names(level), 5, side = "right"), the.time, the.pid, msg)
}

# provides additionally setup for the logger
init_logger <- function(threshold = NULL, 
                        filename = NULL, timestamp = FALSE, tee = FALSE) {
  
  if (!is.null(threshold)) {
    flog.threshold(threshold)
  }
  
  if (!is.null(filename)) {
    ts <- if_else(timestamp, format(Sys.time(), "-%Y%m%d-%H%M%S"), "")
    if (tee) {
      flog.appender(appender.tee(paste0(PATH_LOG, filename, ts)))
    } else {
      flog.appender(appender.file(paste0(PATH_LOG, filename, ts)))
    }
  }
}

# create our standard logger
flog.logger("ROOT", threshold = INFO, appender = appender.console, layout = my_layout.simple.parallel, carp = NULL)

