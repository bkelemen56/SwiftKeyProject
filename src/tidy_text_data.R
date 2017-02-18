#----------------------------------------------------------------------------
#
# R script to tidy the text data for processing
#
# Input files: data/text_data.rds
# Output file: data/tidy_text_data.rds
#
#----------------------------------------------------------------------------

library(tidyverse)

# only read text_data.rds if not already in memory
if (!exists('text_data')) {
  text_data <- readRDS('data/text_data.rds')
}

# make text data frame with these columns:
# source = blogs|news|twitter
# line = line number from the original file
# text = text lines from file
text_df <-
  seq_along(text_data) %>%
  lapply(
    function(i) {
      n <- length(text_data[[i]])
      list(
        source = rep(names(text_data)[i], n),
        line = 1:n,
        text = text_data[[i]]
      )
    }
  ) %>%
  bind_rows()
