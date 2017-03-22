# old code...

nlines0 <- function(file_name) {
  return(
    tryCatch({
      # using the second fasest (first is 'wc') to do it all in R
      n <- 0L
      f <- file(file_name, open = "rb")
      while (length(chunk <- readBin(f, "raw", chuck_size)) > 0) {
        n <- n + sum(chunk == as.raw(10L))
      }
      
      # fastest (see perf_text_linecount.R for test of 4 alternatives)
      # system.time(n <- as.integer(system2(
      #   "wc",
      #   args = c("-l", file_name, " | awk '{print $1}'"),
      #   stdout = TRUE
      # )))
      
      n
    },
    warning = function(e)
      return(warning_message(e)),
    error = function(e)
      return(error_message(e)),
    finally = 
      close(f)
    )
  )
}

