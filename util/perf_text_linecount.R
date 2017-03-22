#----------------------------------------------------------------------------
#
# Performance test on text line count methods
#
#----------------------------------------------------------------------------

library(R.utils)

file_name <- "data/final/en_US/en_US.twitter.txt"

chuck_size <- 500000
f <- NULL

# slower
print("readLines test:")
print(system.time({
  n <- 0
  f <- file(file_name, open = "r")
  while ((lines_read <-
          length(readLines(f, chuck_size, skipNul = TRUE))) > 0) {
    n <- n + lines_read
  }
  close(f)
}))

# faster
print("countLines test:")
print(system.time(n <- countLines(file_name)))

# much faster
print("readBin test:")
print(system.time({
  n <- 0L
  f <- file(file_name, open = "rb")
  while (length(chunk <- readBin(f, "raw", chuck_size)) > 0) {
    n <- n + sum(chunk == as.raw(10L))
  }
  close(f)
}))

# fastest
print("OS wc test:")
print(system.time(n <- as.integer(system2(
  "wc",
  args = c("-l", file_name, " | awk '{print $1}'"),
  stdout = TRUE
))))

