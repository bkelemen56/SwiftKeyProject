#----------------------------------------------------------------------------
#
# General utility functions
#
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
#
# Generic exception messages for tryCatch() 
#
#----------------------------------------------------------------------------

exception_message <- function(type, e) {
  message(paste0(type, ":\n", e))
  return(NA)
}

warning_message <- function(e) {
  return(exception_message("WARNING", e))
}

error_message <- function(e) {
  return(exception_message("ERROR", e))
  # TODO: stop program?
}

#----------------------------------------------------------------------------
#
# Generic formatting functions
#
#----------------------------------------------------------------------------

# If 'x' is an integer, returns 'x' as character. Otherwise formats 'x' with
# 'k' decimals (rounded) and returns as character
fmt_int_or_decimal <- function(x, k) {
  if ((x - trunc(x)) == 0) {
    return (as.character(x))
  } else {
    return (format(round(x, k), nsmall=k))
  }
}
