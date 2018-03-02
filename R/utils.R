# Remove empty list elements
remove_empty <- function(x) {
  x[sapply(x, is.null)] <- NULL
  x
}

# Perform a controlled recursive unlist
unlist_recursive <- function(x, depth = 1) {
  depth <- depth - 1
  if (depth > 0) x <- unlist_recursive(x, depth)
  unlist(x, recursive = FALSE)
}

# Check if arguments supplied in ... are valid (type and length)
check_arg <- function(..., var_type = "character", var_length = 1) {
  check <- sapply(list(...), function(x) all(is.vector(x, var_type), length(x) == var_length))
  if (!all(check)) {
    # retrieve argument names
    bad_arg <- substitute(list(...))[-1]
    bad_arg <- sapply(bad_arg, deparse)
    bad_arg <- paste("\t", bad_arg[which(!check)], "should be a", var_type, "of length", var_length, collapse = "\n")
    stop(paste0("Invalid argument:\n", bad_arg), call. = FALSE)
  }
}