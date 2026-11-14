##############
#Checks of the arguments of the package's functions
##############

# Shared by the methods: that arguments are numbers, positive where they must be, recycled to
# a common length, and the result given back the shape of its arguments.

# Stops with a clear error when an argument is not numeric. Arguments that are all NA (of
# any type) pass, so that they give NA results like the base R distribution functions. The
# error names the calling function.
check_numeric <- function(...) {
  args <- list(...)
  for (name in names(args)) {
    value <- args[[name]]
    if (!is.numeric(value) && !all(is.na(value))) {
      stop(simpleError(paste(name, "must be numeric"), sys.call(-1)))
    }
  }
  invisible(NULL)
}

# Stops with a clear error when an argument is not numeric, or has a value that is not NA
# and not positive (negative, with allow_zero = TRUE). NA values pass, so that they give NA
# results like the base R distribution functions. The error names the calling function.
check_positive <- function(..., allow_zero = FALSE) {
  args <- list(...)
  for (name in names(args)) {
    value <- args[[name]]
    # without this, text reaches arithmetic ("non-numeric argument to binary operator") or
    # is compared as text
    if (!is.numeric(value) && !all(is.na(value))) {
      stop(simpleError(paste(name, "must be numeric"), sys.call(-1)))
    }
    invalid <- if (allow_zero) value < 0 else value <= 0
    if (any(invalid, na.rm = TRUE)) {
      stop(simpleError(paste(name, if (allow_zero) "must be non-negative" else "must be positive"), sys.call(-1)))
    }
  }
  invisible(NULL)
}

# Recycles every argument to a common length, like the base R distribution functions: the
# longest length, or 0 when any argument has length 0. Lengths that do not recycle are an
# error, which names the calling function (or .call, when given). The first argument of the
# common length is kept, so that restore_shape() can give the result its names or dimensions.
recycle_arguments <- function(..., .call = sys.call(-1)) {
  args <- list(...)
  argLengths <- lengths(args)
  n <- if (any(argLengths == 0)) 0L else max(argLengths)
  if (n > 0 && any(n %% argLengths != 0)) {
    stop(simpleError("the arguments' lengths are not multiples of each other, so they cannot be recycled", .call))
  }
  # the input checks let non-numeric arguments through only when they are all NA (e.g.
  # NA_character_); as numbers they give NA rather than an error in the arithmetic
  recycled <- lapply(args, function(arg) rep_len(if (is.numeric(arg)) as.vector(arg) else as.numeric(as.vector(arg)), n))
  attr(recycled, "template") <- args[[which(argLengths == n)[1]]]
  recycled
}

# Returns a numeric result (numeric(0) rather than ifelse()'s logical(0) for zero-length
# arguments) with the names or dimensions of the argument kept by recycle_arguments().
restore_shape <- function(result, args) {
  template <- attr(args, "template")
  result <- as.numeric(result)
  if (is.null(dim(template))) {
    names(result) <- names(template)
  } else {
    dim(result) <- dim(template)
    dimnames(result) <- dimnames(template)
  }
  result
}
