##############
#Small helpers shared across the package
##############

# A value, or `default` when it is NULL or empty (an input the page has not sent yet).
or_default <- function(x, default) if (is.null(x) || length(x) == 0) default else x

# The value of an expression, or NULL when it stops with an error.
quiet <- function(expr) tryCatch(expr, error = function(e) NULL)

# Whether x is a single number that is not missing.
is_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)

# Sets the seed of the random numbers of the function that calls it, and restores the caller's
# random numbers (their seed and generators) when that function exits, so that a function
# given a seed gives the same results each time without changing the random numbers of its
# caller. `kind` is the generator: the normal and sample kinds are pinned too, so that a seed
# gives the same results whatever the caller's RNGkind(); with kind NULL, set.seed(seed) uses
# the generators in use. With seed NULL the random numbers are only restored.
netsimr_local_seed <- function(seed, kind = "Mersenne-Twister", envir = parent.frame()) {
  global <- globalenv()
  saved <- if (exists(".Random.seed", envir = global, inherits = FALSE)) get(".Random.seed", envir = global)
  saved_kind <- RNGkind()
  restore <- function() {
    #restoring a "Rounding" sample kind warns; the caller chose it, so the warning is not ours
    suppressWarnings(RNGkind(saved_kind[1], saved_kind[2], saved_kind[3]))
    if (is.null(saved)) {
      if (exists(".Random.seed", envir = global, inherits = FALSE)) rm(".Random.seed", envir = global)
    } else {
      assign(".Random.seed", saved, envir = global)
    }
  }
  do.call(base::on.exit, list(as.call(list(restore)), add = TRUE), envir = envir)
  if (is.null(seed)) return(invisible(NULL))
  if (is.null(kind)) {
    set.seed(seed)
  } else {
    set.seed(seed, kind = kind, normal.kind = "Inversion", sample.kind = "Rejection")
  }
  invisible(NULL)
}
