##############
#Running simulations in parallel
##############

# The workers of a parallel run, when a run is worth splitting across them, and the chunks of
# simulations each runs with its own random number stream.

#' The number of workers of a parallel run
#'
#' One per available core: the \code{mc.cores} option when it is set, otherwise the cores
#' that \code{parallel::detectCores()} finds, at most 2 when \code{R CMD check} limits the
#' cores (as CRAN does).
#' @return A whole number, at least 1.
#' @noRd
netsimr_available_cores <- function() {
  option <- getOption("mc.cores")
  if (is.numeric(option) && length(option) == 1 && is.finite(option) && option >= 1) return(as.integer(option))
  cores <- suppressWarnings(parallel::detectCores())
  if (is.na(cores) || cores < 1) cores <- 1L
  limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(limit) && !identical(tolower(limit), "false")) cores <- min(cores, 2L)
  as.integer(cores)
}

#' Start worker R processes for a parallel run
#'
#' @param workers The number of workers, or NULL for one per available core.
#' @return A socket cluster of the parallel package, with the package loaded on its workers.
#' @noRd
netsimr_start_workers <- function(workers = NULL) {
  netsimr_check_workers(workers)
  if (is.null(workers)) workers <- netsimr_available_cores()
  #with Nagle's algorithm on, the many small writes of a run's settings and results stall
  #on the sockets for a few tenths of a second (a fifth of a second each on Windows), more
  #than a small run takes; the master's end of the sockets is made at the start
  old_options <- options(socketOptions = "no-delay")
  on.exit(options(old_options), add = TRUE)
  cluster <- parallel::makePSOCKcluster(as.integer(workers))
  #load the package on the workers now, about a second, rather than in the first run
  tryCatch(parallel::clusterCall(cluster, loadNamespace, "NetSimR"), error = function(e) NULL)
  cluster
}

#' Check the workers argument of a parallel run
#'
#' @param workers NULL, a whole number of at least 1, a cluster, or a function that returns one.
#' @return NULL, invisibly; an error for anything else.
#' @noRd
netsimr_check_workers <- function(workers) {
  if (is.null(workers) || inherits(workers, "cluster") || is.function(workers)) return(invisible(NULL))
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1 || workers != round(workers)) {
    stop("workers must be NULL, a whole number of at least 1, a cluster from parallel::makeCluster(), or a function that returns one.", call. = FALSE)
  }
  invisible(NULL)
}

#' The record of the last simulation run's processing
#'
#' \code{simulate_function()} sets \code{parallel} (TRUE when the chunks ran on workers) and
#' \code{workers} (how many) on every call, so that the simulator app can show how an
#' "auto" run went.
#' @noRd
netsimr_last_run <- new.env(parent = emptyenv())

#' The costs that decide whether an "auto" run goes to parallel workers
#'
#' Measured on a 6-core, 12-thread Windows machine with R 4.6 (the decision rests on their
#' ratios, which change less between machines than the times do): a second per 6.7 million
#' claims drawn and per million simulations (5 million when each total is drawn in one step)
#' in one session; workers that run \code{w} chunk groups at once are \code{w^0.75} times as
#' fast (memory and shared cores limit the gain); sending a run to warm workers takes about
#' 0.03 seconds and 0.1 milliseconds per chunk (each chunk's seed and results travel), and
#' starting workers 0.6 seconds and 0.09 more per worker.
#' @noRd
netsimr_parallel_costs <- list(
  claim = 1.5e-7, simulation = 1e-6, simulation_one_step = 2e-7,
  scaling = 0.75, dispatch = 0.03, per_chunk = 1e-4, start = 0.6, start_per_worker = 0.09,
  #the share of the single-process time a parallel run must beat, as it has no live progress
  gain = 0.75
)

#' How many workers an "auto" run should use
#'
#' Compares the time of the chunks in one session (measured, or estimated from the expected
#' number of claims drawn and the number of simulations) with their time on workers, from
#' the number of chunks and the cost of starting workers that are not running yet; the
#' chunks go to the workers when that is at least a quarter faster.
#' @param claims The expected number of claims drawn by the whole run.
#' @param simulations The number of simulations.
#' @param n_chunks The number of chunks; the workers take contiguous groups of them.
#' @param one_step TRUE when each simulation's total is drawn in one step.
#' @param running The number of workers already running, or NULL.
#' @param start The number of workers that would be started, or NULL to choose the fastest
#'   number up to the available cores.
#' @param single The time the chunks would take in this session, in seconds, when it has been
#'   measured (an "auto" run times one chunk); NULL to estimate it from the claims and
#'   simulations.
#' @return The number of workers to use, 0 to run in the session.
#' @noRd
netsimr_parallel_plan <- function(claims, simulations, n_chunks, one_step = FALSE, running = NULL, start = NULL,
                                  single = NULL) {
  costs <- netsimr_parallel_costs
  if (n_chunks < 2) return(0L)
  if (is.null(single)) {
    if (!is.finite(claims)) return(0L)
    single <- costs$claim * claims + simulations * if (one_step) costs$simulation_one_step else costs$simulation
  }
  if (!is.finite(single)) return(0L)
  candidates <- if (!is.null(running)) running else if (!is.null(start)) start else seq_len(netsimr_available_cores())
  candidates <- candidates[candidates >= 2]
  if (!length(candidates)) return(0L)
  #the chunks are split into one contiguous group per worker, so the slowest worker runs
  #ceiling(n_chunks / workers) of them
  in_parallel <- n_chunks / ceiling(n_chunks / pmin(candidates, n_chunks))
  time <- single / in_parallel^costs$scaling + costs$dispatch + costs$per_chunk * n_chunks
  if (is.null(running)) time <- time + costs$start + costs$start_per_worker * candidates
  best <- which.min(time)
  if (time[best] <= costs$gain * single) as.integer(candidates[best]) else 0L
}

#' Shut down the workers of a parallel run
#'
#' Asks each worker to stop; when that fails, as for a worker that has died, closes the
#' connections of all of them, which ends the workers still running and leaves no
#' connection for the garbage collector to close later with a warning.
#' @param cluster A socket cluster of the parallel package, or NULL.
#' @return NULL, invisibly.
#' @noRd
netsimr_stop_workers <- function(cluster) {
  if (is.null(cluster)) return(invisible(NULL))
  stopped <- tryCatch({
    parallel::stopCluster(cluster)
    TRUE
  }, error = function(e) FALSE)
  if (!stopped) {
    for (node in cluster) tryCatch(close(node$con), error = function(e) NULL)
  }
  invisible(NULL)
}

#' Run the chunks of a simulation on parallel workers
#'
#' Splits the chunks into one contiguous group per worker and sends each group to one
#' worker, so the setup cost is paid once per worker rather than once per chunk. Each
#' chunk starts from its own random stream, set in the worker just before it runs, so the
#' results are identical to a sequential run, and the caller's stream is not touched. An
#' error in a chunk is caught on its worker, which stays healthy, and raised again here
#' with its message; the warnings of the chunks are raised again here too. Any other
#' error, such as a worker that died, has the class \code{netsimr_worker_error}.
#' @param cluster A socket cluster of the parallel package.
#' @param run_chunk Function of the chunk index that returns the chunk's results.
#' @param chunk_seeds List of L'Ecuyer-CMRG seeds (\code{.Random.seed} values), one per chunk.
#' @param chunks The numbers of the chunks, one per seed, that run_chunk() is given.
#' @return List of the chunk results, in chunk order.
#' @noRd
run_chunks_on_workers <- function(cluster, run_chunk, chunk_seeds, chunks = seq_along(chunk_seeds)) {
  n_chunks <- length(chunk_seeds)
  groups <- parallel::splitIndices(n_chunks, min(n_chunks, length(cluster)))
  tasks <- lapply(groups, function(k) list(chunks = chunks[k], seeds = chunk_seeds[k]))
  run_group <- function(task) {
    warnings <- character(0)
    result <- tryCatch(
      withCallingHandlers(
        lapply(seq_along(task$chunks), function(k) {
          assign(".Random.seed", task$seeds[[k]], envir = globalenv())
          run_chunk(task$chunks[[k]])
        }),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) structure(list(message = conditionMessage(e)), class = "netsimr_chunk_error")
    )
    list(result = result, warnings = warnings)
  }
  groups_done <- tryCatch(
    parallel::clusterApply(cluster, tasks, run_group),
    error = function(e) {
      stop(structure(class = c("netsimr_worker_error", "error", "condition"),
                     list(message = paste("A parallel worker failed:", conditionMessage(e)), call = NULL)))
    }
  )
  for (message in unique(unlist(lapply(groups_done, `[[`, "warnings")))) warning(message, call. = FALSE)
  for (group in groups_done) {
    if (inherits(group$result, "netsimr_chunk_error")) stop(group$result$message, call. = FALSE)
  }
  unlist(lapply(groups_done, `[[`, "result"), recursive = FALSE, use.names = FALSE)
}
