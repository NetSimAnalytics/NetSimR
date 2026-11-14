##############
#The simulation engine
##############

# simulate_function(), which simulate_claims() and the simulator run: claim counts and sizes
# from the distributions of R/simulation_distributions.R, the severity cap and the layers,
# in chunks run in parallel (R/parallel_workers.R) when that is faster.

#' The largest number of simulations a run may have
#'
#' Used by the settings validation and by the number of simulations input of the app.
#'
#' @noRd
max_number_of_simulations <- 10000000L

#' Apply severity cap function
#'
#' @param claims A vector of Claims.
#' @param severity_cap_boolean A variable that if true, the function will cap the claims, otherwise will just return them.
#' @param severity_cap_amount The claim cap value.
#' @return If \code{severity_cap_boolean} is true, then will return the minimum of \code{severity_cap_amount} or \code{claims} otherwise will return \code{claims}. The operation is vectorised.
#' @noRd
apply_severity_cap <- function(claims, severity_cap_boolean, severity_cap_amount){
  if (!severity_cap_boolean) return(claims)
  pmin(claims, severity_cap_amount)
}

#' A vector with the reinsurance structure options
#'
#' The names of the reinsurance structures that \code{\link{apply_deductible_limit}} and
#' \code{\link{simulate_function}} accept, used by the simulator app as the choices of its
#' structure inputs and to validate saved settings.
#'
#' @return The reinsurance structure options
#' @keywords internal
reinsurance_structures_options <- c('No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer')

#' Apply a deductible and limit to claims
#'
#' Works out what a reinsurance structure cedes (or, for an excluded layer, leaves) of each
#' claim, for pricing a layer or checking the simulator's figures by hand.
#'
#' @param gross_claims_data A vector of Claims.
#' @param reinsurance_structure The chosen reinsurance structure, a single string. Options are: 'No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer'; anything else is an error.
#' @param deductible The deductible of the reinsurance structure, zero or more. Not used by 'No Reinsurance Structure'.
#' @param limit The limit of the reinsurance structure, zero or more. Used only by 'Limited Layer' and 'Exclude Layer'.
#' @return A vector with one value per claim: for 'Unlimited Layer' and 'Limited Layer', the
#' amount ceded to the layer; for 'Exclude Layer', the claims with the layer taken out (the
#' claims less what a 'Limited Layer' with the same deductible and limit would cede); for
#' 'No Reinsurance Structure', the claims unchanged. Stops with an error when a deductible
#' or limit the structure uses is negative.
#' @export
#' @examples
#' apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 40, 20)
#' apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 10, 30)
#' apply_deductible_limit(c(100, 50, 20), 'Exclude Layer', 40, 20)
apply_deductible_limit <- function(gross_claims_data, reinsurance_structure, deductible, limit){
  #NA or several structures would otherwise give "missing value where TRUE/FALSE needed" or
  #"the condition has length > 1" below
  if (!(is.character(reinsurance_structure) && length(reinsurance_structure) == 1 &&
        reinsurance_structure %in% reinsurance_structures_options)) {
    given <- if (is.character(reinsurance_structure) && length(reinsurance_structure) == 1) {
      paste0(" (got '", reinsurance_structure, "')")
    } else {
      ""
    }
    stop("Unknown reinsurance structure", given, ": reinsurance_structure must be one of ",
         paste0("'", reinsurance_structures_options, "'", collapse = ", "), ".", call. = FALSE)
  }
  if (reinsurance_structure == 'No Reinsurance Structure') {return(gross_claims_data)}

  #a negative amount would cede more than the claims (or less than nothing)
  if (isTRUE(any(deductible < 0))) stop("The deductible must not be negative.", call. = FALSE)
  layer_claims <- pmax(gross_claims_data - deductible, 0)

  if (reinsurance_structure == 'Unlimited Layer') {return(layer_claims)}

  if (isTRUE(any(limit < 0))) stop("The limit must not be negative.", call. = FALSE)
  limited_layer_claims <- pmin(layer_claims, limit)

  if (reinsurance_structure == 'Limited Layer') {return(limited_layer_claims)}

  # 'Exclude Layer', the only option left
  gross_claims_data - limited_layer_claims
}

#' Whether the simulator asks a run for its largest claims
#'
#' The largest claim of each period (for occurrence exceedance) needs every claim drawn,
#' which a run without a layer, cap or slice otherwise skips by drawing each period's total
#' in one step. The app asks for them unless the run expects more than 20 million claims,
#' about 4 seconds of drawing, so a very large run is not slowed down.
#'
#' @param freq_mean The mean number of claims per period, or NA when it is not known.
#' @param n The number of simulations.
#' @return TRUE or FALSE.
#' @noRd
sim_wants_largest_claims <- function(freq_mean, n) {
  expected <- suppressWarnings(as.numeric(freq_mean) * as.numeric(n))
  !isTRUE(expected > 2e7)
}

#' Find missing or invalid simulation settings
#'
#' Checks every setting that the chosen options require: that it is a number, and that
#' it lies in its allowed range. Distribution parameters, slice alphas, counts (the number
#' of simulations, the chunk size, the number of reinstatements) and the seed must be
#' finite; the severity cap, deductibles, limits and slice thresholds may be Inf.
#'
#' @param settings A named list of \code{simulate_function} arguments.
#' @return A character vector naming each setting that is missing or invalid.
#' An empty vector means the settings are complete.
#' @noRd
find_missing_simulation_settings <- function(settings) {
  s <- settings
  problems <- character(0)
  nth <- function(x, i) if (i <= length(x)) x[[i]] else NULL
  #a number that must be present and at least (or, when strict, above) a minimum
  check_amount <- function(value, label, minimum = 0, strict = FALSE, whole = FALSE, maximum = NA, finite = FALSE) {
    if (!is_number(value)) {
      problems <<- c(problems, label)
      return(invisible(NULL))
    }
    if (finite && !is.finite(value)) {
      problems <<- c(problems, paste(label, "must be finite"))
      return(invisible(NULL))
    }
    if (whole && value != round(value)) problems <<- c(problems, paste(label, "must be a whole number"))
    if (value < minimum || (strict && value == minimum)) {
      problems <<- c(problems, paste(label, if (strict) "must be greater than" else "must be at least", format(minimum)))
    }
    if (!is.na(maximum) && value > maximum) problems <<- c(problems, paste(label, "must be at most", format(maximum)))
    invisible(NULL)
  }
  check_params <- function(values, distr, options, group) {
    if (!(is.character(distr) && length(distr) == 1 && distr %in% names(options))) {
      problems <<- c(problems, paste(group, "distribution"))
      return(invisible(NULL))
    }
    found <- distribution_param_problems(options[[distr]], values)
    if (length(found)) problems <<- c(problems, paste(group, found))
    invisible(NULL)
  }

  if (!is_number(s$numOfSimulations)) {
    problems <- c(problems, "Number of simulations")
  } else if (!is.finite(s$numOfSimulations) || s$numOfSimulations != round(s$numOfSimulations) ||
             s$numOfSimulations < 1 || s$numOfSimulations > max_number_of_simulations) {
    problems <- c(problems, paste(
      "Number of simulations must be a whole number between 1 and"
      ,format(max_number_of_simulations, big.mark = ",", scientific = FALSE)
    ))
  }
  if (!is.null(s$chunk_size)) check_amount(s$chunk_size, "Chunk size", minimum = 1, whole = TRUE, finite = TRUE)
  check_params(s$freq_params, s$freqDistr, freq_dist_options, "Frequency")
  check_params(s$sev_params, s$sevDistr, sev_dist_options, "Severity")
  if (isTRUE(s$seedSetBinary)) {
    if (!is_number(s$seedValue)) {
      problems <- c(problems, "Seed value")
    } else if (!is.finite(s$seedValue) || s$seedValue != round(s$seedValue)) {
      problems <- c(problems, "Seed value must be a whole number")
    } else if (abs(s$seedValue) > .Machine$integer.max) {
      #set.seed() only takes integers
      largest <- format(.Machine$integer.max, big.mark = ",", scientific = FALSE)
      problems <- c(problems, paste0("Seed value must be between -", largest, " and ", largest))
    }
  }

  #a Normal truncated at zero needs some probability mass above zero to sample from
  if (isTRUE(s$sevTruncateAtZero) && identical(s$sevDistr, "Normal")) {
    sev <- suppressWarnings(as.numeric(unlist(s$sev_params)))
    if (length(sev) == 2 && all(is.finite(sev)) && sev[2] >= 0 &&
        stats::pnorm(0, sev[1], sev[2], lower.tail = FALSE) < 1e-9) {
      problems <- c(problems, paste(
        "Severity Normal truncated at zero has almost no probability above zero",
        "(increase the mean or reduce the standard deviation)"
      ))
    }
  }

  if (isTRUE(s$paretoSlice)) {
    if (!is_number(s$pareto_slice_times)) {
      problems <- c(problems, "Number of Pareto Slices")
    } else if (s$pareto_slice_times != round(s$pareto_slice_times) ||
               s$pareto_slice_times < 1 || s$pareto_slice_times > max_number_of_pareto_slices) {
      problems <- c(problems, paste("Number of Pareto Slices must be a whole number from 1 to", max_number_of_pareto_slices))
    } else {
      for (j in seq_len(s$pareto_slice_times)) {
        check_amount(nth(s$slice_pareto_alphas, j), paste("Slice", j, "alpha"), minimum = 0, strict = TRUE, finite = TRUE)
        check_amount(nth(s$slice_pareto_x_ms, j), paste("Slice", j, "threshold (x_m)"), minimum = 0, strict = TRUE)
      }
      #each slice replaces the tail above its threshold, so thresholds must increase
      #(compared pairwise rather than with diff(), which gives NaN for two Inf thresholds)
      x_ms <- utils::head(suppressWarnings(as.numeric(unlist(s$slice_pareto_x_ms))), s$pareto_slice_times)
      if (length(x_ms) == s$pareto_slice_times && length(x_ms) > 1 &&
          !anyNA(x_ms) && !all(x_ms[-1] > x_ms[-length(x_ms)])) {
        problems <- c(problems, "Slice thresholds must increase from one slice to the next")
      }
    }
  }

  if (isTRUE(s$sevCapBinary)) check_amount(s$sev_cap_amount, "Severity Cap Amount")

  layers_with_deductible <- c('Unlimited Layer', 'Limited Layer', 'Exclude Layer')
  layers_with_limit <- c('Limited Layer', 'Exclude Layer')

  is_structure <- function(x) is.character(x) && length(x) == 1 && x %in% reinsurance_structures_options
  if (!is_structure(s$reinsuranceStructureEEL)) problems <- c(problems, "EEL reinsurance structure")
  if (!is_structure(s$reinsuranceStructureAL)) problems <- c(problems, "AL reinsurance structure")

  if (isTRUE(s$reinsuranceStructureEEL %in% layers_with_deductible)) {
    check_amount(s$reinsurance_structure_eel_deductible_amount, "EEL Deductible Amount")
  }
  if (isTRUE(s$reinsuranceStructureEEL %in% layers_with_limit)) {
    check_amount(s$reinsurance_structure_eel_limit_amount, "EEL Limit Amount", strict = TRUE)
  }
  if (isTRUE(s$reinsuranceStructureEEL == 'Limited Layer') &&
      isTRUE(s$reinsuranceStructureLimitedReinstatements)) {
    check_amount(s$reinsuranceStructureReinstatementLimit, "Number of Reinstatements", whole = TRUE, finite = TRUE)
    #an upfront premium is optional; given, it prices the reinstatements used
    premium <- s$reinsurance_structure_eel_premium
    if (!is.null(premium) && !(length(premium) == 1 && is.na(premium))) {
      check_amount(premium, "EEL Upfront Premium", finite = TRUE)
      rates <- s$reinsurance_structure_eel_reinstatement_rates
      if (is.null(rates)) rates <- 1
      if (!is.numeric(rates) || length(rates) == 0 || any(!is.finite(rates)) || any(rates < 0)) {
        problems <- c(problems, "Reinstatement premium rates must be non-negative numbers")
      } else if (length(rates) > 1 && is_number(s$reinsuranceStructureReinstatementLimit) &&
                 length(rates) != s$reinsuranceStructureReinstatementLimit) {
        problems <- c(problems, paste("Reinstatement premium rates must be one rate, or one for each of the",
                                      s$reinsuranceStructureReinstatementLimit, "reinstatements"))
      }
    }
  }

  if (isTRUE(s$reinsuranceStructureAL %in% layers_with_deductible)) {
    check_amount(s$reinsurance_structure_al_deductible_amount, "AL Deductible Amount")
  }
  if (isTRUE(s$reinsuranceStructureAL %in% layers_with_limit)) {
    check_amount(s$reinsurance_structure_al_limit_amount, "AL Limit Amount", strict = TRUE)
  }

  problems
}

#' Simulate insurance claims with reinsurance structures
#'
#' A function to simulate frequency - severity of insurance claims using chunked vectorisation.
#' The function applies severity cap, reinsurance structure for each and every loss claim,
#' reinsurance structure for aggregate claims, and allows for piecewise Pareto slices
#'
#' Order of the calculations, for each simulation (a period, e.g. a year): claims are drawn
#' from the severity distribution (with its Pareto slices) and capped at the severity cap;
#' the each-and-every-loss (EEL) structure applies to each claim; the results are summed
#' over the period; then the aggregate deductible comes off that sum, and finally the
#' aggregate limit and the reinstatement capacity cap what is left. In short: cap -> EEL
#' layer per claim -> annual sum -> aggregate deductible -> aggregate limit and
#' reinstatement capacity.
#'
#' The reinstatement capacity applies to a 'Limited Layer' EEL structure with limited
#' reinstatements, which pays at most \code{(reinstatements + 1) * limit} in a period. With
#' an aggregate 'Unlimited Layer' or 'Limited Layer', the ceded total is
#' \code{min(max(S - aggregate deductible, 0), aggregate limit, (reinstatements + 1) * limit)},
#' where S is the sum of the period's EEL recoveries before any capacity (the market
#' convention for an annual aggregate deductible). For example, three claims of 100 through
#' a layer of 100 excess of 0 with no reinstatements and an aggregate deductible of 50 cede
#' \code{min(300 - 50, 100) = 100}. Without an aggregate structure, the capacity caps S.
#' With an aggregate 'Exclude Layer', the capacity caps S first and the aggregate layer is
#' then taken out of the capped amount. An unlimited EEL layer, or limited reinstatements
#' switched off, has no capacity cap. (Before version 0.2.0 the capacity was applied before
#' the aggregate deductible, which gave smaller ceded totals when the two were combined.)
#'
#' Totals are returned at full precision; round them only for display.
#'
#' Random numbers: each chunk of simulations uses its own L'Ecuyer-CMRG random stream,
#' derived from one seed, so a run gives the same results whether or not it runs in
#' parallel. With \code{seedSetBinary = TRUE} (the default when a \code{seedValue} is given)
#' the run is reproducible from \code{seedValue} and the caller's random number stream is
#' left unchanged; otherwise the seed is drawn from the caller's stream, so
#' \code{set.seed()} before the call also makes it reproducible.
#' The streams always use Inversion for normal draws and Rejection sampling, so a seed
#' gives the same results whatever the caller's \code{RNGkind()}, which is restored afterwards.
#' Results depend on the chunk size, which by default adapts to the expected number of
#' claims per simulation.
#'
#' @param numOfSimulations The number of simulations to run.
#' @param freq_params A vector of the frequency distribution parameters.
#' @param sev_params A vector of the severity distribution parameters.
#' @param seedSetBinary True if there is a fixed seed (\code{seedValue}), otherwise false. Defaults to TRUE when a \code{seedValue} is given and FALSE otherwise, so a \code{seedValue} on its own makes the run reproducible; an explicit FALSE ignores \code{seedValue}.
#' @param seedValue The seed value, a whole number between \code{-.Machine$integer.max} and \code{.Machine$integer.max}, or NULL (the default) for no fixed seed.
#' @param freqDistr The frequency distribution: \code{"Poisson"}, \code{"Negative_Binomial"}, \code{"Binomial"}, \code{"Zero_Inflated_Poisson"}, \code{"Zero_Inflated_Negative_Binomial"} or \code{"Fixed_number_of_Counts"}. The parameters of each are listed in \code{\link{simulate_claims}}.
#' @param sevDistr The severity distribution: \code{"Normal"}, \code{"LogNormal"}, \code{"Gamma"}, \code{"Exponential"}, \code{"Pareto"} or \code{"Fixed_Severity"}. The parameters of each are listed in \code{\link{simulate_claims}}.
#' @param paretoSlice True if there is Pareto slicing.
#' @param pareto_slice_times The number of Pareto slices.
#' @param slice_pareto_alphas A vector of Pareto slices' alpha parameters.
#' @param slice_pareto_x_ms A vector of Pareto slices' x_m parameters.
#' @param sevCapBinary True if there is a severity cap.
#' @param sev_cap_amount The severity cap amount.
#' @param reinsuranceStructureEEL The chosen reinsurance structure for each and every loss claim.
#' @param reinsurance_structure_eel_deductible_amount The deductible for each and every loss reinsurance structure.
#' @param reinsurance_structure_eel_limit_amount The limit for each and every loss reinsurance structure.
#' @param reinsuranceStructureAL The chosen reinsurance structure for aggregate claims.
#' @param reinsurance_structure_al_deductible_amount The deductible for aggregate reinsurance structure.
#' @param reinsurance_structure_al_limit_amount The limit for aggregate reinsurance structure.
#' @param reinsuranceStructureLimitedReinstatements True if there is a limit in reinstatements, otherwise false.
#' @param reinsuranceStructureReinstatementLimit The reinstatement limit.
#' @param reinsurance_structure_eel_premium The upfront premium of the EEL layer, or NULL (the default) for none. Given with a limited number of reinstatements, the results gain the column \code{reinstatement_premium}; ignored otherwise.
#' @param reinsurance_structure_eel_reinstatement_rates The premium rate of each reinstatement as a share of the upfront premium: one rate for all of them, or one per reinstatement. Defaults to 1 (100\%). See \code{\link{reinstatement_premium}}.
#' @param multiprocessing TRUE to run the chunks in parallel on worker R processes (base R's \pkg{parallel} package), FALSE (the default) to run them in this session, or "auto" to run them on workers only when that is expected to be at least a quarter faster. Results are the same either way. "auto" estimates both times from the expected number of claims drawn, the number of simulations and chunks, the cores, and whether the workers are already running: starting them takes about a second, so a single run goes to them from a few seconds of work, while a cluster given as \code{workers} is used from a fraction of a second; with \code{workers = NULL} it also chooses how many to start. A run of a single chunk runs in this session, unless TRUE is given with a cluster.
#' @param workers The workers of a parallel run: \code{NULL} (the default) for one per available core (\code{getOption("mc.cores")} if set, otherwise \code{parallel::detectCores()}, at most 2 under \code{R CMD check}), or a number of workers; these are started for the call and shut down when it finishes, so each call pays the start-up cost. To reuse workers across calls, pass a cluster made with \code{parallel::makeCluster()}: it is used as it is and left running, to be stopped with \code{parallel::stopCluster()}. A function with no arguments that returns such a cluster is called only when the run goes to the workers (as "auto" decides), which lets a caller start them when they are first needed. Ignored unless \code{multiprocessing} is TRUE or "auto".
#' @param sevTruncateAtZero True to draw Normal severities from the Normal distribution truncated at zero, so that no claim is negative. Ignored for other severity distributions. Defaults to FALSE.
#' @param chunk_size The number of simulations processed per vectorised batch. By default (NULL) it is chosen from the expected number of claims per simulation, so that a batch holds about a million claims (between 100 and 10,000 simulations). Because of the floor of 100 simulations, a batch holds more than a million claims when the mean frequency exceeds 10,000 claims per simulation (about 100 million at a mean of a million), and memory use grows with it; give a smaller \code{chunk_size} to keep batches small. Results with a fixed seed depend on the chunk size.
#' @param gross True (the default) to return the gross total claims before reinsurance. Set it to FALSE when only the totals after the structures are needed: with an each-and-every-loss layer this allows drawing only the claims that reach the layer, which is much faster.
#' @param shortcuts True (the default) to use exact shortcuts where the settings allow: when no layer, cap, Pareto slice or truncation acts on individual claims, each simulation's total is drawn in one step for the Normal, Gamma, Exponential and fixed severities; with \code{gross = FALSE} and a layer, only the claims above the deductible are drawn. The results follow the same distribution as without shortcuts. Set it to FALSE to simulate every claim.
#' @param progress An optional function called after each chunk of a sequential run with the fraction done and a short description, e.g. to update a progress bar.
#' @param reinsurance_structure_eel_dedctible_amount,reinsurance_structure_al_dedctible_amount Deprecated misspellings of \code{reinsurance_structure_eel_deductible_amount} and \code{reinsurance_structure_al_deductible_amount}, the names before version 0.4.0; they still work, with a warning.
#' @param largest_claims True to add the column \code{largest_claim}: each simulation's largest single claim, for occurrence exceedance (OEP) as \code{total_claims} gives aggregate exceedance (AEP). Defaults to FALSE. Every claim is then drawn, so the one-step shortcut for totals (see \code{shortcuts}) is not used.
#' @return A data frame with one row per simulation, at full precision: \code{claim_counts},
#' the claim count; \code{total_claims}, the total claims after the reinsurance structures;
#' \code{gross_claims}, the gross total claims before them (after Pareto slices and the
#' severity cap; unless \code{gross = FALSE}); and, when reinstatements are limited,
#' \code{number_of_reinstatements_used}: the EEL layer's recoveries in the period divided by
#' the EEL limit, capped at the number of reinstatements, so reinstatements are counted pro
#' rata to the amount recovered. The recoveries are taken after the aggregate deductible and
#' limit of an aggregate 'Unlimited Layer' or 'Limited Layer', but before an aggregate
#' 'Exclude Layer' is taken out: with an exclusion they are the EEL recoveries after the
#' reinstatement capacity. For example, three claims of 100 through a layer of 60 excess of
#' 30 with two reinstatements and an aggregate exclusion of 150 excess of 50 give a total of
#' 50 but 2 reinstatements used (180 / 60, capped at 2). With an upfront premium,
#' \code{reinstatement_premium}: the premium for the reinstatements used, pro rata to the
#' amount reinstated (see \code{\link{reinstatement_premium}}). With \code{largest_claims =
#' TRUE}, \code{largest_claim}: the simulation's largest single claim after the
#' each-and-every-loss structure (the layer's loss on that claim; the gross claim when there
#' is no structure), before the reinstatement limit and the aggregate layer, which act on
#' the period's total; 0 for a simulation without claims.
#' Stops with an error that names any required setting that is missing or invalid.
#' @seealso \code{\link{simulate_claims}}, a simpler interface with short argument names,
#'   and \code{\link{run_shiny_simulator}} for the same model in an app.
#' @export
#' @examples
#' # 1,000 simulated years of Poisson claim counts with Normal claim sizes, no reinsurance
#' results <- simulate_function(
#'   numOfSimulations = 1000, freq_params = 3, sev_params = c(1000, 200),
#'   seedSetBinary = TRUE, seedValue = 1, freqDistr = "Poisson", sevDistr = "Normal"
#' )
#' summary(results$total_claims)
#'
#' # the same claims ceded to a layer of 1,500 excess of 800 on each claim
#' layer <- simulate_function(
#'   numOfSimulations = 1000, freq_params = 3, sev_params = c(1000, 200),
#'   seedSetBinary = TRUE, seedValue = 1, freqDistr = "Poisson", sevDistr = "Normal",
#'   reinsuranceStructureEEL = "Limited Layer",
#'   reinsurance_structure_eel_deductible_amount = 800,
#'   reinsurance_structure_eel_limit_amount = 1500
#' )
#' mean(layer$total_claims)
simulate_function <- function(
    numOfSimulations,
    freq_params,
    sev_params,
    seedSetBinary = !is.null(seedValue),
    seedValue = NULL,
    freqDistr,
    sevDistr,
    paretoSlice = FALSE,
    pareto_slice_times = NULL,
    slice_pareto_alphas = NULL,
    slice_pareto_x_ms = NULL,
    sevCapBinary = FALSE,
    sev_cap_amount = NULL,
    reinsuranceStructureEEL = "No Reinsurance Structure",
    reinsurance_structure_eel_deductible_amount = NULL,
    reinsurance_structure_eel_limit_amount = NULL,
    reinsuranceStructureAL = "No Reinsurance Structure",
    reinsurance_structure_al_deductible_amount = NULL,
    reinsurance_structure_al_limit_amount = NULL,
    reinsuranceStructureLimitedReinstatements = FALSE,
    reinsuranceStructureReinstatementLimit = NULL,
    reinsurance_structure_eel_premium = NULL,
    reinsurance_structure_eel_reinstatement_rates = 1,
    multiprocessing = FALSE,
    workers = NULL,
    sevTruncateAtZero = FALSE,
    chunk_size = NULL,
    gross = TRUE,
    shortcuts = TRUE,
    progress = NULL,
    largest_claims = FALSE,
    reinsurance_structure_eel_dedctible_amount = NULL,
    reinsurance_structure_al_dedctible_amount = NULL
){
  #the deductibles' names before 0.4.0, misspelled, still work
  for (layer in c("eel", "al")) {
    old_value <- get(paste0("reinsurance_structure_", layer, "_dedctible_amount"))
    if (!is.null(old_value)) {
      new_name <- paste0("reinsurance_structure_", layer, "_deductible_amount")
      warning("reinsurance_structure_", layer, "_dedctible_amount is deprecated: use ", new_name, ".", call. = FALSE)
      if (is.null(get(new_name))) assign(new_name, old_value)
    }
  }
  reinsurance_structure_eel_dedctible_amount <- reinsurance_structure_al_dedctible_amount <- NULL
  rm(layer, old_value)
  #collect the settings; an omitted argument without a default is treated as not set (NULL)
  arg_env <- environment()
  arg_defaults <- formals(sys.function())
  has_no_default <- vapply(arg_defaults, function(d) is.symbol(d) && as.character(d) == "", logical(1))
  settings <- list()
  for (arg in names(arg_defaults)) {
    if (has_no_default[[arg]] && do.call(missing, list(as.name(arg)), envir = arg_env)) {
      assign(arg, NULL, envir = arg_env)
    }
    settings[arg] <- list(get(arg, envir = arg_env))
  }

  #stop early with a clear message if a required setting is missing or invalid
  missing_settings <- find_missing_simulation_settings(settings)
  if (length(missing_settings) > 0) {
    stop("Missing or invalid settings: ", paste(missing_settings, collapse = ", "), call. = FALSE)
  }

  #parameters may be supplied as lists; use plain numeric vectors from here on
  freq_params <- as.numeric(unlist(freq_params))
  sev_params <- as.numeric(unlist(sev_params))
  freq_object <- freq_dist_options[[freqDistr]]
  sev_object <- sev_dist_options[[sevDistr]]

  #the severity for this run: the chosen distribution, truncated and sliced as asked
  truncate <- isTRUE(sevTruncateAtZero) && identical(sevDistr, "Normal")
  model <- severity_model(sev_object, sev_params, truncate)
  slices <- NULL
  if (isTRUE(paretoSlice)) {
    k <- pareto_slice_times
    slices <- pareto_splice(
      model,
      thresholds = as.numeric(unlist(slice_pareto_x_ms))[seq_len(k)],
      alphas = as.numeric(unlist(slice_pareto_alphas))[seq_len(k)]
    )
  }
  cap <- if (isTRUE(sevCapBinary)) sev_cap_amount else Inf

  #exact shortcuts: one draw per simulation when nothing acts on individual claims, or
  #only the claims above the deductible when the gross totals are not needed
  #(the largest claims need the claims themselves, which a total drawn in one step skips)
  sum_in_one_step <- isTRUE(shortcuts) && is.null(slices) && !isTRUE(sevCapBinary) && !truncate &&
    identical(reinsuranceStructureEEL, "No Reinsurance Structure") && !is.null(sev_object@sum_func) &&
    !isTRUE(largest_claims)
  #the largest of each simulation's values, a simulation without any being 0; id holds the
  #simulation numbers 1 to n. With a few claims per simulation, the claims are sorted by
  #simulation and size (a radix sort), and each simulation's largest is the last of its
  #claims; with many, split() into one group per simulation is faster, with id made a factor
  #directly, without as.factor()'s sort
  largest_by_simulation <- function(values, id, n) {
    if (!length(values)) return(numeric(n))
    if (length(values) <= 50 * n) {
      claims <- tabulate(id, n)
      largest <- numeric(n)
      has <- claims > 0
      largest[has] <- values[order(id, values, method = "radix")][cumsum(claims)[has]]
      return(largest)
    }
    groups <- split(values, structure(id, levels = as.character(seq_len(n)), class = "factor"))
    vapply(groups, function(x) if (length(x)) max(x) else 0, numeric(1), USE.NAMES = FALSE)
  }
  draw_large_claims_only <- isTRUE(shortcuts) && !isTRUE(gross) &&
    reinsuranceStructureEEL %in% c("Unlimited Layer", "Limited Layer") && !is.null(freq_object@split_func)
  if (draw_large_claims_only) {
    eel_deductible <- reinsurance_structure_eel_deductible_amount
    final_survival <- if (is.null(slices)) model$survival else slices$survival
    final_tail_quantile <- if (is.null(slices)) model$tail_quantile else slices$tail_quantile
    #a claim cedes something only above the deductible; a cap at or below it cedes nothing
    large_prob <- if (cap <= eel_deductible) 0 else final_survival(eel_deductible)
  }

  #chunks: by default about a million claims each, to keep memory use flat
  expected_claims <- distribution_moments(freq_dist_options, freqDistr, freq_params)[["mean"]]
  if (is.null(chunk_size)) {
    chunk_size <- if (is.finite(expected_claims) && expected_claims > 0) {
      min(10000, max(100, floor(1e6 / expected_claims)))
    } else {
      10000
    }
  }
  n_chunks <- ceiling(numOfSimulations / chunk_size)
  chunk_sizes <- rep(chunk_size, n_chunks)
  chunk_sizes[n_chunks] <- numOfSimulations - chunk_size * (n_chunks - 1)

  #random numbers: one L'Ecuyer-CMRG stream per chunk, derived from a single seed, so that
  #sequential and parallel runs agree; the caller's generator is restored afterwards
  global_env <- globalenv()
  if (isTRUE(seedSetBinary)) {
    base_seed <- seedValue
  } else {
    #draw the seed from the caller's stream, so set.seed() before the call makes the run reproducible
    base_seed <- sample.int(.Machine$integer.max, 1L)
  }
  #the normal and sample kinds are pinned too (the streams carry them), so that a seed gives
  #the same results whatever the caller's RNGkind()
  netsimr_local_seed(base_seed, kind = "L'Ecuyer-CMRG")
  chunk_seeds <- vector("list", n_chunks)
  stream <- get(".Random.seed", envir = global_env)
  for (i in seq_len(n_chunks)) {
    chunk_seeds[[i]] <- stream
    stream <- parallel::nextRNGStream(stream)
  }

  #simulations for one chunk
  simulate_chunk <- function(this_n){

    if (draw_large_claims_only) {
      #split the claim counts into large claims (above the deductible) and the rest,
      #and draw only the large claims, from the tail of the severity
      counts <- freq_object@split_func(this_n, freq_params, large_prob)
      totals <- numeric(this_n)
      largest <- numeric(this_n)
      n_large <- sum(counts$large)
      if (n_large > 0) {
        large_claims <- pmin(final_tail_quantile(stats::runif(n_large, min = 0, max = large_prob)), cap)
        ceded <- apply_deductible_limit(
          large_claims
          ,reinsurance_structure = reinsuranceStructureEEL
          ,deductible = eel_deductible
          ,limit = reinsurance_structure_eel_limit_amount
        )
        large_id <- rep.int(seq_len(this_n), counts$large)
        sums <- rowsum(ceded, large_id)
        totals[as.integer(rownames(sums))] <- sums[, 1]
        #the claims not drawn cede nothing, so the largest ceded claim is among those drawn
        if (isTRUE(largest_claims)) largest <- largest_by_simulation(ceded, large_id, this_n)
      }
      return(list(claim_counts = counts$total, total_claims = totals, gross_claims = NULL, largest_claim = largest))
    }

    #simulate claim counts for this chunk
    counts <- freq_object@simulate_func(this_n, freq_params)

    if (sum_in_one_step) {
      totals <- sev_object@sum_func(counts, sev_params)
      return(list(claim_counts = counts, total_claims = totals, gross_claims = totals))
    }

    total_claims_needed <- sum(counts)

    #edge case: chunk has zero claims across all simulations
    if(total_claims_needed == 0){
      return(list(claim_counts = counts, total_claims = rep(0, this_n), gross_claims = rep(0, this_n),
                  largest_claim = rep(0, this_n)))
    }

    sim_id <- rep.int(seq_len(this_n), counts)

    #simulate all individual severities for the chunk in one vectorised call
    claims <- model$draw(total_claims_needed)

    #pareto slices: claims above the first threshold are redrawn from the spliced tail
    if (!is.null(slices)) {
      above <- claims > slices$first_threshold
      if (any(above)) claims[above] <- slices$draw_above_first(sum(above))
    }

    #apply severity cap
    claims <- apply_severity_cap(
      claims
      ,severity_cap_boolean = isTRUE(sevCapBinary)
      ,severity_cap_amount = sev_cap_amount
    )

    #keep the gross claims (after tail adjustments and the cap, before any reinsurance)
    gross_claims <- claims

    #apply EEL deductible/limit per individual claim
    claims <- apply_deductible_limit(
      claims
      ,reinsurance_structure = reinsuranceStructureEEL
      ,deductible = reinsurance_structure_eel_deductible_amount
      ,limit = reinsurance_structure_eel_limit_amount
    )

    #sum individual claims back to simulation-level totals, gross and after the EEL structure;
    #simulations with no claims keep a total of zero
    sums <- rowsum(cbind(gross = gross_claims, total = claims), sim_id)
    ids <- as.integer(rownames(sums))
    totals <- numeric(this_n)
    gross_totals <- numeric(this_n)
    totals[ids] <- sums[, "total"]
    gross_totals[ids] <- sums[, "gross"]
    #each simulation's largest single claim, on the basis of its total before the aggregate step
    largest <- if (isTRUE(largest_claims)) largest_by_simulation(claims, sim_id, this_n)

    return(list(claim_counts = counts, total_claims = totals, gross_claims = gross_totals, largest_claim = largest))
  }
  run_chunk <- function(i) simulate_chunk(chunk_sizes[i])

  #run chunks, optionally in parallel across chunks (not per-simulation); "auto" goes to the
  #workers only when that is expected to be faster (see netsimr_parallel_plan()), and a run
  #of a single chunk stays in this session unless a cluster is given (the results are the
  #same either way, as each chunk has its own random stream)
  if (is.character(multiprocessing) && !identical(multiprocessing, "auto")) {
    stop("multiprocessing must be TRUE, FALSE or \"auto\".", call. = FALSE)
  }
  auto <- identical(multiprocessing, "auto")
  if (isTRUE(multiprocessing) || auto) netsimr_check_workers(workers)
  chunk_results <- vector("list", n_chunks)
  run_here <- function(chunks) {
    for (i in chunks) {
      assign(".Random.seed", chunk_seeds[[i]], envir = global_env)
      chunk_results[[i]] <<- run_chunk(i)
      if (is.function(progress)) progress(i / n_chunks, paste("Chunk", i, "of", n_chunks))
    }
  }
  remaining <- seq_len(n_chunks)
  parallel_run <- (isTRUE(multiprocessing) && (n_chunks >= 2 || inherits(workers, "cluster"))) || (auto && n_chunks >= 2)
  if (parallel_run && auto) {
    is_cluster <- inherits(workers, "cluster")
    #"auto" times the second chunk in this session (the first also compiles the code) and
    #decides from it how to run the rest; a run of two chunks is judged from its size alone
    measured <- NULL
    if (n_chunks >= 3) {
      run_here(1L)
      #(proc.time(), as system.time() would first collect the garbage, a twentieth of a second)
      started <- proc.time()[["elapsed"]]
      run_here(2L)
      seconds <- proc.time()[["elapsed"]] - started
      remaining <- seq.int(3L, n_chunks)
      measured <- seconds * sum(chunk_sizes[remaining]) / chunk_sizes[2]
    }
    claims_drawn <- if (sum_in_one_step) {
      0
    } else if (draw_large_claims_only) {
      expected_claims * large_prob * numOfSimulations
    } else {
      expected_claims * numOfSimulations
    }
    #a cluster runs warm; a function starts all the available cores; a number is started as it is
    planned <- netsimr_parallel_plan(
      claims = claims_drawn, simulations = numOfSimulations, n_chunks = length(remaining),
      one_step = sum_in_one_step, running = if (is_cluster) length(workers),
      start = if (is.numeric(workers)) workers else if (is.function(workers)) netsimr_available_cores(),
      single = measured
    )
    parallel_run <- planned > 0
    if (parallel_run && is.null(workers)) workers <- planned
  }
  netsimr_last_run$parallel <- FALSE
  netsimr_last_run$workers <- 0L
  if (parallel_run) {
    #reuse a cluster the caller gives (e.g. the app keeps warm workers), or the one a function
    #given as workers returns (left running for the caller); otherwise start workers for this
    #call and shut them down afterwards
    cluster <- workers
    if (is.function(cluster)) cluster <- cluster()
    if (!inherits(cluster, "cluster")) {
      cluster <- netsimr_start_workers(cluster)
      on.exit(netsimr_stop_workers(cluster), add = TRUE)
    }
    netsimr_last_run$parallel <- TRUE
    netsimr_last_run$workers <- length(cluster)
    if (is.function(progress)) {
      progress((min(remaining) - 1) / n_chunks, paste("On", length(cluster), "parallel workers"))
    }
    #keep objects the workers do not need (e.g. a Shiny progress callback, or the cluster
    #itself, or the chunks already run) out of the chunk closure, which is sent to the workers
    #with its environment
    progress <- NULL
    settings <- NULL
    workers <- NULL
    done <- chunk_results
    chunk_results <- NULL
    done[remaining] <- run_chunks_on_workers(cluster, run_chunk, chunk_seeds[remaining], remaining)
    chunk_results <- done
    rm(done)
  } else {
    run_here(remaining)
  }

  data <- data.frame(
    claim_counts = unlist(lapply(chunk_results, `[[`, "claim_counts"), use.names = FALSE)
    ,total_claims = unlist(lapply(chunk_results, `[[`, "total_claims"), use.names = FALSE)
  )
  if (isTRUE(gross)) {
    data$gross_claims <- unlist(lapply(chunk_results, `[[`, "gross_claims"), use.names = FALSE)
  }
  if (isTRUE(largest_claims)) {
    data$largest_claim <- unlist(lapply(chunk_results, `[[`, "largest_claim"), use.names = FALSE)
  }
  rm(chunk_results)

  #the aggregate step, on each simulation's total after the EEL structure (see Details);
  #a limited EEL layer with limited reinstatements pays at most (reinstatements + 1) limits
  reinstatements_limited <- identical(reinsuranceStructureEEL, 'Limited Layer') &&
    isTRUE(reinsuranceStructureLimitedReinstatements)
  capacity <- if (reinstatements_limited) {
    (reinsuranceStructureReinstatementLimit + 1) * reinsurance_structure_eel_limit_amount
  } else {
    Inf
  }
  apply_al <- function(x) apply_deductible_limit(
    x
    ,reinsuranceStructureAL
    ,reinsurance_structure_al_deductible_amount
    ,reinsurance_structure_al_limit_amount
  )
  if (reinsuranceStructureAL %in% c('Unlimited Layer', 'Limited Layer')) {
    #aggregate layer: its deductible comes off the period's EEL recoveries first, then the
    #aggregate limit and the reinstatement capacity cap what is left
    data$total_claims <- pmin(apply_al(data$total_claims), capacity)
    layer_recoveries <- data$total_claims
  } else {
    #no aggregate layer, or an aggregate exclusion taken out of the capped recoveries; the
    #reinstatements used are counted on the recoveries before the exclusion
    data$total_claims <- pmin(data$total_claims, capacity)
    layer_recoveries <- data$total_claims
    data$total_claims <- apply_al(data$total_claims)
  }
  if (reinstatements_limited) {
    #reinstatements are used pro rata to the amount the layer recovers
    data$number_of_reinstatements_used <- pmin(
      layer_recoveries / reinsurance_structure_eel_limit_amount
      ,reinsuranceStructureReinstatementLimit
    )
    #and priced, given an upfront premium, pro rata to the amount reinstated
    if (!is.null(reinsurance_structure_eel_premium) && !is.na(reinsurance_structure_eel_premium[1])) {
      data$reinstatement_premium <- reinstatement_premium(
        data$number_of_reinstatements_used
        ,reinsurance_structure_eel_premium
        ,reinsurance_structure_eel_reinstatement_rates
      )
    }
  }
  return(data)
}
