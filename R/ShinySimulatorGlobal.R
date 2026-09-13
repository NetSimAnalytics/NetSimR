#help functions

#' Parameter to set the maximum number of pareto slices
#'
#' @return The the maximum number of Pareto Slices.
max_number_of_pareto_slices <- 5

#' Random Pareto generator
#'
#' @param n Number of values to generate.
#' @param alpha A positive real number. Alpha parameter of the Pareto distribution.
#' @param x_m A positive real number. The minimum value for the Pareto distribution.
#' @return A vector of \code{n} random Pareto variables with parameters \code{alpha} and \code{x_m}.
rpareto <- function(n, alpha, x_m) x_m / runif(n)^(1/alpha)

#' Random Normal generator truncated at zero
#'
#' Draws from the Normal distribution conditional on being positive, by inverse-CDF sampling:
#' u ~ U(pnorm(0, mean, sd), 1) and x = qnorm(u, mean, sd).
#'
#' @param n Number of values to generate.
#' @param mean The mean of the underlying Normal distribution.
#' @param sd The standard deviation of the underlying Normal distribution.
#' @return A vector of \code{n} positive random values.
#' @noRd
rnorm_truncated_at_zero <- function(n, mean, sd) {
  lower <- pnorm(0, mean = mean, sd = sd)
  qnorm(runif(n, min = lower, max = 1), mean = mean, sd = sd)
}

#' Apply severity cap function
#'
#' @param claims A vector of Claims.
#' @param severity_cap_boolean A variable that if true, the function will cap the claims, otherwise will just return them.
#' @param severity_cap_amount The claim cap value.
#' @return If \code{severity_cap_boolean} is true, then will return the minimum of \code{severity_cap_amount} or \code{claims} otherwise will return \code{claims}. The operation is vectorised.
apply_severity_cap <- function(claims, severity_cap_boolean, severity_cap_amount){
  if (!severity_cap_boolean) return(claims)
  pmin(claims, severity_cap_amount)
}

#' A vector with the reinsurance structure options
#'
#' @return The reinsurance structure options
reinsurance_structures_options <- c('No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer')

#' Apply a deductible and limit to claims
#'
#' @param gross_claims_data A vector of Claims.
#' @param reinsurance_structure The chosen reinsurance structure. Options are: 'No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer'.
#' @param deductible The deductible of the reinsurance structure.
#' @param limit The limit of the reinsurance structure.
#' @return The ceded claims for the structure, with the chosen deductible and limit.
#' @export
#' @examples
#' apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 40, 20)
#' apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 10, 30)
apply_deductible_limit <- function(gross_claims_data, reinsurance_structure, deductible, limit){
  if (reinsurance_structure == 'No Reinsurance Structure') {return(gross_claims_data)}

  layer_claims <- pmax(gross_claims_data - deductible, 0)

  if (reinsurance_structure == 'Unlimited Layer') {return(layer_claims)}

  limited_layer_claims <- pmin(layer_claims, limit)

  if (reinsurance_structure == 'Limited Layer') {return(limited_layer_claims)}

  if (reinsurance_structure == 'Exclude Layer') {return(gross_claims_data - limited_layer_claims)}

  stop("Unknown reinsurance structure: ", reinsurance_structure)
}

#' The class of the distribution objects
#'
distributionClass <- setClass("distributionClass", slots = c(
  distrID="character"
  ,distr_label="character"
  ,paramIDs="character"
  ,param_labels="character"
  ,param_min_values="numeric"
  ,param_max_values="numeric"
  ,param_whole_numbers="logical"
  ,simulate_func = "function"
))

#' A vector with the frequency distribution objects
#'
#' @return The frequency distribution objects.
freq_dist_options <- c(
  Poisson=distributionClass(
    distrID='Poisson'
    ,distr_label='Poisson'
    ,paramIDs=c("lamda")
    ,param_labels=c("lambda (mean claims)")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpois(n = number_of_simulations, lambda = parameters[1])
    )}
  )
  ,Negative_Binomial=distributionClass(
    distrID='Negative_Binomial'
    ,distr_label='Negative Binomial'
    ,paramIDs=c("r", "beta")
    ,param_labels=c("r (shape)", "beta (scale)")
    ,param_min_values=c(0,0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpois(n = number_of_simulations, lambda = rgamma(n =number_of_simulations, shape = parameters[1], scale = parameters[2]))
    )}
  )
  ,Binomial=distributionClass(
    distrID='Binomial'
    ,distr_label='Binomial'
    ,paramIDs=c("n", "p")
    ,param_labels=c("n (number of trials)", "p (probability)")
    ,param_min_values=c(0,0)
    ,param_max_values=c(NA,1)
    ,param_whole_numbers=c(TRUE, FALSE)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rbinom(n = number_of_simulations, size = parameters[1], prob = parameters[2])
    )}
  )
  ,Fixed_number_of_Counts=distributionClass(
    distrID='Fixed_number_of_Counts'
    ,distr_label='Fixed number of Counts'
    ,paramIDs=c("FixedNumberOfCounts")
    ,param_labels=c("Number of claims")
    ,param_min_values=c(0)
    ,param_whole_numbers=c(TRUE)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rep(parameters[1], number_of_simulations)
    )}
  )
)

#' A data frame with the frequency distribution parameter placeholders
#'
#' @return The frequency distribution parameter placeholders.
freq_dist_parameter_placeholders <- data.frame(
  param_number = seq_len(max(vapply(freq_dist_options, function(x) length(x@paramIDs), integer(1))))
  ,param_id = paste0("freq_param_", seq_len(max(vapply(freq_dist_options, function(x) length(x@paramIDs), integer(1)))))
)

#' A vector with the severity distribution objects
#'
#' @return The severity distribution objects.
sev_dist_options <- c(
  Normal=distributionClass(
    distrID='Normal'
    ,distr_label='Normal'
    ,paramIDs=c("mu", "sigma")
    ,param_labels=c("Mean", "Standard deviation")
    ,param_min_values=c(0, 0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rnorm(n = number_of_simulations, mean = parameters[1], sd = parameters[2])
    )}
  )
  ,LogNormal=distributionClass(
    distrID='LogNormal'
    ,distr_label='Log-Normal'
    ,paramIDs=c("mu", "sigma")
    ,param_labels=c("mu (mean of log)", "sigma (sd of log)")
    ,param_min_values=c(0,0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rlnorm(n = number_of_simulations, meanlog = parameters[1], sdlog = parameters[2])
    )}
  )
  ,Gamma=distributionClass(
    distrID='Gamma'
    ,distr_label='Gamma'
    ,paramIDs=c("shape", "scale")
    ,param_labels=c("Shape", "Scale")
    ,param_min_values=c(0,0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rgamma(n = number_of_simulations, shape = parameters[1], scale = parameters[2])
    )}
  )
  ,Exponential=distributionClass(
    distrID='Exponential'
    ,distr_label='Exponential'
    ,paramIDs=c("rate")
    ,param_labels=c("Rate")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rexp(n = number_of_simulations, rate = parameters[1])
    )}
  )
  ,Pareto=distributionClass(
    distrID='Pareto'
    ,distr_label='Pareto'
    ,paramIDs=c("alpha", "x_m")
    ,param_labels=c("alpha (shape)", "x_m (minimum)")
    ,param_min_values=c(0, 0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpareto(n = number_of_simulations, alpha = parameters[1], x_m = parameters[2])
    )}
  )
  ,Fixed_Severity=distributionClass(
    distrID='Fixed_Severity'
    ,distr_label='Fixed Severity'
    ,paramIDs=c("Fixed_sev_amount")
    ,param_labels=c("Claim amount")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rep(parameters[1], number_of_simulations)
    )}
  )
)

#' A data frame with the severity distribution parameter placeholders
#'
#' @return The severity distribution parameter placeholders.
sev_dist_parameter_placeholders <- data.frame(
  param_number = seq_len(max(vapply(sev_dist_options, function(x) length(x@paramIDs), integer(1))))
  ,param_id = paste0("sev_param_", seq_len(max(vapply(sev_dist_options, function(x) length(x@paramIDs), integer(1)))))
)

#' Mean and standard deviation implied by a distribution's parameters
#'
#' @param options \code{freq_dist_options} or \code{sev_dist_options}.
#' @param id The distribution's \code{distrID}.
#' @param params A numeric vector (or list) of parameters in \code{paramIDs} order.
#' @param truncate_at_zero If TRUE and the distribution is the Normal, the moments of the
#' Normal truncated at zero are returned. Ignored for other distributions.
#' @return A named numeric vector \code{c(mean = , sd = )}. Both are NA when the parameters
#' are missing or invalid, and Inf when the moment does not exist (e.g. Pareto alpha <= 1).
#' @noRd
distribution_moments <- function(options, id, params, truncate_at_zero = FALSE) {
  unavailable <- c(mean = NA_real_, sd = NA_real_)
  if (!(is.character(id) && length(id) == 1 && id %in% names(options))) return(unavailable)
  object <- options[[id]]
  n_params <- length(object@paramIDs)

  #inputs that have not been filled in arrive as NULL entries in a list
  as_number <- function(v) if (is.numeric(v) && length(v) == 1) as.numeric(v) else NA_real_
  p <- if (is.list(params)) vapply(params, as_number, numeric(1)) else suppressWarnings(as.numeric(params))
  if (length(p) < n_params) return(unavailable)
  p <- p[seq_len(n_params)]
  if (anyNA(p)) return(unavailable)

  #parameters outside their allowed range (or fractional where whole numbers are required) are invalid
  pad <- function(x, fill) c(x, rep(fill, max(0, n_params - length(x))))[seq_len(n_params)]
  mins <- pad(object@param_min_values, NA)
  maxs <- pad(object@param_max_values, NA)
  whole <- pad(object@param_whole_numbers, FALSE)
  if (any(!is.na(mins) & p < mins) || any(!is.na(maxs) & p > maxs)) return(unavailable)
  if (any((whole %in% TRUE) & p != round(p))) return(unavailable)

  moments <- switch(
    id
    ,Poisson = c(p[1], sqrt(p[1]))
    #Poisson-Gamma mixture with Gamma shape r and scale beta
    ,Negative_Binomial = c(p[1] * p[2], sqrt(p[1] * p[2] * (1 + p[2])))
    ,Binomial = c(p[1] * p[2], sqrt(p[1] * p[2] * (1 - p[2])))
    ,Fixed_number_of_Counts = c(p[1], 0)
    ,Normal = if (isTRUE(truncate_at_zero)) truncated_normal_moments(p[1], p[2]) else c(p[1], p[2])
    ,LogNormal = c(exp(p[1] + p[2]^2 / 2), sqrt(exp(2 * p[1] + p[2]^2) * (exp(p[2]^2) - 1)))
    ,Gamma = c(p[1] * p[2], sqrt(p[1]) * p[2])
    ,Exponential = if (p[1] > 0) c(1 / p[1], 1 / p[1]) else unavailable
    ,Pareto = pareto_moments(p[1], p[2])
    ,Fixed_Severity = c(p[1], 0)
    ,unavailable
  )
  moments <- as.numeric(moments)
  moments[is.nan(moments)] <- NA_real_
  c(mean = moments[1], sd = moments[2])
}

#' Mean and standard deviation of the Normal distribution truncated at zero
#'
#' @param mu Mean of the underlying Normal distribution.
#' @param sigma Standard deviation of the underlying Normal distribution.
#' @return A numeric vector of length two: the mean and the standard deviation. NA when
#' (almost) no probability mass lies above zero.
#' @noRd
truncated_normal_moments <- function(mu, sigma) {
  if (sigma == 0) return(if (mu > 0) c(mu, 0) else c(NA_real_, NA_real_))
  alpha <- -mu / sigma
  tail <- pnorm(alpha, lower.tail = FALSE)
  if (tail < 1e-9) return(c(NA_real_, NA_real_))
  lambda <- dnorm(alpha) / tail
  variance <- sigma^2 * (1 + alpha * lambda - lambda^2)
  c(mu + sigma * lambda, sqrt(max(variance, 0)))
}

#' Mean and standard deviation of the Pareto distribution
#'
#' @param alpha The shape parameter.
#' @param x_m The minimum value.
#' @return A numeric vector of length two: the mean (Inf when alpha <= 1) and the
#' standard deviation (Inf when alpha <= 2). NA when the parameters are not positive.
#' @noRd
pareto_moments <- function(alpha, x_m) {
  if (alpha <= 0 || x_m <= 0) return(c(NA_real_, NA_real_))
  mean_value <- if (alpha > 1) alpha * x_m / (alpha - 1) else Inf
  sd_value <- if (alpha > 2) x_m / (alpha - 1) * sqrt(alpha / (alpha - 2)) else Inf
  c(mean_value, sd_value)
}

#' Find missing or non-numeric simulation settings
#'
#' Checks every numeric setting that the chosen options require.
#'
#' @param settings A named list of \code{simulate_function} arguments.
#' @return A character vector naming each setting that is missing or not a number.
#' An empty vector means the settings are complete.
#' @noRd
find_missing_simulation_settings <- function(settings) {
  s <- settings
  problems <- character(0)
  is_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  nth <- function(x, i) if (i <= length(x)) x[[i]] else NULL
  check <- function(value, label) {
    if (!is_number(value)) problems <<- c(problems, label)
  }
  check_params <- function(values, distr, options, group) {
    if (!(is.character(distr) && length(distr) == 1 && distr %in% names(options))) {
      problems <<- c(problems, paste(group, "distribution"))
      return(invisible(NULL))
    }
    labels <- options[[distr]]@param_labels
    whole <- options[[distr]]@param_whole_numbers
    for (i in seq_along(labels)) {
      value <- nth(values, i)
      label <- paste0(group, " parameter '", labels[i], "'")
      if (!is_number(value)) {
        problems <<- c(problems, label)
      } else if (isTRUE(whole[i]) && value != round(value)) {
        #claim counts and Binomial n cannot be fractional
        problems <<- c(problems, paste(label, "must be a whole number"))
      }
    }
  }

  if (!is_number(s$numOfSimulations)) {
    problems <- c(problems, "Number of simulations")
  } else if (s$numOfSimulations != round(s$numOfSimulations) ||
             s$numOfSimulations < 1 || s$numOfSimulations > 10000000) {
    problems <- c(problems, "Number of simulations must be a whole number between 1 and 10,000,000")
  }
  check_params(s$freq_params, s$freqDistr, freq_dist_options, "Frequency")
  check_params(s$sev_params, s$sevDistr, sev_dist_options, "Severity")
  if (isTRUE(s$seedSetBinary)) {
    if (!is_number(s$seedValue)) {
      problems <- c(problems, "Seed value")
    } else if (s$seedValue != round(s$seedValue)) {
      problems <- c(problems, "Seed value must be a whole number")
    }
  }

  #a Normal truncated at zero needs some probability mass above zero to sample from
  if (isTRUE(s$sevTruncateAtZero) && identical(s$sevDistr, "Normal")) {
    sev <- suppressWarnings(as.numeric(unlist(s$sev_params)))
    if (length(sev) == 2 && !anyNA(sev) && pnorm(0, sev[1], sev[2], lower.tail = FALSE) < 1e-9) {
      problems <- c(problems, paste(
        "Severity Normal truncated at zero has almost no probability above zero",
        "(increase the mean or reduce the standard deviation)"
      ))
    }
  }

  if (isTRUE(s$paretoSlice)) {
    if (!is_number(s$pareto_slice_times)) {
      problems <- c(problems, "Number of Pareto Slices")
    } else {
      for (j in seq_len(s$pareto_slice_times)) {
        check(nth(s$slice_pareto_alphas, j), paste("Slice", j, "alpha"))
        check(nth(s$slice_pareto_x_ms, j), paste("Slice", j, "threshold (x_m)"))
      }
      #each slice replaces the tail above its threshold, so thresholds must increase
      x_ms <- utils::head(suppressWarnings(as.numeric(unlist(s$slice_pareto_x_ms))), s$pareto_slice_times)
      if (length(x_ms) == s$pareto_slice_times && length(x_ms) > 1 &&
          !anyNA(x_ms) && any(diff(x_ms) <= 0)) {
        problems <- c(problems, "Slice thresholds must increase from one slice to the next")
      }
    }
  }

  if (isTRUE(s$sevCapBinary)) check(s$sev_cap_amount, "Severity Cap Amount")

  layers_with_deductible <- c('Unlimited Layer', 'Limited Layer', 'Exclude Layer')
  layers_with_limit <- c('Limited Layer', 'Exclude Layer')

  is_structure <- function(x) is.character(x) && length(x) == 1 && x %in% reinsurance_structures_options
  if (!is_structure(s$reinsuranceStructureEEL)) problems <- c(problems, "EEL reinsurance structure")
  if (!is_structure(s$reinsuranceStructureAL)) problems <- c(problems, "AL reinsurance structure")

  if (isTRUE(s$reinsuranceStructureEEL %in% layers_with_deductible)) {
    check(s$reinsurance_structure_eel_dedctible_amount, "EEL Deductible Amount")
  }
  if (isTRUE(s$reinsuranceStructureEEL %in% layers_with_limit)) {
    check(s$reinsurance_structure_eel_limit_amount, "EEL Limit Amount")
  }
  if (isTRUE(s$reinsuranceStructureEEL == 'Limited Layer') &&
      isTRUE(s$reinsuranceStructureLimitedReinstatements)) {
    check(s$reinsuranceStructureReinstatementLimit, "Number of Reinstatements")
  }

  if (isTRUE(s$reinsuranceStructureAL %in% layers_with_deductible)) {
    check(s$reinsurance_structure_al_dedctible_amount, "AL Deductible Amount")
  }
  if (isTRUE(s$reinsuranceStructureAL %in% layers_with_limit)) {
    check(s$reinsurance_structure_al_limit_amount, "AL Limit Amount")
  }

  problems
}

#' A function to simulate frequency - severity of insurance claims using chunked vectorisation.
#' The function applies severity cap, reinsurance structure for each and every loss claim,
#' reinsurance structure for aggregate claims, and allows for piecewise pareto slices.
#'
#' @param numOfSimulations The number of simulations to run.
#' @param freq_params A vector of the frequency distribution parameters.
#' @param sev_params A vector of the severity distribution parameters.
#' @param seedSetBinary True if there is a fixed seed, otherwise false.
#' @param seedValue The seed value.
#' @param freqDistr The frequency distribution. Options are as per the freq_dist_options.
#' @param sevDistr The severity distribution. Options are as per the sev_dist_options.
#' @param paretoSlice True if there is Pareto slicing.
#' @param pareto_slice_times The number of Pareto slices.
#' @param slice_pareto_alphas A vector of Pareto slices' alpha parameters.
#' @param slice_pareto_x_ms A vector of Pareto slices' x_m parameters.
#' @param sevCapBinary True if there is a severity cap.
#' @param sev_cap_amount The severity cap amount.
#' @param reinsuranceStructureEEL The chosen reinsurance structure for each and every loss claim.
#' @param reinsurance_structure_eel_dedctible_amount The deductible for each and every loss reinsurance structure.
#' @param reinsurance_structure_eel_limit_amount The limit for each and every loss reinsurance structure.
#' @param reinsuranceStructureAL The chosen reinsurance structure for aggregate claims.
#' @param reinsurance_structure_al_dedctible_amount The deductible for aggregate reinsurance structure.
#' @param reinsurance_structure_al_limit_amount The limit for aggregate reinsurance structure.
#' @param reinsuranceStructureLimitedReinstatements True if there is a limit in reinstatements, otherwise false.
#' @param reinsuranceStructureReinstatementLimit The reinstatement limit.
#' @param multiprocessing True if multiprocessing is used, otherwise false. An already active multi-worker future plan is reused; otherwise a multisession plan is started for the call and the caller's plan is restored afterwards.
#' @param sevTruncateAtZero True to draw Normal severities from the Normal distribution truncated at zero, so that no claim is negative. Ignored for other severity distributions. Defaults to FALSE.
#' @param chunk_size The number of simulations processed per vectorised batch. Defaults to 10000.
#' @return A data frame with one row per simulation: the claim count, the total claims after the reinsurance structures, the gross total claims before them, and the number of reinstatements used (when reinstatements are limited).
#' Stops with an error that names any required setting that is missing or not a number.
#' @export
#' @examples
#' # 1,000 simulated years of Poisson claim counts with Normal claim sizes, no reinsurance
#' results <- simulate_function(
#'   numOfSimulations = 1000, freq_params = 3, sev_params = c(1000, 200),
#'   seedSetBinary = TRUE, seedValue = 1, freqDistr = "Poisson", sevDistr = "Normal",
#'   reinsuranceStructureEEL = "No Reinsurance Structure",
#'   reinsuranceStructureAL = "No Reinsurance Structure", multiprocessing = FALSE
#' )
#' summary(results$total_claims)
#'
#' # the same claims ceded to a layer of 1,500 excess of 800 on each claim
#' layer <- simulate_function(
#'   numOfSimulations = 1000, freq_params = 3, sev_params = c(1000, 200),
#'   seedSetBinary = TRUE, seedValue = 1, freqDistr = "Poisson", sevDistr = "Normal",
#'   reinsuranceStructureEEL = "Limited Layer",
#'   reinsurance_structure_eel_dedctible_amount = 800,
#'   reinsurance_structure_eel_limit_amount = 1500,
#'   reinsuranceStructureAL = "No Reinsurance Structure", multiprocessing = FALSE
#' )
#' mean(layer$total_claims)
simulate_function <- function(
    numOfSimulations,
    freq_params,
    sev_params,
    seedSetBinary,
    seedValue,
    freqDistr,
    sevDistr,
    paretoSlice,
    pareto_slice_times,
    slice_pareto_alphas,
    slice_pareto_x_ms,
    sevCapBinary,
    sev_cap_amount,
    reinsuranceStructureEEL,
    reinsurance_structure_eel_dedctible_amount,
    reinsurance_structure_eel_limit_amount,
    reinsuranceStructureAL,
    reinsurance_structure_al_dedctible_amount,
    reinsurance_structure_al_limit_amount,
    reinsuranceStructureLimitedReinstatements,
    reinsuranceStructureReinstatementLimit,
    multiprocessing,
    sevTruncateAtZero = FALSE,
    chunk_size = 10000
){
  #collect the settings; arguments not needed for the chosen options may be omitted,
  #and an omitted argument without a default is treated as not set (NULL)
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

  #stop early with a clear message if a required setting is missing
  missing_settings <- find_missing_simulation_settings(settings)
  if (length(missing_settings) > 0) {
    stop("Missing or invalid settings: ", paste(missing_settings, collapse = ", "), call. = FALSE)
  }

  #parameters may be supplied as lists; use plain numeric vectors from here on
  freq_params <- as.numeric(unlist(freq_params))
  sev_params <- as.numeric(unlist(sev_params))
  if (isTRUE(paretoSlice)) {
    slice_pareto_alphas <- as.numeric(unlist(slice_pareto_alphas))
    slice_pareto_x_ms <- as.numeric(unlist(slice_pareto_x_ms))
  }

  #the Normal severity can be truncated at zero so that no claim is negative
  simulate_severities <- sev_dist_options[[sevDistr]]@simulate_func
  if (isTRUE(sevTruncateAtZero) && sevDistr == "Normal") {
    simulate_severities <- function(number_of_simulations, parameters) {
      rnorm_truncated_at_zero(n = number_of_simulations, mean = parameters[1], sd = parameters[2])
    }
  }

  #set custom seed
  if(isTRUE(seedSetBinary)){set.seed(seedValue)}

  n_chunks <- ceiling(numOfSimulations / chunk_size)
  chunk_sizes <- rep(chunk_size, n_chunks)
  remainder <- numOfSimulations - chunk_size * (n_chunks - 1)
  chunk_sizes[n_chunks] <- remainder

  #vectorised worker for a single chunk of simulations
  simulate_chunk <- function(this_n){

    #simulate claim counts for this chunk
    counts <- freq_dist_options[[freqDistr]]@simulate_func(
      number_of_simulations = this_n
      ,parameters = freq_params
    )

    total_claims_needed <- sum(counts)

    #edge case: chunk has zero claims across all simulations
    if(total_claims_needed == 0){
      return(list(claim_counts = counts, total_claims = rep(0, this_n), gross_claims = rep(0, this_n)))
    }

    sim_id <- rep.int(seq_len(this_n), counts)

    #simulate all individual severities for the chunk in one vectorised call
    claims <- simulate_severities(
      number_of_simulations = total_claims_needed
      ,parameters = sev_params
    )

    #apply pareto slices using logical indexing (only redraws claims above threshold)
    if(isTRUE(paretoSlice)){
      for(j in 1:pareto_slice_times){
        idx <- claims > slice_pareto_x_ms[j]
        if(any(idx)){
          claims[idx] <- rpareto(n = sum(idx), alpha = slice_pareto_alphas[j], x_m = slice_pareto_x_ms[j])
        }
      }
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
      ,deductible = reinsurance_structure_eel_dedctible_amount
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

    return(list(claim_counts = counts, total_claims = totals, gross_claims = gross_totals))
  }

  #shiny progress bars and notifications only work inside a running shiny app
  in_shiny_session <- !is.null(shiny::getDefaultReactiveDomain())

  run_chunks_sequentially <- function(on_chunk_done = function(i) NULL) {
    res <- vector("list", length(chunk_sizes))
    for (i in seq_along(chunk_sizes)) {
      res[[i]] <- simulate_chunk(chunk_sizes[i])
      on_chunk_done(i)
    }
    res
  }

  #run chunks, optionally in parallel across chunks (not per-simulation)
  if (isTRUE(multiprocessing)) {
    #reuse workers that are already running (e.g. the app keeps a warm multisession plan);
    #otherwise start a plan for this call and restore the caller's plan afterwards
    if (future::nbrOfWorkers() <= 1) {
      old_plan <- future::plan(future::multisession)
      on.exit(future::plan(old_plan), add = TRUE)
    }

    if (in_shiny_session) {
      shiny::showNotification(
        "Running in parallel. Live progress is not available in multiprocessing mode.",
        type = "message",
        duration = NULL,
        id = "parallel_sim_notice"
      )
      on.exit(shiny::removeNotification("parallel_sim_notice"), add = TRUE)
    }

    chunk_results <- future.apply::future_lapply(
      chunk_sizes,
      simulate_chunk,
      future.seed = TRUE
    )

  } else if (in_shiny_session) {
    chunk_results <- shiny::withProgress(
      message = "Running simulations",
      detail = paste("Processing", n_chunks, "chunks"),
      value = 0,
      run_chunks_sequentially(function(i) {
        shiny::incProgress(
          amount = 1 / length(chunk_sizes),
          detail = paste("Chunk", i, "of", length(chunk_sizes))
        )
      })
    )

  } else {
    chunk_results <- run_chunks_sequentially()
  }

  data <- data.frame(
    claim_counts = unlist(lapply(chunk_results, `[[`, "claim_counts"), use.names = FALSE)
    ,total_claims = unlist(lapply(chunk_results, `[[`, "total_claims"), use.names = FALSE)
    ,gross_claims = round(unlist(lapply(chunk_results, `[[`, "gross_claims"), use.names = FALSE), 2)
  )
  rm(chunk_results); gc(FALSE)

  #apply reinstatements
  if(reinsuranceStructureEEL %in% c('Limited Layer')){
    if(isTRUE(reinsuranceStructureLimitedReinstatements)){
      data$total_claims <- apply_deductible_limit(
        data$total_claims
        ,'Limited Layer'
        ,0
        ,(reinsuranceStructureReinstatementLimit + 1) * reinsurance_structure_eel_limit_amount
      )
      data$number_of_reinstatements_used <- (data$total_claims / reinsurance_structure_eel_limit_amount)
      data$number_of_reinstatements_used <- ifelse(
        data$number_of_reinstatements_used > reinsuranceStructureReinstatementLimit
        ,reinsuranceStructureReinstatementLimit
        ,data$number_of_reinstatements_used
      )
      data$number_of_reinstatements_used <- round(data$number_of_reinstatements_used, 2)
    }
  }

  #apply reinsurance structure AL
  data$total_claims <- apply_deductible_limit(
    data$total_claims
    ,reinsuranceStructureAL
    ,reinsurance_structure_al_dedctible_amount
    ,reinsurance_structure_al_limit_amount
  )
  data$total_claims <- round(data$total_claims, 2)
  return(data)
}

#' A function to run the shiny simulator application
#'
#' @return Opens the shiny simulator application
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_simulator()
#' }
run_shiny_simulator <- function() {
  shinyApp(ui = shiny_simulator_ui, server = shiny_simulator_server)
}
