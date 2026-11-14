##############
#The claim count and claim size distributions of the simulations
##############

# The distributions that simulate_claims(), simulate_function() and the simulator offer, with
# their parameters and moments, and the Pareto tails spliced onto a claim size distribution.

#' Parameter to set the maximum number of Pareto slices
#'
#' The largest number of Pareto slices that the simulator app and
#' \code{\link{simulate_claims}} accept, so that the app can build one row of slice inputs
#' for each.
#'
#' @return The maximum number of Pareto Slices.
#' @keywords internal
max_number_of_pareto_slices <- 6

#' Random Pareto generator with a minimum value
#'
#' Pareto (type I) with shape \code{alpha} and minimum \code{x_m}: P(X > x) = (x_m / x)^alpha
#' for x >= x_m. This is not the Lomax parameterisation some packages call "Pareto".
#'
#' @param n Number of values to generate.
#' @param alpha A positive real number. Alpha parameter of the Pareto distribution.
#' @param x_m A positive real number. The minimum value for the Pareto distribution.
#' @return A vector of \code{n} random Pareto variables with parameters \code{alpha} and \code{x_m}.
#' @noRd
rpareto_xm <- function(n, alpha, x_m) x_m / stats::runif(n)^(1/alpha)

#' Random Normal generator truncated at zero
#'
#' Draws from the Normal distribution conditional on being positive, by inverse-CDF
#' sampling on the upper tail (s ~ U(0, P(X > 0)), x = the upper-tail quantile of s),
#' which stays accurate when only a little of the distribution lies above zero.
#'
#' @param n Number of values to generate.
#' @param mean The mean of the underlying Normal distribution.
#' @param sd The standard deviation of the underlying Normal distribution.
#' @return A vector of \code{n} positive random values.
#' @noRd
rnorm_truncated_at_zero <- function(n, mean, sd) {
  above_zero <- stats::pnorm(0, mean = mean, sd = sd, lower.tail = FALSE)
  stats::qnorm(stats::runif(n, min = 0, max = above_zero), mean = mean, sd = sd, lower.tail = FALSE)
}

#' A function slot that may be empty
#'
#' @noRd
setClassUnion("functionOrNULL", c("function", "NULL"))

#' The class of the distribution objects
#'
#' Each object describes one frequency or severity distribution of the simulator:
#' its parameters (ids used as app input ids, labels, allowed ranges and whether they
#' must be whole numbers) and the functions the simulator needs. \code{simulate_func}
#' draws values; \code{moments_func} gives the mean and standard deviation. Severity
#' distributions also have \code{survival_func} and \code{tail_quantile_func} (the
#' upper-tail probability and its inverse), used to splice Pareto tails and to draw only
#' the claims that reach a layer, and optionally \code{sum_func}, which draws the total
#' of a given number of claims in one step. Frequency distributions may have
#' \code{split_func}, which draws the number of claims and how many of them are large.
#' Allowed ranges are \code{param_min_values} and \code{param_max_values} (NA for no
#' bound), with \code{param_min_strict} and \code{param_max_strict} marking bounds the
#' value may not equal. \code{param_display_order} is the order the app shows the
#' parameters' inputs in, as positions in \code{paramIDs} (empty for that order): the
#' Pareto shows its minimum, x_m, before alpha, the order actuaries enter them in, while
#' its parameters stay \code{c(alpha, x_m)} wherever they are passed as a vector.
#' \code{param_amounts} marks the parameters that are amounts of money (empty for none),
#' which the app shows with thousands separators.
#' @keywords internal
distributionClass <- setClass(
  "distributionClass",
  slots = c(
    distrID = "character"
    ,distr_label = "character"
    ,paramIDs = "character"
    ,param_labels = "character"
    ,param_min_values = "numeric"
    ,param_max_values = "numeric"
    ,param_min_strict = "logical"
    ,param_max_strict = "logical"
    ,param_whole_numbers = "logical"
    ,param_display_order = "integer"
    ,param_amounts = "logical"
    ,simulate_func = "function"
    ,moments_func = "function"
    ,survival_func = "functionOrNULL"
    ,tail_quantile_func = "functionOrNULL"
    ,sum_func = "functionOrNULL"
    ,split_func = "functionOrNULL"
  ),
  prototype = list(
    param_min_values = numeric(0)
    ,param_max_values = numeric(0)
    ,param_min_strict = logical(0)
    ,param_max_strict = logical(0)
    ,param_whole_numbers = logical(0)
    ,param_display_order = integer(0)
    ,param_amounts = logical(0)
    ,survival_func = NULL
    ,tail_quantile_func = NULL
    ,sum_func = NULL
    ,split_func = NULL
  ),
  validity = function(object) {
    problems <- character(0)
    n <- length(object@paramIDs)
    if (length(object@distrID) != 1 || length(object@distr_label) != 1) {
      problems <- c(problems, "distrID and distr_label must be single strings")
    }
    if (length(object@param_labels) != n) problems <- c(problems, "param_labels must have one label per parameter")
    for (slot_name in c("param_min_values", "param_max_values", "param_min_strict", "param_max_strict", "param_whole_numbers",
                        "param_amounts")) {
      if (!(length(methods::slot(object, slot_name)) %in% c(0, n))) {
        problems <- c(problems, paste(slot_name, "must be empty or have one value per parameter"))
      }
    }
    order <- object@param_display_order
    if (length(order) && !identical(sort(order), seq_len(n))) {
      problems <- c(problems, "param_display_order must be empty or list each parameter's position once")
    }
    if (is.null(object@survival_func) != is.null(object@tail_quantile_func)) {
      problems <- c(problems, "survival_func and tail_quantile_func must be given together")
    }
    if (length(problems)) problems else TRUE
  }
)

#' Positions of a distribution's parameters in the order they are shown
#'
#' The app's inputs, the report and the Compare tab show a distribution's parameters in
#' \code{param_display_order} when it is set (the Pareto shows x_m before alpha), and in
#' \code{paramIDs} order otherwise.
#'
#' @param object A distribution object.
#' @return Positions in \code{paramIDs}, first the one shown first.
#' @noRd
param_display_positions <- function(object) {
  order <- object@param_display_order
  if (length(order)) order else seq_along(object@paramIDs)
}

#' A vector with the frequency distribution objects
#'
#' A named list of \code{distributionClass} objects, one for each claim count distribution
#' of the simulator: \code{Poisson}, \code{Negative_Binomial}, \code{Binomial},
#' \code{Zero_Inflated_Poisson}, \code{Zero_Inflated_Negative_Binomial} and
#' \code{Fixed_number_of_Counts}. The names are the values accepted by the \code{freqDistr} argument of
#' \code{simulate_function()}, and the objects hold the parameter ids, labels and ranges
#' of the app's inputs and the functions that draw the claim counts. A zero-inflated
#' count is zero with the probability of its last parameter, and otherwise drawn from the
#' Poisson or Negative Binomial of its other parameters: more periods without a claim than
#' those distributions give on their own.
#'
#' @return The frequency distribution objects.
#' @keywords internal
freq_dist_options <- c(
  Poisson=distributionClass(
    distrID='Poisson'
    ,distr_label='Poisson'
    ,paramIDs=c("lamda")
    ,param_labels=c("lambda (mean claims)")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rpois(n = number_of_simulations, lambda = parameters[1])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1], sqrt(p[1]))
    #a Poisson count splits into independent Poisson counts of large and small claims
    ,split_func = function(n, p, large_prob) {
      large <- stats::rpois(n, p[1] * large_prob)
      list(total = large + stats::rpois(n, p[1] * (1 - large_prob)), large = large)
    }
  )
  ,Negative_Binomial=distributionClass(
    distrID='Negative_Binomial'
    ,distr_label='Negative Binomial'
    ,paramIDs=c("r", "beta")
    ,param_labels=c("r (shape)", "beta (scale)")
    ,param_min_values=c(0, 0)
    ,param_min_strict=c(TRUE, TRUE)
    #Poisson-Gamma mixture with Gamma shape r and scale beta
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rpois(n = number_of_simulations, lambda = stats::rgamma(n = number_of_simulations, shape = parameters[1], scale = parameters[2]))
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1] * p[2], sqrt(p[1] * p[2] * (1 + p[2])))
    #given the Gamma rate, large and small claims are independent Poisson counts
    ,split_func = function(n, p, large_prob) {
      rate <- stats::rgamma(n, shape = p[1], scale = p[2])
      large <- stats::rpois(n, rate * large_prob)
      list(total = large + stats::rpois(n, rate * (1 - large_prob)), large = large)
    }
  )
  ,Binomial=distributionClass(
    distrID='Binomial'
    ,distr_label='Binomial'
    ,paramIDs=c("n", "p")
    ,param_labels=c("n (number of trials)", "p (probability)")
    ,param_min_values=c(0, 0)
    ,param_max_values=c(NA, 1)
    ,param_whole_numbers=c(TRUE, FALSE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rbinom(n = number_of_simulations, size = parameters[1], prob = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1] * p[2], sqrt(p[1] * p[2] * (1 - p[2])))
    #each trial gives a large claim, a small claim or none (a multinomial split)
    ,split_func = function(n, p, large_prob) {
      large_trial_prob <- p[2] * large_prob
      large <- stats::rbinom(n, size = p[1], prob = large_trial_prob)
      small_prob <- if (large_trial_prob < 1) p[2] * (1 - large_prob) / (1 - large_trial_prob) else 0
      list(total = large + stats::rbinom(n, size = p[1] - large, prob = small_prob), large = large)
    }
  )
  ,Zero_Inflated_Poisson=distributionClass(
    distrID='Zero_Inflated_Poisson'
    ,distr_label='Zero-inflated Poisson'
    ,paramIDs=c("zip_lambda", "zip_zero")
    ,param_labels=c("lambda (mean of the Poisson)", "Probability of an extra zero")
    ,param_min_values=c(0, 0)
    ,param_max_values=c(NA, 1)
    ,param_max_strict=c(FALSE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rbinom(number_of_simulations, 1, 1 - parameters[2]) * stats::rpois(n = number_of_simulations, lambda = parameters[1])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) zero_inflated_moments(p[1], p[1], p[2])
    #a period with claims splits as the Poisson does; a period of extra zeros has neither
    ,split_func = function(n, p, large_prob) {
      any_claims <- stats::rbinom(n, 1, 1 - p[2])
      large <- any_claims * stats::rpois(n, p[1] * large_prob)
      list(total = large + any_claims * stats::rpois(n, p[1] * (1 - large_prob)), large = large)
    }
  )
  ,Zero_Inflated_Negative_Binomial=distributionClass(
    distrID='Zero_Inflated_Negative_Binomial'
    ,distr_label='Zero-inflated Negative Binomial'
    ,paramIDs=c("zinb_r", "zinb_beta", "zinb_zero")
    ,param_labels=c("r (shape)", "beta (scale)", "Probability of an extra zero")
    ,param_min_values=c(0, 0, 0)
    ,param_max_values=c(NA, NA, 1)
    ,param_min_strict=c(TRUE, TRUE, FALSE)
    ,param_max_strict=c(FALSE, FALSE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rbinom(number_of_simulations, 1, 1 - parameters[3]) *
        stats::rpois(n = number_of_simulations, lambda = stats::rgamma(n = number_of_simulations, shape = parameters[1], scale = parameters[2]))
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) {
      zero_inflated_moments(p[1] * p[2], p[1] * p[2] * (1 + p[2]), p[3])
    }
    ,split_func = function(n, p, large_prob) {
      rate <- stats::rbinom(n, 1, 1 - p[3]) * stats::rgamma(n, shape = p[1], scale = p[2])
      large <- stats::rpois(n, rate * large_prob)
      list(total = large + stats::rpois(n, rate * (1 - large_prob)), large = large)
    }
  )
  ,Fixed_number_of_Counts=distributionClass(
    distrID='Fixed_number_of_Counts'
    ,distr_label='Fixed number of Counts'
    ,paramIDs=c("FixedNumberOfCounts")
    ,param_labels=c("Number of claims")
    ,param_min_values=c(0)
    ,param_whole_numbers=c(TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      rep(parameters[1], number_of_simulations)
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1], 0)
    ,split_func = function(n, p, large_prob) {
      list(total = rep(p[1], n), large = stats::rbinom(n, size = p[1], prob = large_prob))
    }
  )
)

#' Mean and standard deviation of a zero-inflated count
#'
#' A count that is zero with probability \code{zero_prob} and otherwise drawn from a
#' distribution with the given mean and variance has mean \code{(1 - zero_prob) * mean}
#' and variance \code{(1 - zero_prob) * (variance + zero_prob * mean^2)}.
#'
#' @param mean,variance The mean and variance of the distribution without the extra zeros.
#' @param zero_prob The probability of an extra zero.
#' @return A numeric vector of length two: the mean and the standard deviation.
#' @noRd
zero_inflated_moments <- function(mean, variance, zero_prob) {
  c((1 - zero_prob) * mean, sqrt((1 - zero_prob) * (variance + zero_prob * mean^2)))
}

#' A data frame with the frequency distribution parameter placeholders
#'
#' One row per parameter of the frequency distribution with the most parameters, giving
#' the number and output id of the placeholder in which the simulator app renders that
#' parameter's input for the chosen distribution.
#'
#' @return The frequency distribution parameter placeholders.
#' @keywords internal
freq_dist_parameter_placeholders <- data.frame(
  param_number = seq_len(max(vapply(freq_dist_options, function(x) length(x@paramIDs), integer(1))))
  ,param_id = paste0("freq_param_", seq_len(max(vapply(freq_dist_options, function(x) length(x@paramIDs), integer(1)))))
)

#' A vector with the severity distribution objects
#'
#' A named list of \code{distributionClass} objects, one for each severity distribution
#' of the simulator: Normal, LogNormal, Gamma, Exponential, Pareto, Weibull, Burr,
#' Generalised_Pareto and Fixed_Severity.
#' The names are the values accepted by the \code{sevDistr} argument of
#' \code{simulate_function()}. The Normal distribution has its own parameter ids
#' (normal_mean, normal_sd), so that switching between the Normal and the Log-Normal in
#' the app does not carry values across, and so have the Weibull, the Burr and the
#' Generalised Pareto.
#'
#' The Burr (type XII) has P(X > x) = (1 + (x / scale)^gamma)^(-alpha) for x > 0: a
#' Pareto tail of index alpha * gamma over a body shaped by gamma (alpha = 1 is the
#' log-logistic, gamma = 1 the Lomax). The Generalised Pareto has
#' P(X > x) = (1 + shape * (x - location) / scale)^(-1 / shape) for x above its location:
#' a Pareto tail of index 1 / shape for a positive shape, the Exponential for shape 0 and
#' a tail that ends at location - scale / shape for a negative one.
#'
#' @return The severity distribution objects.
#' @keywords internal
sev_dist_options <- c(
  Normal=distributionClass(
    distrID='Normal'
    ,distr_label='Normal'
    ,paramIDs=c("normal_mean", "normal_sd")
    ,param_labels=c("Mean", "Standard deviation")
    ,param_min_values=c(NA, 0)
    ,param_amounts=c(TRUE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rnorm(n = number_of_simulations, mean = parameters[1], sd = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) {
      if (isTRUE(truncate_at_zero)) truncated_normal_moments(p[1], p[2]) else c(p[1], p[2])
    }
    ,survival_func = function(x, p) stats::pnorm(x, mean = p[1], sd = p[2], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qnorm(s, mean = p[1], sd = p[2], lower.tail = FALSE)
    #the sum of n Normal claims is Normal with mean n * mean and variance n * sd^2; where
    #n * mean or sqrt(n) * sd overflows, rnorm() would give NaN, so those simulations sum
    #their claims one by one instead, which gives the infinite totals the claims add up to
    ,sum_func = function(counts, p) {
      totals_mean <- counts * p[1]
      totals_sd <- sqrt(counts) * p[2]
      in_range <- is.finite(totals_mean) & is.finite(totals_sd)
      totals <- numeric(length(counts))
      totals[in_range] <- stats::rnorm(sum(in_range), mean = totals_mean[in_range], sd = totals_sd[in_range])
      overflow <- which(!in_range & counts > 0)
      if (length(overflow) > 0) {
        claims <- stats::rnorm(sum(counts[overflow]), mean = p[1], sd = p[2])
        totals[overflow] <- rowsum(claims, rep.int(seq_along(overflow), counts[overflow]), reorder = FALSE)[, 1]
      }
      totals
    }
  )
  ,LogNormal=distributionClass(
    distrID='LogNormal'
    ,distr_label='Log-Normal'
    ,paramIDs=c("mu", "sigma")
    ,param_labels=c("mu (mean of log)", "sigma (sd of log)")
    ,param_min_values=c(NA, 0)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rlnorm(n = number_of_simulations, meanlog = parameters[1], sdlog = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) {
      c(exp(p[1] + p[2]^2 / 2), sqrt(exp(2 * p[1] + p[2]^2) * (exp(p[2]^2) - 1)))
    }
    ,survival_func = function(x, p) stats::plnorm(x, meanlog = p[1], sdlog = p[2], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qlnorm(s, meanlog = p[1], sdlog = p[2], lower.tail = FALSE)
  )
  ,Gamma=distributionClass(
    distrID='Gamma'
    ,distr_label='Gamma'
    ,paramIDs=c("shape", "scale")
    ,param_labels=c("Shape", "Scale")
    ,param_min_values=c(0, 0)
    ,param_min_strict=c(TRUE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rgamma(n = number_of_simulations, shape = parameters[1], scale = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1] * p[2], sqrt(p[1]) * p[2])
    ,survival_func = function(x, p) stats::pgamma(x, shape = p[1], scale = p[2], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qgamma(s, shape = p[1], scale = p[2], lower.tail = FALSE)
    #the sum of n Gamma(shape, scale) claims is Gamma(n * shape, scale)
    ,sum_func = function(counts, p) {
      totals <- numeric(length(counts))
      any_claims <- counts > 0
      totals[any_claims] <- stats::rgamma(sum(any_claims), shape = counts[any_claims] * p[1], scale = p[2])
      totals
    }
  )
  ,Exponential=distributionClass(
    distrID='Exponential'
    ,distr_label='Exponential'
    ,paramIDs=c("rate")
    ,param_labels=c("Rate")
    ,param_min_values=c(0)
    ,param_min_strict=c(TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rexp(n = number_of_simulations, rate = parameters[1])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(1 / p[1], 1 / p[1])
    ,survival_func = function(x, p) stats::pexp(x, rate = p[1], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qexp(s, rate = p[1], lower.tail = FALSE)
    #the sum of n Exponential(rate) claims is Gamma(n, 1 / rate)
    ,sum_func = function(counts, p) {
      totals <- numeric(length(counts))
      any_claims <- counts > 0
      totals[any_claims] <- stats::rgamma(sum(any_claims), shape = counts[any_claims], scale = 1 / p[1])
      totals
    }
  )
  ,Pareto=distributionClass(
    distrID='Pareto'
    ,distr_label='Pareto'
    ,paramIDs=c("alpha", "x_m")
    ,param_labels=c("alpha (shape)", "x_m (minimum)")
    ,param_min_values=c(0, 0)
    ,param_min_strict=c(TRUE, TRUE)
    ,param_display_order=c(2L, 1L)
    ,param_amounts=c(FALSE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      rpareto_xm(n = number_of_simulations, alpha = parameters[1], x_m = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) pareto_moments(p[1], p[2])
    ,survival_func = function(x, p) (p[2] / pmax(x, p[2]))^p[1]
    ,tail_quantile_func = function(s, p) p[2] * s^(-1 / p[1])
  )
  ,Weibull=distributionClass(
    distrID='Weibull'
    ,distr_label='Weibull'
    ,paramIDs=c("weibull_shape", "weibull_scale")
    ,param_labels=c("Shape", "Scale")
    ,param_min_values=c(0, 0)
    ,param_min_strict=c(TRUE, TRUE)
    ,param_amounts=c(FALSE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      stats::rweibull(n = number_of_simulations, shape = parameters[1], scale = parameters[2])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) weibull_moments(p[1], p[2])
    ,survival_func = function(x, p) stats::pweibull(x, shape = p[1], scale = p[2], lower.tail = FALSE)
    ,tail_quantile_func = function(s, p) stats::qweibull(s, shape = p[1], scale = p[2], lower.tail = FALSE)
  )
  ,Burr=distributionClass(
    distrID='Burr'
    ,distr_label='Burr'
    ,paramIDs=c("burr_alpha", "burr_gamma", "burr_scale")
    ,param_labels=c("alpha (first shape)", "gamma (second shape)", "Scale")
    ,param_min_values=c(0, 0, 0)
    ,param_min_strict=c(TRUE, TRUE, TRUE)
    ,param_amounts=c(FALSE, FALSE, TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      burr_tail_quantile(stats::runif(number_of_simulations), parameters[1], parameters[2], parameters[3])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) burr_moments(p[1], p[2], p[3])
    ,survival_func = function(x, p) exp(burr_log_survival(x, p[1], p[2], p[3]))
    ,tail_quantile_func = function(s, p) burr_tail_quantile(s, p[1], p[2], p[3])
  )
  ,Generalised_Pareto=distributionClass(
    distrID='Generalised_Pareto'
    ,distr_label='Generalised Pareto'
    ,paramIDs=c("gpd_location", "gpd_scale", "gpd_shape")
    ,param_labels=c("Location (threshold)", "Scale", "Shape (xi)")
    ,param_min_values=c(0, 0, NA)
    ,param_min_strict=c(FALSE, TRUE, FALSE)
    ,param_amounts=c(TRUE, TRUE, FALSE)
    ,simulate_func = function(number_of_simulations, parameters){
      gpd_tail_quantile(stats::runif(number_of_simulations), parameters[1], parameters[2], parameters[3])
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) gpd_moments(p[1], p[2], p[3])
    ,survival_func = function(x, p) exp(gpd_log_survival(x, p[1], p[2], p[3]))
    ,tail_quantile_func = function(s, p) gpd_tail_quantile(s, p[1], p[2], p[3])
  )
  ,Fixed_Severity=distributionClass(
    distrID='Fixed_Severity'
    ,distr_label='Fixed Severity'
    ,paramIDs=c("Fixed_sev_amount")
    ,param_labels=c("Claim amount")
    ,param_min_values=c(0)
    ,param_amounts=c(TRUE)
    ,simulate_func = function(number_of_simulations, parameters){
      rep(parameters[1], number_of_simulations)
    }
    ,moments_func = function(p, truncate_at_zero = FALSE) c(p[1], 0)
    ,survival_func = function(x, p) as.numeric(x < p[1])
    ,tail_quantile_func = function(s, p) rep(p[1], length(s))
    ,sum_func = function(counts, p) counts * p[1]
  )
)

#' A data frame with the severity distribution parameter placeholders
#'
#' One row per parameter of the severity distribution with the most parameters, giving
#' the number and output id of the placeholder in which the simulator app renders that
#' parameter's input for the chosen distribution.
#'
#' @return The severity distribution parameter placeholders.
#' @keywords internal
sev_dist_parameter_placeholders <- data.frame(
  param_number = seq_len(max(vapply(sev_dist_options, function(x) length(x@paramIDs), integer(1))))
  ,param_id = paste0("sev_param_", seq_len(max(vapply(sev_dist_options, function(x) length(x@paramIDs), integer(1)))))
)

#' Problems with a distribution's parameter values
#'
#' Checks that each parameter is a number, a whole number where required, and within its
#' allowed range.
#'
#' @param object A distribution object.
#' @param values A numeric vector (or list) of parameters in \code{paramIDs} order.
#' @return A character vector of problems, each starting with "parameter '<label>'";
#' empty when the values are valid.
#' @noRd
distribution_param_problems <- function(object, values) {
  pick <- function(x, i, default) if (i <= length(x)) x[[i]] else default
  labels <- object@param_labels
  problems <- character(0)
  for (i in seq_along(labels)) {
    value <- pick(values, i, NULL)
    label <- paste0("parameter '", labels[i], "'")
    if (!(is.numeric(value) && length(value) == 1 && !is.na(value))) {
      problems <- c(problems, label)
      next
    }
    if (!is.finite(value)) {
      #an infinite parameter makes the draws fail or gives infinite or NaN totals
      problems <- c(problems, paste(label, "must be finite"))
      next
    }
    if (isTRUE(pick(object@param_whole_numbers, i, FALSE)) && value != round(value)) {
      #claim counts and Binomial n cannot be fractional
      problems <- c(problems, paste(label, "must be a whole number"))
    }
    low <- pick(object@param_min_values, i, NA)
    low_strict <- isTRUE(pick(object@param_min_strict, i, FALSE))
    if (!is.na(low) && (value < low || (low_strict && value == low))) {
      problems <- c(problems, paste(label, if (low_strict) "must be greater than" else "must be at least", format(low)))
    }
    high <- pick(object@param_max_values, i, NA)
    high_strict <- isTRUE(pick(object@param_max_strict, i, FALSE))
    if (!is.na(high) && (value > high || (high_strict && value == high))) {
      problems <- c(problems, paste(label, if (high_strict) "must be less than" else "must be at most", format(high)))
    }
  }
  problems
}

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
  if (length(distribution_param_problems(object, p)) > 0) return(unavailable)

  moments <- as.numeric(object@moments_func(p, truncate_at_zero))
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
  tail <- stats::pnorm(alpha, lower.tail = FALSE)
  if (tail < 1e-9) return(c(NA_real_, NA_real_))
  lambda <- stats::dnorm(alpha) / tail
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

#' The severity distribution used for a run
#'
#' Wraps a severity distribution object and its parameters, applying the truncation of
#' the Normal at zero when asked, into three functions: \code{draw(n)}, \code{survival(x)}
#' (P(X > x)) and \code{tail_quantile(s)} (the x with P(X > x) = s).
#'
#' @param object A severity distribution object.
#' @param params Numeric parameters in \code{paramIDs} order.
#' @param truncate_at_zero TRUE to truncate the Normal at zero.
#' @return A list with the functions \code{draw}, \code{survival} and \code{tail_quantile}.
#' @noRd
severity_model <- function(object, params, truncate_at_zero = FALSE) {
  if (isTRUE(truncate_at_zero) && identical(object@distrID, "Normal")) {
    mu <- params[1]
    sd <- params[2]
    above_zero <- stats::pnorm(0, mean = mu, sd = sd, lower.tail = FALSE)
    return(list(
      draw = function(n) rnorm_truncated_at_zero(n, mean = mu, sd = sd)
      ,survival = function(x) pmin(1, stats::pnorm(pmax(x, 0), mean = mu, sd = sd, lower.tail = FALSE) / above_zero)
      ,tail_quantile = function(s) stats::qnorm(s * above_zero, mean = mu, sd = sd, lower.tail = FALSE)
    ))
  }
  list(
    draw = function(n) object@simulate_func(n, params)
    ,survival = function(x) object@survival_func(x, params)
    ,tail_quantile = function(s) object@tail_quantile_func(s, params)
  )
}

#' Splice piecewise Pareto tails onto a severity distribution
#'
#' Above the first threshold the severity follows a Pareto with the first alpha, above the
#' second threshold a Pareto with the second alpha, and so on. This is the distribution the
#' simulator has always produced by redrawing claims above each threshold in turn; here the
#' claims above the first threshold are drawn from it in one step.
#'
#' @param model The base severity model from \code{severity_model()}.
#' @param thresholds Increasing slice thresholds (x_m values).
#' @param alphas The slice alphas.
#' @return A list with \code{first_threshold}, \code{draw_above_first(n)} (draws from the
#' spliced distribution conditional on exceeding the first threshold), and
#' \code{survival(x)} and \code{tail_quantile(s)} of the spliced distribution.
#' @noRd
pareto_splice <- function(model, thresholds, alphas) {
  k <- length(thresholds)
  #probability of exceeding each threshold
  survival_at <- numeric(k)
  survival_at[1] <- model$survival(thresholds[1])
  if (k > 1) {
    for (j in 2:k) survival_at[j] <- survival_at[j - 1] * (thresholds[j - 1] / thresholds[j])^alphas[j - 1]
  }

  #upper-tail quantile for probabilities at or below the first threshold's exceedance
  pareto_part_quantile <- function(s) {
    j <- k - findInterval(s, rev(survival_at), left.open = TRUE)
    thresholds[j] * (survival_at[j] / s)^(1 / alphas[j])
  }

  list(
    first_threshold = thresholds[1]
    ,draw_above_first = function(n) pareto_part_quantile(stats::runif(n, min = 0, max = survival_at[1]))
    ,survival = function(x) {
      out <- model$survival(x)
      j <- findInterval(x, thresholds, left.open = TRUE)
      above <- j > 0
      out[above] <- survival_at[j[above]] * (thresholds[j[above]] / x[above])^alphas[j[above]]
      out
    }
    ,tail_quantile = function(s) {
      out <- numeric(length(s))
      in_base <- s >= survival_at[1]
      out[in_base] <- model$tail_quantile(s[in_base])
      out[!in_base] <- pareto_part_quantile(s[!in_base])
      out
    }
  )
}
