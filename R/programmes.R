##############
#Comparing reinsurance programmes
##############

#' Compare reinsurance programmes
#'
#' @description Simulates the claims of a portfolio once and puts them through each of
#'   several reinsurance programmes (a quota share, an excess of loss layer on each
#'   claim and an aggregate stop loss, alone or together), to compare what each costs
#'   with the capital it saves: the expected ceded losses, the reinsurance premiums, the
#'   margin paid to the reinsurers, the net result, its value at risk and the capital it
#'   needs.
#'
#' @details Every programme sees the same simulated claims. In each, the quota share
#'   cedes its share of every claim first; the excess of loss layer then recovers from
#'   each retained claim the part above its deductible, up to its limit, and at most
#'   \eqn{(1 + \textrm{reinstatements})} limits a year; the stop loss last recovers the
#'   part of the year's retained claims above its attachment, up to its limit.
#'
#'   The premium of a quota share is its share of the subject premium, less its ceding
#'   commission. A layer's or stop loss's premium is given or, when it is not, priced at
#'   the expected ceded loss with \code{loading} as the share of the premium for the
#'   reinsurer's expenses and profit (\code{\link{technical_premium}}, with the
#'   reinstatements the simulations use). The excess of loss layer's reinstatement
#'   premiums are paid pro rata to the cover reinstated.
#'
#'   The net result of a year is the subject premium less the reinsurance premiums paid,
#'   plus the ceding commission, less the retained claims (before other expenses). The
#'   capital a programme needs is the mean net result less its \eqn{1 - \textrm{level}}
#'   quantile: what a bad year at that level costs against an average one. The capital
#'   relief is the capital without reinsurance less the programme's, and its value is
#'   \code{coc_rate} of the relief less the margin paid for it: positive when the
#'   programme saves more in the cost of capital than it costs.
#' @param frequency,frequency_params The claim count distribution and its parameters,
#'   as in \code{\link{simulate_claims}}, such as "Poisson" (lambda) or "Negative
#'   Binomial" (r, beta).
#' @param severity,severity_params The claim size distribution and its parameters, as
#'   in \code{\link{simulate_claims}}, such as "LogNormal" (meanlog, sdlog) or "Pareto"
#'   (alpha, x_m).
#' @param programmes A named list of programmes, each a list with any of:
#'   \code{quota_share} (the share ceded) and \code{commission} (the ceding commission,
#'   a share of the ceded premium); \code{xl}, a list with the \code{deductible},
#'   \code{limit}, \code{reinstatements} (\code{Inf}, the default, for unlimited and free),
#'   \code{reinstatement_rate} (1 by default) and optionally \code{premium}; and
#'   \code{stop_loss}, a list with the \code{attachment}, \code{limit} and optionally
#'   \code{premium}. "No reinsurance" is added as the first programme.
#' @param premium The subject premium of the year.
#' @param n The number of simulated years.
#' @param level The level of the value at risk, such as 0.995.
#' @param loading The share of a priced layer's or stop loss's premium for the
#'   reinsurer's expenses and profit.
#' @param coc_rate The cost of capital a year, for the value of the capital relief.
#' @param seed A whole number for a reproducible run, or \code{NULL}. The stream of the
#'   R session is restored after a run with a seed.
#' @return An object of class \code{netsimr_programmes}, a list with \code{summary}, a
#'   data frame with a row per programme (the expected ceded loss, the reinsurance
#'   premium and margin, the net mean result and its standard deviation, the
#'   probability of a loss-making year, the value at risk of the net result, the
#'   capital, the capital relief and its value), \code{results}, the simulated net
#'   result of each programme (a column each), and the settings.
#' @seealso \code{\link{simulate_claims}}, \code{\link{technical_premium}} and
#'   \code{\link{run_shiny_reinsurance_pricing_tool}}, which compares programmes on its
#'   Programmes tab.
#' @export
#' @examples
#' programmes <- list(
#'   `Quota share 30%` = list(quota_share = 0.3, commission = 0.25),
#'   `XL 4m xs 1m` = list(xl = list(deductible = 1e6, limit = 4e6, reinstatements = 1)),
#'   `Stop loss 30m xs 45m` = list(stop_loss = list(attachment = 45e6, limit = 30e6))
#' )
#' cmp <- compare_reinsurance("Poisson", 200, "LogNormal", c(11, 1.5), programmes,
#'                            premium = 60e6, n = 5000, seed = 1)
#' cmp
compare_reinsurance <- function(frequency, frequency_params, severity, severity_params, programmes, premium, n = 10000,
                                level = 0.995, loading = 0.25, coc_rate = 0.1, seed = NULL) {
  if (!is.list(programmes) || (length(programmes) > 0 && (is.null(names(programmes)) || any(!nzchar(names(programmes)))))) {
    stop("programmes must be a named list of programmes.", call. = FALSE)
  }
  if (anyDuplicated(c("No reinsurance", names(programmes)))) stop("Each programme needs its own name.", call. = FALSE)
  if (!is.numeric(premium) || length(premium) != 1 || !is.finite(premium) || premium < 0) {
    stop("premium must be the subject premium, at least 0.", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1 || !is.finite(n) || n < 100 || n != round(n)) stop("n must be a whole number of at least 100.", call. = FALSE)
  if (!is.numeric(level) || length(level) != 1 || !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be a probability between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(loading) || length(loading) != 1 || !is.finite(loading) || loading < 0 || loading >= 1) {
    stop("loading must be a share of the premium, from 0 to below 1.", call. = FALSE)
  }
  check <- function(x, name, positive = FALSE) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0 || (positive && x == 0)) {
      stop(name, " must be ", if (positive) "above 0." else "at least 0.", call. = FALSE)
    }
  }
  for (name in names(programmes)) {
    p <- programmes[[name]]
    if (!is.list(p)) stop("Programme '", name, "' must be a list.", call. = FALSE)
    if (!is.null(p$quota_share)) {
      check(p$quota_share, paste0("The quota share of '", name, "'"))
      if (p$quota_share > 1) stop("The quota share of '", name, "' must be at most 1.", call. = FALSE)
    }
    if (!is.null(p$xl)) {
      check(p$xl$deductible, paste0("The deductible of '", name, "'"))
      check(p$xl$limit, paste0("The limit of '", name, "'"), positive = TRUE)
    }
    if (!is.null(p$stop_loss)) {
      check(p$stop_loss$attachment, paste0("The stop loss attachment of '", name, "'"))
      check(p$stop_loss$limit, paste0("The stop loss limit of '", name, "'"), positive = TRUE)
    }
  }
  frequency_id <- claims_distribution_id(frequency, freq_dist_options, "frequency")
  severity_id <- claims_distribution_id(severity, sev_dist_options, "severity")
  fp <- claims_distribution_params(frequency_params, freq_dist_options[[frequency_id]], "frequency_params")
  sp <- claims_distribution_params(severity_params, sev_dist_options[[severity_id]], "severity_params")

  if (!is.null(seed)) {
    global <- globalenv()
    saved <- if (exists(".Random.seed", envir = global, inherits = FALSE)) get(".Random.seed", envir = global)
    kind <- RNGkind()
    on.exit({
      suppressWarnings(RNGkind(kind[1], kind[2], kind[3]))
      if (is.null(saved)) {
        if (exists(".Random.seed", envir = global, inherits = FALSE)) rm(".Random.seed", envir = global)
      } else {
        assign(".Random.seed", saved, envir = global)
      }
    }, add = TRUE)
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  }
  # the claims of every year, once for all the programmes, drawn as the simulator draws them
  counts <- freq_dist_options[[frequency_id]]@simulate_func(n, fp)
  total <- sum(counts)
  sizes <- sev_dist_options[[severity_id]]@simulate_func(total, sp)
  year <- factor(rep.int(seq_len(n), counts), levels = seq_len(n))
  by_year <- function(x) {
    out <- tapply(x, year, sum)
    out[is.na(out)] <- 0
    as.numeric(out)
  }
  gross <- by_year(sizes)

  run <- function(p) {
    q <- if (is.null(p$quota_share)) 0 else p$quota_share
    retained_claims <- (1 - q) * sizes
    qs_premium <- q * premium
    commission <- if (is.null(p$commission)) 0 else p$commission * qs_premium
    xl_recovery <- rep(0, n)
    xl_premium <- 0
    reinstatement <- rep(0, n)
    if (!is.null(p$xl)) {
      x <- p$xl
      reinstatements <- if (is.null(x$reinstatements)) Inf else x$reinstatements
      rate <- if (is.null(x$reinstatement_rate)) 1 else x$reinstatement_rate
      per_claim <- pmin(pmax(retained_claims - x$deductible, 0), x$limit)
      xl_recovery <- pmin(by_year(per_claim), (reinstatements + 1) * x$limit)
      reinstated <- if (is.finite(reinstatements)) pmin(xl_recovery, reinstatements * x$limit) else xl_recovery
      xl_premium <- if (!is.null(x$premium)) x$premium else {
        # unlimited reinstatements are free: the premium is all upfront
        if (is.finite(reinstatements)) {
          technical_premium(mean(xl_recovery), x$limit, loading = loading, reinstatements = reinstatements,
                            reinstatement_rate = rate, used = mean(reinstated) / x$limit)$upfront_premium
        } else {
          mean(xl_recovery) / (1 - loading)
        }
      }
      if (rate > 0 && is.finite(reinstatements) && reinstatements > 0) reinstatement <- xl_premium * rate * reinstated / x$limit
    }
    retained <- by_year(retained_claims) - xl_recovery
    sl_recovery <- rep(0, n)
    sl_premium <- 0
    if (!is.null(p$stop_loss)) {
      s <- p$stop_loss
      sl_recovery <- pmin(pmax(retained - s$attachment, 0), s$limit)
      sl_premium <- if (!is.null(s$premium)) s$premium else mean(sl_recovery) / (1 - loading)
    }
    net_claims <- retained - sl_recovery
    paid <- qs_premium - commission + xl_premium + reinstatement + sl_premium
    list(result = premium - paid - net_claims, ceded = gross - net_claims, paid = paid)
  }
  all <- c(list(`No reinsurance` = list()), programmes)
  runs <- lapply(all, run)
  results <- vapply(runs, function(r) r$result, numeric(n))
  if (n == 1) results <- matrix(results, nrow = 1)
  colnames(results) <- names(all)
  capital_of <- function(x) mean(x) - stats::quantile(x, 1 - level, names = FALSE)
  capital <- vapply(runs, function(r) capital_of(r$result), numeric(1))
  ceded <- vapply(runs, function(r) mean(r$ceded), numeric(1))
  paid <- vapply(runs, function(r) mean(r$paid), numeric(1))
  relief <- capital[1] - capital
  summary <- data.frame(
    programme = names(all),
    expected_ceded = ceded,
    reinsurance_premium = paid,
    margin = paid - ceded,
    mean_result = colMeans(results),
    sd_result = apply(results, 2, stats::sd),
    p_loss = colMeans(results < 0),
    var_result = apply(results, 2, stats::quantile, probs = 1 - level, names = FALSE),
    capital = capital,
    capital_relief = relief,
    value = coc_rate * relief - (paid - ceded),
    stringsAsFactors = FALSE, row.names = NULL
  )
  structure(list(summary = summary, results = results, gross = gross, premium = premium, level = level, loading = loading,
                 coc_rate = coc_rate, n = n, programmes = programmes),
            class = "netsimr_programmes")
}

#' @export
print.netsimr_programmes <- function(x, ...) {
  s <- x$summary
  amount <- function(v) formatC(v, format = "f", digits = 0, big.mark = ",")
  cat("Reinsurance programmes, ", format(x$n, big.mark = ","), " simulated years, capital at ", format(100 * x$level), "%\n\n", sep = "")
  shown <- data.frame(programme = s$programme, ceded = amount(s$expected_ceded), margin = amount(s$margin),
                      mean_result = amount(s$mean_result), capital = amount(s$capital), relief = amount(s$capital_relief),
                      value = amount(s$value), stringsAsFactors = FALSE)
  print(shown, row.names = FALSE, right = TRUE)
  invisible(x)
}
